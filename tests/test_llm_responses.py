import os
import unittest
from types import SimpleNamespace
from unittest import mock

from core.experiment import llm_router
from core.utils.llm_client import AgentClient, LLMClient, ResponseRejectedError


class _FakeResponses:
    def __init__(self, response):
        self.response = response
        self.requests = []

    def create(self, **kwargs):
        self.requests.append(kwargs)
        return self.response


class _FakeSequenceResponses:
    def __init__(self, responses):
        self.responses = iter(responses)

    def create(self, **_kwargs):
        return next(self.responses)


class _FakeOpenAI:
    def __init__(self, response):
        self.responses = _FakeResponses(response)


class _FakeAnthropicMessages:
    def __init__(self):
        self.requests = []

    def create(self, **kwargs):
        self.requests.append(kwargs)
        return SimpleNamespace(
            content=[SimpleNamespace(text="anthropic result")],
            usage=SimpleNamespace(input_tokens=5, output_tokens=3),
        )


class _FakeAnthropic:
    def __init__(self):
        self.messages = _FakeAnthropicMessages()


def _response(text="result", input_tokens=11, output_tokens=7, status="completed"):
    return SimpleNamespace(
        output_text=text,
        status=status,
        error=None,
        incomplete_details=None,
        usage=SimpleNamespace(
            input_tokens=input_tokens,
            output_tokens=output_tokens,
        ),
    )


class ResponsesTransportTests(unittest.TestCase):
    def test_analysis_client_uses_responses_request_and_usage(self):
        sdk = _FakeOpenAI(_response("  answer  "))
        client = LLMClient(client="openai")
        client._openai_client = sdk
        messages = [
            {"role": "system", "content": "system rules"},
            {"role": "user", "content": "question"},
            {"role": "assistant", "content": "bad JSON"},
            {"role": "user", "content": "retry as JSON"},
        ]
        request_parameters = {
            "text": {"format": {"type": "json_object"}},
            "reasoning": {"effort": "low"},
            "max_output_tokens": 200,
        }

        text, input_tokens, output_tokens = client._call_openai(
            "gpt-test", messages, request_parameters
        )

        self.assertEqual(text, "answer")
        self.assertEqual((input_tokens, output_tokens), (11, 7))
        self.assertEqual(
            sdk.responses.requests,
            [
                {
                    "model": "gpt-test",
                    "instructions": "system rules",
                    "input": messages[1:],
                    "store": False,
                    **request_parameters,
                }
            ],
        )

    def test_agent_client_uses_responses_and_handles_missing_usage(self):
        response = _response("agent action")
        response.usage = None
        sdk = _FakeOpenAI(response)
        client = object.__new__(AgentClient)
        client.provider = "openai"
        client._client = sdk

        result = client.get_response(
            messages=[
                {"role": "system", "content": "be an agent"},
                {"role": "user", "content": "act"},
            ],
            request_params={"model": "gpt-test"},
        )

        self.assertEqual(result.content, "agent action")
        self.assertEqual(result.input_tokens, 0)
        self.assertEqual(result.output_tokens, 0)
        self.assertEqual(sdk.responses.requests[0]["store"], False)
        self.assertEqual(sdk.responses.requests[0]["instructions"], "be an agent")
        self.assertEqual(
            sdk.responses.requests[0]["input"],
            [{"role": "user", "content": "act"}],
        )

    def test_analysis_client_parses_responses_structured_output(self):
        sdk = _FakeOpenAI(_response('{"accepted": true}'))
        client = LLMClient(client="openai")
        client._openai_client = sdk

        result = client.get_response(
            model="gpt-test",
            messages=[{"role": "user", "content": "classify"}],
            request_parameters={
                "text": {"format": {"type": "json_object"}},
            },
            max_retries=1,
        )

        self.assertEqual(result.content, {"accepted": True})
        self.assertEqual((result.input_tokens, result.output_tokens), (11, 7))

    def test_incomplete_or_empty_response_is_rejected(self):
        for response in (
            _response("", status="completed"),
            _response("partial", status="incomplete"),
        ):
            sdk = _FakeOpenAI(response)
            client = object.__new__(AgentClient)
            client.provider = "openai"
            client._client = sdk

            with self.subTest(status=response.status):
                with self.assertRaises(ValueError):
                    client.get_response(
                        messages=[{"role": "user", "content": "act"}],
                        request_params={"model": "gpt-test"},
                    )

    def test_c2_rejected_response_retains_reported_usage(self):
        sdk = _FakeOpenAI(
            _response(
                "partial",
                input_tokens=13,
                output_tokens=8,
                status="incomplete",
            )
        )
        client = object.__new__(AgentClient)
        client.provider = "openai"
        client._client = sdk

        with self.assertRaises(ResponseRejectedError) as raised:
            client.get_response(
                messages=[{"role": "user", "content": "act"}],
                request_params={"model": "gpt-test"},
            )

        self.assertEqual(raised.exception.input_tokens, 13)
        self.assertEqual(raised.exception.output_tokens, 8)

    def test_c2_analysis_retry_includes_rejected_response_usage(self):
        sdk = SimpleNamespace(
            responses=_FakeSequenceResponses(
                [
                    _response(
                        "partial",
                        input_tokens=13,
                        output_tokens=8,
                        status="incomplete",
                    ),
                    _response("complete", input_tokens=11, output_tokens=7),
                ]
            )
        )
        client = LLMClient(client="openai")
        client._openai_client = sdk

        result = client.get_response(
            model="gpt-test",
            messages=[{"role": "user", "content": "analyze"}],
            max_retries=2,
            output_json=False,
        )

        self.assertEqual(result.content, "complete")
        self.assertEqual(result.input_tokens, 24)
        self.assertEqual(result.output_tokens, 15)

    def test_custom_base_url_still_uses_responses_transport(self):
        sdk = _FakeOpenAI(_response())
        with mock.patch("core.utils.llm_client.OpenAI", return_value=sdk) as factory:
            client = AgentClient(
                provider="openai",
                base_url="http://127.0.0.1:9000/v1",
                api_key="EMPTY",
            )
            client.get_response(
                messages=[{"role": "user", "content": "act"}],
                request_params={"model": "local-model"},
            )

        factory.assert_called_once_with(
            base_url="http://127.0.0.1:9000/v1", api_key="EMPTY"
        )
        self.assertEqual(sdk.responses.requests[0]["model"], "local-model")

    def test_caller_cannot_enable_server_side_response_storage(self):
        sdk = _FakeOpenAI(_response())
        client = object.__new__(AgentClient)
        client.provider = "openai"
        client._client = sdk

        client.get_response(
            messages=[{"role": "user", "content": "act"}],
            request_params={"model": "gpt-test", "store": True},
        )

        self.assertIs(sdk.responses.requests[0]["store"], False)

    def test_anthropic_adapter_translates_neutral_response_parameters(self):
        sdk = _FakeAnthropic()
        client = object.__new__(AgentClient)
        client.provider = "anthropic"
        client._client = sdk

        result = client.get_response(
            messages=[
                {"role": "system", "content": "system rules"},
                {"role": "user", "content": "question"},
            ],
            request_params={
                "model": "claude-test",
                "max_output_tokens": 321,
                "temperature": 0.5,
                "text": {"format": {"type": "json_object"}},
                "reasoning": {"effort": "low"},
                "store": False,
            },
        )

        self.assertEqual(result.content, "anthropic result")
        self.assertEqual(
            sdk.messages.requests,
            [
                {
                    "model": "claude-test",
                    "messages": [{"role": "user", "content": "question"}],
                    "max_tokens": 321,
                    "system": "system rules",
                    "temperature": 0.5,
                }
            ],
        )

    @unittest.skipUnless(
        os.environ.get("RUN_OPENAI_E2E") == "1",
        "set RUN_OPENAI_E2E=1 to make a paid Responses API request",
    )
    def test_live_openai_responses_smoke(self):
        client = AgentClient(provider="openai")

        result = client.get_response(
            messages=[
                {"role": "system", "content": "Reply with exactly OK."},
                {"role": "user", "content": "Confirm connectivity."},
            ],
            request_params={
                "model": "gpt-5-mini",
                "max_output_tokens": 128,
                "reasoning": {"effort": "low"},
            },
        )

        self.assertTrue(str(result.content).strip())
        self.assertGreater(result.input_tokens, 0)
        self.assertGreater(result.output_tokens, 0)


class ResponsesRouterTests(unittest.TestCase):
    def test_openai_router_uses_responses_parameter_shapes(self):
        router = object.__new__(llm_router.LLMRouter)
        router.model_name = "gpt-5.1"

        with mock.patch.object(
            llm_router, "AgentClient", return_value=mock.sentinel.client
        ):
            client, params = router._build_remote_client()

        self.assertIs(client, mock.sentinel.client)
        self.assertEqual(
            params,
            {
                "model": "gpt-5.1",
                "text": {"format": {"type": "json_object"}},
                "reasoning": {"effort": "low"},
            },
        )

    def test_openai_nano_models_use_low_reasoning_and_json_output(self):
        for model_name in ("gpt-5-nano", "gpt-5.4-nano"):
            with self.subTest(model=model_name):
                router = object.__new__(llm_router.LLMRouter)
                router.model_name = model_name

                with mock.patch.object(
                    llm_router, "AgentClient", return_value=mock.sentinel.client
                ):
                    client, params = router._build_remote_client()

                self.assertIs(client, mock.sentinel.client)
                self.assertEqual(
                    params,
                    {
                        "model": model_name,
                        "text": {"format": {"type": "json_object"}},
                        "reasoning": {"effort": "low"},
                    },
                )

    def test_local_router_uses_responses_output_limit_and_format(self):
        router = object.__new__(llm_router.LLMRouter)
        router.model_name = llm_router.MODEL_MAP["QWEN3"]

        with mock.patch.object(
            llm_router, "AgentClient", return_value=mock.sentinel.client
        ):
            client, params = router._build_local_client(9000)

        self.assertIs(client, mock.sentinel.client)
        self.assertEqual(
            params,
            {
                "model": "Qwen/Qwen3-32B",
                "text": {"format": {"type": "json_object"}},
                "temperature": 1,
                "max_output_tokens": 256,
            },
        )


if __name__ == "__main__":
    unittest.main()
