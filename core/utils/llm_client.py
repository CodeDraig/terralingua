import ast
import json
import os
import re
from dataclasses import dataclass
from typing import Any, Dict, List, Optional

from anthropic import Anthropic
from dotenv import load_dotenv
from openai import BadRequestError, OpenAI

load_dotenv()


_RESPONSES_ONLY_PARAMS = {"text", "reasoning", "store"}


class ResponseRejectedError(ValueError):
    """A Responses result that cannot satisfy the caller's text contract."""

    def __init__(
        self, message: str, *, input_tokens: int = 0, output_tokens: int = 0
    ):
        super().__init__(message)
        self.input_tokens = input_tokens
        self.output_tokens = output_tokens


def _split_responses_input(messages: list[dict]) -> tuple[str | None, list[dict]]:
    """Split system guidance from ordered Responses input messages."""
    system_messages = []
    input_messages = []
    for message in messages:
        if message["role"] == "system":
            system_messages.append(str(message["content"]))
        else:
            input_messages.append(message.copy())

    instructions = "\n\n".join(system_messages) if system_messages else None
    return instructions, input_messages


def _build_responses_request(messages: list[dict], request_parameters: dict) -> dict:
    """Build a stateless Responses request without mutating caller data."""
    instructions, input_messages = _split_responses_input(messages)
    request = request_parameters.copy()
    request["store"] = False
    request["input"] = input_messages
    if instructions is not None:
        request["instructions"] = instructions
    return request


def _read_responses_result(response: Any) -> tuple[str, int, int]:
    """Normalize one completed Responses result to TerraLingua's text contract."""
    usage = getattr(response, "usage", None)
    input_tokens = getattr(usage, "input_tokens", 0) if usage is not None else 0
    output_tokens = getattr(usage, "output_tokens", 0) if usage is not None else 0

    status = getattr(response, "status", None)
    if status != "completed":
        detail = getattr(response, "error", None) or getattr(
            response, "incomplete_details", None
        )
        raise ResponseRejectedError(
            f"OpenAI Responses request ended with status {status!r}: {detail}",
            input_tokens=input_tokens,
            output_tokens=output_tokens,
        )

    text = getattr(response, "output_text", None)
    if not isinstance(text, str) or not text.strip():
        raise ResponseRejectedError(
            "OpenAI Responses request returned no text output.",
            input_tokens=input_tokens,
            output_tokens=output_tokens,
        )

    return text.strip(), input_tokens, output_tokens


@dataclass
class Response:
    content: str | None | Dict
    input_tokens: int
    output_tokens: int


class LLMClient:
    """
    Generic LLM API client supporting OpenAI and Anthropic providers.

    Usage:
        client = LLMClient()
        response, tokens = client.get_response(
            provider="openai",
            model="gpt-4",
            messages=[{"role": "system", "content": "..."}, ...],
            request_parameters={"temperature": 0.7},
            token_counter={"input": 0, "output": 0}
        )
    """

    def __init__(self, client="anthropic", long_context: bool = False):
        self.long_context = long_context
        self._openai_client: Optional[OpenAI] = None
        self._anthropic_client: Optional[Anthropic] = None

        if client == "openai":
            self._call_client = self._call_openai
        elif client == "anthropic":
            self._call_client = self._call_anthropic
        else:
            raise ValueError(f"Unsupported client: {client}")

    def _get_openai_client(self) -> OpenAI:
        """Get or create OpenAI client."""
        if self._openai_client is None:
            self._openai_client = OpenAI(api_key=os.environ.get("OPENAI_API_KEY"))
        return self._openai_client

    def _get_anthropic_client(self) -> Anthropic:
        """Get or create Anthropic client."""
        if self._anthropic_client is None:
            if self.long_context:
                default_headers = {"anthropic-beta": "context-1m-2025-08-07"}
            else:
                default_headers = None
            self._anthropic_client = Anthropic(
                api_key=os.environ.get("ANTHROPIC_API_KEY"),
                default_headers=default_headers,
            )
        return self._anthropic_client

    def _remove_thinking_tags(self, text: str) -> str:
        """
        Remove thinking tags from model response if present.

        Args:
            text: Raw response text

        Returns:
            Text with thinking tags removed
        """
        if "</think>" in text:
            return text.split("</think>")[1]
        return text

    def _extract_json(
        self, text: str, has_structured_output: bool
    ) -> tuple[str | None, str | None]:
        """
        Extract JSON from response text.

        Args:
            text: Response text
            has_structured_output: Whether a Responses text format was requested

        Returns:
            Tuple of (json_string, error_message)
        """
        if has_structured_output:
            # Model should return pure JSON
            return text, None

        # Try to extract JSON from markdown code blocks
        json_match = re.search(r"```json\s*(\{.*?\})\s*```", text, re.DOTALL)
        if json_match:
            return json_match.group(1), None

        # Try to find JSON without code blocks
        json_match = re.search(r"(\{.*\})", text, re.DOTALL)
        if json_match:
            return json_match.group(1), None

        return None, "Error: No valid JSON found in response"

    def _call_openai(
        self, model: str, messages: list[dict], request_parameters: dict
    ) -> tuple[str, int, int]:
        """
        Call OpenAI API.

        Returns:
            Tuple of (response_text, input_tokens, output_tokens)
        """
        self._openai_client = self._get_openai_client()
        request = _build_responses_request(
            messages, {**request_parameters, "model": model}
        )
        response = self._openai_client.responses.create(**request)
        return _read_responses_result(response)

    def _call_anthropic(
        self, model: str, messages: list[dict], request_parameters: dict
    ) -> tuple[str, int, int]:
        """
        Call Anthropic API.

        Note: Anthropic API has different message format - system message is separate.

        Returns:
            Tuple of (response_text, input_tokens, output_tokens)
        """
        self._anthropic_client = self._get_anthropic_client()
        # Extract system message if present
        system_message = None
        anthropic_messages = []

        for msg in messages:
            if msg["role"] == "system":
                system_message = msg["content"]
            else:
                anthropic_messages.append(msg)

        # Prepare kwargs
        kwargs = {
            "model": model,
            "messages": anthropic_messages,
            "max_tokens": request_parameters.get("max_output_tokens", 10000),
        }

        # Add system message if present
        if system_message:
            kwargs["system"] = system_message

        # Add parameters supported by Anthropic's Messages API.
        for key, value in request_parameters.items():
            if key not in _RESPONSES_ONLY_PARAMS and key != "max_output_tokens":
                kwargs[key] = value

        resp = self._anthropic_client.messages.create(**kwargs)

        # Extract text from response
        if len(resp.content) == 0:
            error = "Response has no content."
            if resp.stop_reason == "refusal":
                error = "Model refused to complete the request."
            raise ValueError(error)

        text = resp.content[0].text
        input_tokens = resp.usage.input_tokens
        output_tokens = resp.usage.output_tokens

        return text, input_tokens, output_tokens

    def get_response(
        self,
        model: str,
        messages: list[dict],
        request_parameters: dict | None = None,
        max_retries: int = 10,
        enable_error_reprompting: bool = True,
        track_tokens: bool = True,
        output_json: bool = True,
    ) -> Response:
        """
        Make an LLM API call with retry logic and JSON parsing.

        Args:
            model: Model name (e.g., "gpt-4", "claude-sonnet-4-5")
            messages: List of message dicts with "role" and "content"
            request_parameters: Provider-neutral generation parameters using the
                OpenAI Responses shape where the providers differ
            max_retries: Maximum number of retry attempts
            enable_error_reprompting: If True, append error messages to retry
            track_tokens: If True, track token usage
            output_json: If True, parse response as JSON; otherwise return raw text

        Returns:
            Response containing parsed JSON or raw text plus accumulated usage

        Raises:
            BadRequestError: Re-raised if caller wants to handle token overflow (OpenAI only)
            Exception: If max retries exhausted or other unhandled errors
        """
        if request_parameters is None:
            request_parameters = {}

        token_counter = {"input": 0, "output": 0}

        # Create a working copy of messages to allow retry modifications
        messages_copy = list(messages)

        text_format = request_parameters.get("text", {}).get("format", {})
        has_structured_output = text_format.get("type") not in (None, "text")

        for trial in range(max_retries):
            try:
                # Make API call based on provider
                text, input_tokens, output_tokens = self._call_client(
                    model, messages_copy, request_parameters
                )

                # Track tokens
                if track_tokens:
                    token_counter["input"] += input_tokens
                    token_counter["output"] += output_tokens

                # Remove thinking tags if present
                text = self._remove_thinking_tags(text)

                # If not expecting JSON, return raw text
                if not output_json:
                    return Response(
                        content=text,
                        input_tokens=token_counter["input"],
                        output_tokens=token_counter["output"],
                    )

                try:
                    parsed_json = json.loads(text)
                    return Response(
                        content=parsed_json,
                        input_tokens=token_counter["input"],
                        output_tokens=token_counter["output"],
                    )
                except json.JSONDecodeError:
                    pass  # Fall through to retry logic below

                # Parse JSON response
                json_str, error_msg = self._extract_json(text, has_structured_output)

                if json_str:
                    try:
                        parsed_json = json.loads(json_str)
                        return Response(
                            content=parsed_json,
                            input_tokens=token_counter["input"],
                            output_tokens=token_counter["output"],
                        )
                    except json.JSONDecodeError:
                        try:
                            parsed_json = ast.literal_eval(json_str)
                            return Response(
                                content=parsed_json,
                                input_tokens=token_counter["input"],
                                output_tokens=token_counter["output"],
                            )
                        except (ValueError, SyntaxError) as e:
                            error_msg = f"JSON parsing error: {str(e)} \n Response was: {json_str}"
                    except Exception as e:
                        error_msg = f"Unexpected error parsing JSON: {str(e)} \n Response was: {json_str}"

                # If we get here, JSON parsing failed
                if not enable_error_reprompting:
                    raise ValueError(error_msg)

                # Append error message and retry
                messages_copy.extend(
                    [
                        {"role": "assistant", "content": text},
                        {
                            "role": "user",
                            "content": f"{error_msg}\nPlease provide a valid JSON response.",
                        },
                    ]
                )

            except ResponseRejectedError as e:
                if track_tokens:
                    token_counter["input"] += e.input_tokens
                    token_counter["output"] += e.output_tokens
                if trial == max_retries - 1:
                    raise Exception(
                        f"Failed after {max_retries} retries. Last error: {str(e)}"
                    )
            except BadRequestError as e:
                # Re-raise BadRequestError for caller to handle (e.g., token reduction)
                raise e
            except Exception as e:
                if trial == max_retries - 1:
                    raise Exception(
                        f"Failed after {max_retries} retries. Last error: {str(e)}"
                    )
                # Continue to next retry

        raise Exception(f"Failed to get valid response after {max_retries} retries")


class AgentClient:
    """
    Client used for the LLM agents in the environment.
    This is very simple as all the parsing etc is done outside by the agent itself.
    Supports Responses-compatible OpenAI endpoints and the Anthropic API.
    """

    def __init__(self, provider: str = "openai", **kwargs):
        self.provider = provider
        self._client: Any
        if provider == "openai":
            self._client = OpenAI(**kwargs)
        elif provider == "anthropic":
            self._client = Anthropic(
                api_key=os.environ.get("ANTHROPIC_API_KEY"), **kwargs
            )
        else:
            raise ValueError(f"Unsupported provider: {provider}")

    def get_response(
        self, messages: List[Dict[str, str]], request_params: Dict
    ) -> Response:
        if self.provider == "openai":
            return self._get_response_openai(messages, request_params)
        else:
            return self._get_response_anthropic(messages, request_params)

    def _get_response_openai(
        self, messages: List[Dict[str, str]], request_params: Dict
    ) -> Response:
        request = _build_responses_request(messages, request_params)
        response = self._client.responses.create(**request)
        text, input_tokens, output_tokens = _read_responses_result(response)
        return Response(
            content=text,
            input_tokens=input_tokens,
            output_tokens=output_tokens,
        )

    def _get_response_anthropic(
        self, messages: List[Dict[str, str]], request_params: Dict
    ) -> Response:
        # Anthropic takes system as a top-level param, not inside messages
        system_message = None
        anthropic_messages = []
        for msg in messages:
            if msg["role"] == "system":
                system_message = msg["content"]
            else:
                anthropic_messages.append(msg)

        kwargs = {
            "model": request_params.get("model"),
            "messages": anthropic_messages,
            "max_tokens": request_params.get("max_output_tokens", 4096),
        }
        if system_message:
            kwargs["system"] = system_message

        # Pass through parameters supported by Anthropic's Messages API.
        for key, value in request_params.items():
            if key not in _RESPONSES_ONLY_PARAMS and key != "max_output_tokens":
                kwargs[key] = value

        response = self._client.messages.create(**kwargs)

        if not response.content:
            raise ValueError("Empty response from Anthropic API")

        return Response(
            content=response.content[0].text,
            input_tokens=response.usage.input_tokens,
            output_tokens=response.usage.output_tokens,
        )
