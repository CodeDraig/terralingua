from itertools import cycle

from core.utils.llm_client import AgentClient


class LLMRouter:
    def __init__(
        self,
        model: str,
        instances: int | None = None,
        provider: str = "anthropic",
        openai_base_url: str | None = None,
    ):
        if provider not in {"openai", "anthropic"}:
            raise ValueError(f"Unknown provider: {provider}")
        if not model.strip():
            raise ValueError("Model ID must not be empty")

        self.provider = provider
        self.model_name = model
        self.openai_base_url = openai_base_url
        self.refresh(instances)

    def refresh(self, instances=None):
        if instances is None:
            raise ValueError(
                f"Instances must be specified for model {self.model_name}"
            )
        self.clients = [self._build_client() for _ in range(instances)]
        self.cycle = cycle(self.clients)

    def _build_client(self):
        if self.provider == "openai":
            client_kwargs = {"provider": "openai"}
            if self.openai_base_url is not None:
                client_kwargs["base_url"] = self.openai_base_url
            llm_client = AgentClient(**client_kwargs)
            llm_request_params = {
                "model": self.model_name,
                "text": {"format": {"type": "json_object"}},
            }
        else:
            llm_client = AgentClient(provider="anthropic")
            llm_request_params = {
                "model": self.model_name,
                "max_output_tokens": 4096,
            }
        return llm_client, llm_request_params

    def next(self):
        return next(self.cycle)
