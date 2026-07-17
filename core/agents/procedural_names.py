"""Deterministic, neutral display names for simulated agents."""

from functools import lru_cache
from hashlib import blake2s


_STARTS = (
    "ae", "ari", "ava", "bel", "bri", "ca", "cor", "da",
    "dre", "eli", "ena", "fae", "gali", "hara", "ily", "ira",
    "jora", "ka", "keli", "lio", "luma", "mara", "mire", "navi",
    "nera", "oli", "orin", "pela", "ravi", "sela", "tavi", "vela",
)

_ENDS = (
    "dor", "len", "lin", "mar", "mir", "na", "nel", "nor",
    "ra", "ran", "rel", "ren", "ria", "rin", "ris", "ron",
    "sa", "sen", "sor", "ta", "tan", "tel", "ther", "tin",
    "va", "ven", "via", "vin", "wen", "ya", "yon", "zar",
)

_BASE_NAMES = tuple(
    f"{start}{end}".capitalize() for start in _STARTS for end in _ENDS
)

if len(_BASE_NAMES) != len(set(_BASE_NAMES)):
    raise RuntimeError("Procedural name fragments must produce unique base names")


@lru_cache(maxsize=None)
def _ordered_names(seed: int) -> tuple[str, ...]:
    """Return a stable seed-specific permutation without global RNG state."""

    def order_key(name: str) -> bytes:
        return blake2s(f"{seed}:{name}".encode(), digest_size=16).digest()

    return tuple(sorted(_BASE_NAMES, key=order_key))


def procedural_name(seed: int, index: int) -> str:
    """Return the unique display name at ``index`` for ``seed``."""
    if index < 0:
        raise ValueError("Name index must be non-negative")

    ordered = _ordered_names(int(seed))
    cycle, offset = divmod(index, len(ordered))
    base = ordered[offset]
    return base if cycle == 0 else f"{base}{cycle + 1}"


def procedural_names(seed: int, count: int) -> list[str]:
    """Return ``count`` deterministic, unique display names."""
    if count < 0:
        raise ValueError("Name count must be non-negative")
    return [procedural_name(seed, index) for index in range(count)]
