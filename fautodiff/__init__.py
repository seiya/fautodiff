"""Public API for fautodiff."""

import importlib
from typing import Any

import fparser  # type: ignore[import-untyped]
from packaging.version import Version, parse

if parse(getattr(fparser, "__version__", "0")) < Version("0.2.0"):
    raise RuntimeError("fautodiff requires fparser version 0.2.0 or later")

__all__ = ["parser", "generate_ad"]


def __getattr__(name: str) -> Any:
    """Dynamically import exported members."""

    if name == "parser":
        return importlib.import_module(".parser", __name__)
    if name == "generate_ad":
        return importlib.import_module(".generator", __name__).generate_ad
    raise AttributeError(name)
