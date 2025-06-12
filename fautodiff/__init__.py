"""Public API for fautodiff."""

from packaging.version import Version, parse
import fparser

if parse(getattr(fparser, "__version__", "0")) < Version("0.2.0"):
    raise RuntimeError("fautodiff requires fparser version 0.2.0 or later")

from . import parser
from .generator import generate_ad

__all__ = ["parser", "generate_ad"]
