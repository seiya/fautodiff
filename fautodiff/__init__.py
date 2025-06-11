"""Public API for fautodiff."""

from . import parser
from .generator import generate_ad

__all__ = ["parser", "generate_ad"]
