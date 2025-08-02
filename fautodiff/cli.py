"""Command line interface for the fautodiff generator."""
import argparse
import sys
from . import generator


def main():
    parser_arg = argparse.ArgumentParser(
        description="Generate automatic differentiation code"
    )
    parser_arg.add_argument("input", help="path to original Fortran file")
    parser_arg.add_argument(
        "-o",
        dest="output",
        help="path for generated Fortran file",
    )
    parser_arg.add_argument(
        "--no-warn",
        action="store_true",
        help="suppress warnings about unsupported derivatives",
    )
    parser_arg.add_argument(
        "-I",
        dest="search_dirs",
        action="append",
        default=[],
        help="add directory to .fadmod search path (may be repeated)",
    )
    parser_arg.add_argument(
        "-M",
        dest="fadmod_dir",
        default=None,
        help="directory for .fadmod files (default: current directory)",
    )
    parser_arg.add_argument(
        "--no-fadmod",
        action="store_true",
        help="do not write .fadmod information files",
    )
    parser_arg.add_argument(
        "--mode",
        choices=["reverse", "forward", "both"],
        default="both",
        help="AD mode to generate",
    )
    args = parser_arg.parse_args()

    try:
        code = generator.generate_ad(
            args.input,
            args.output,
            warn=not args.no_warn,
            search_dirs=args.search_dirs,
            write_fadmod=not args.no_fadmod,
            fadmod_dir=args.fadmod_dir,
            mode=args.mode,
        )
    except Exception as exc:
        raise

    if args.output is None:
        print(code, end="")


if __name__ == "__main__":
    main()
