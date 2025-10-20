"""Command line interface for the fautodiff generator."""

import argparse
import sys
from pathlib import Path

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
        help="add directory to .fadmod search path before the current directory (may be repeated)",
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
    parser_arg.add_argument(
        "--disable-directives",
        dest="disable_directives",
        action="store_true",
        help="treat !$FAD directives as ordinary comments",
    )
    parser_arg.add_argument(
        "--disable-scatter-to-gather",
        action="store_true",
        help="do not rewrite OpenMP scatter stores into gather operations",
    )
    parser_arg.add_argument(
        "--emit-validation-driver",
        nargs="?",
        const=None,
        default=False,
        metavar="FILENAME",
        help=(
            "generate a validation driver template alongside the AD output; "
            "optionally provide a filename for the emitted driver"
        ),
    )
    args = parser_arg.parse_args()

    search_dirs = args.search_dirs if args.search_dirs else []
    if "." not in search_dirs:
        search_dirs.append(".")

    emit_validation_arg = args.emit_validation_driver
    emit_validation = emit_validation_arg is not False
    validation_driver_name = None
    if emit_validation:
        if args.mode != "both":
            parser_arg.error(
                "--emit-validation-driver requires --mode=both"
            )
        if emit_validation_arg not in (False, None):
            validation_driver_name = emit_validation_arg

    try:
        src_text = Path(args.input).read_text()
        code = generator.generate_ad(
            src_text,
            args.input,
            args.output,
            warn=not args.no_warn,
            search_dirs=search_dirs,
            write_fadmod=not args.no_fadmod,
            fadmod_dir=args.fadmod_dir,
            mode=args.mode,
            disable_directives=args.disable_directives,
            disable_scatter_to_gather=args.disable_scatter_to_gather,
            emit_validation=emit_validation,
            validation_driver_name=validation_driver_name,
        )
    except Exception as exc:
        raise

    if args.output is None:
        print(code, end="")


if __name__ == "__main__":
    main()
