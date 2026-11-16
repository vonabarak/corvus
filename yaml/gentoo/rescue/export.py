"""Export rescue installation media and its offline update bundle."""

import argparse
from pathlib import Path

from corvus_image_builder.gentoo.export import export_rescue_image

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("source", type=Path)
    parser.add_argument("output", type=Path)
    arguments = parser.parse_args()
    export_rescue_image(arguments.source, arguments.output)
