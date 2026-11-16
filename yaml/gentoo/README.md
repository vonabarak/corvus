# Gentoo images

Each recipe directory contains its YAML pipeline, Python build entry points,
and private assets. Shared kernel configuration lives in `common/kernel`.
The library lives in `python/corvus_image_builder`.

Use `make build IMAGE=glibc-systemd-headless` from this directory, or
`make image IMAGE=gentoo-glibc-systemd-headless` from the repository root.
Use `make plan IMAGE=musl-openrc-headless` to inspect both stage sequences.
Headless recipes use separate `build-base.py` and `build-cloudinit.py` scripts;
other build recipes use `build.py`. Bundles are generated beside their sources.

`build` refreshes dependencies with the selected image policy; `ensure` only
fills missing outputs. The upstream cloud image is owned by its import recipe.
No build recipe imports another recipe's source image.

Guest runtime shell hooks remain assets, not image-building orchestration.
See [builder architecture](../../doc/image-builder.md) and
[rescue operations](rescue/README.md).
