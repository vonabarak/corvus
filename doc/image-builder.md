# Python image builds

The reusable package lives in `python/corvus_image_builder` and is included in
`pip install -e .`. It has no CLI or main function.

Each flavor owns a script under `yaml/gentoo-python`. The headless script
constructs Settings, subclasses PackageInstallStage to select packages, and
orders stages in an ImageRecipe. Extend or replace stage instances in Python.
BuildManager runs the recipe and owns cleanup, including interruption handling.
Portage Python APIs perform queries; emerge performs installations.

Run `make -C yaml/gentoo-python plan` for an unprivileged preview or
`make -C yaml/gentoo-python build` to build and publish the image.
The bundle helper packages the selected script as __main__.py alongside only
the common library. The builder runs `python3 /run/glibc-systemd-headless.pyz`.
The VM supplies Portage and native build tools; no pip installation is required.

Scripts use Python defaults with IMAGE_ROOT, TARGET_DISK, IMAGE_CACHE,
KERNEL_CONFIG, USEPKG and GETBINPKG environment overrides. IMAGE_BUILD_PLAN=1
prints the stage order without accessing disks or requiring Gentoo.
PORTAGE_KEYSERVER retains the repository keyserver override.

Execution formats the target disk inside a disposable builder VM. Failures
preserve mounted filesystems for diagnosis and release the shared cache lock.
The pipeline remains separate from top-level image builds and uses the existing
registered builder dependency. BIOS and UEFI runtime templates are preserved.
