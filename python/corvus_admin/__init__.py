"""corvus-admin — external PKI + deployment utility for Corvus.

Public surface is the CLI (see :mod:`corvus_admin.cli`); this
package's other modules expose smaller seams the tests poke at
directly: :mod:`corvus_admin.ca` for the CA / cert minting,
:mod:`corvus_admin.store` for the on-disk admin directory layout,
:mod:`corvus_admin.runner` for the local vs SSH abstraction, and
:mod:`corvus_admin.deploy` for the higher-level recipes.
"""

__all__ = ["__version__"]

__version__ = "0.1.0"
