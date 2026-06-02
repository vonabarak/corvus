"""corvus-desktop — native PySide6 GUI for the corvus daemon.

Talks Cap'n Proto RPC directly via ``corvus_client.AsyncClient`` on a
dedicated worker thread; the Qt GUI thread never touches pycapnp
objects. See ``client_bridge.CorvusBridge`` for the threading model.

Public entry point: ``corvus_desktop.cli:main`` (also exposed as the
``corvus-desktop`` console script when installed with the ``[desktop]``
extra: ``pip install 'corvus[desktop]'``).
"""

from __future__ import annotations

__version__ = "0.1.0"
