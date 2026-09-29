"""Typed surface of xdist's untyped loadscope scheduler used by our subclass.

Keep this small contract aligned with the installed xdist implementation when
upgrading pytest-xdist. Runtime imports still resolve to the real xdist classes.
"""

from collections import OrderedDict
from collections.abc import Callable

import pytest

class WorkerController:
    shutting_down: bool
    def shutdown(self) -> None: ...

class LoadScopeScheduling:
    config: pytest.Config
    log: Callable[[str], None]
    collection: list[str] | None
    workqueue: OrderedDict[str, dict[str, bool]]
    assigned_work: dict[WorkerController, dict[str, dict[str, bool]]]
    registered_collections: dict[WorkerController, list[str]]
    def __init__(self, config: pytest.Config, log: object) -> None: ...
    @property
    def nodes(self) -> list[WorkerController]: ...
    @property
    def collection_is_completed(self) -> bool: ...
    def _pending_of(self, workload: dict[str, dict[str, bool]]) -> int: ...
    def _assign_work_unit(self, node: WorkerController) -> None: ...
    def _check_nodes_have_same_collection(self) -> bool: ...
    def _split_scope(self, nodeid: str) -> str: ...
