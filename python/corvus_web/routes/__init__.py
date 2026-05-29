"""FastAPI route modules for corvus-web.

Each subsystem (system/vms/disks/...) has its own ``APIRouter`` in a
separate submodule. :mod:`corvus_web.app` mounts them all under
``/api/`` in :func:`corvus_web.app.create_app`.
"""
