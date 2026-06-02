"""Modal ``QDialog`` subclasses for create / edit / attach flows.

Each dialog wraps a small ``QFormLayout`` with a Save / Cancel button
row provided by :class:`form_dialog.FormDialog`. Result payloads are
plain ``dict[str, object]`` so callers can hand them straight to the
matching bridge slot.
"""
