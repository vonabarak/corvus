/*
 * CPython extension shell for the Corvus client. Delegates to Haskell
 * (see ffi-src/Corvus/Python.hs); this file owns the module ABI:
 * PyInit__corvus, method table, argument parsing, GIL handoff, and
 * PyCapsule integration for persistent connections.
 */

#define PY_SSIZE_T_CLEAN
#define Py_LIMITED_API 0x030A0000
#include <Python.h>

#include <pthread.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include <HsFFI.h>
#include "python_ext.h"

/* -------------------------------------------------------------------------- */
/* RTS lifecycle                                                              */
/* -------------------------------------------------------------------------- */

static pthread_once_t rts_once = PTHREAD_ONCE_INIT;
static int rts_initialized = 0;

static void do_rts_init(void) {
    static char arg0[] = "corvus_client";
    static char *argv_storage[] = { arg0, NULL };
    char **argv = argv_storage;
    int argc = 1;
    hs_init(&argc, &argv);
    corvusInit();
    rts_initialized = 1;
}

static void ensure_rts(void) {
    pthread_once(&rts_once, do_rts_init);
}

/* Name used to tag our PyCapsule so PyCapsule_GetPointer validates. */
static const char *CAPSULE_NAME = "corvus_client.connection";

/* -------------------------------------------------------------------------- */
/* One-shot call                                                              */
/* -------------------------------------------------------------------------- */

static PyObject *py_call(PyObject *self, PyObject *args) {
    (void)self;
    const char *in_buf;
    Py_ssize_t in_len;
    if (!PyArg_ParseTuple(args, "y#", &in_buf, &in_len)) {
        return NULL;
    }
    ensure_rts();

    uint8_t *out_buf = NULL;
    size_t out_len = 0;
    int rc;
    Py_BEGIN_ALLOW_THREADS
    rc = corvusCall((uint8_t *)in_buf, (size_t)in_len, &out_buf, &out_len);
    Py_END_ALLOW_THREADS

    if (rc != 0) {
        PyErr_SetString(PyExc_RuntimeError,
                        "corvus_client: internal error crossing FFI boundary");
        return NULL;
    }

    PyObject *result = PyBytes_FromStringAndSize((const char *)out_buf,
                                                 (Py_ssize_t)out_len);
    corvusFreeBuffer(out_buf);
    return result;
}

/* -------------------------------------------------------------------------- */
/* Persistent connection                                                      */
/* -------------------------------------------------------------------------- */

/* Free the Haskell-side handle when the PyCapsule is garbage-collected. */
static void capsule_destructor(PyObject *capsule) {
    void *handle = PyCapsule_GetPointer(capsule, CAPSULE_NAME);
    if (handle) {
        corvusClose(handle);
    }
}

/*
 * open(transport_bytes: bytes) -> PyCapsule | bytes
 *
 * Returns a capsule on success or the raw JSON error bytes on failure.
 * We return bytes instead of raising so the Python wrapper can do
 * kind→exception mapping in one place (from_err).
 */
static PyObject *py_open(PyObject *self, PyObject *args) {
    (void)self;
    const char *in_buf;
    Py_ssize_t in_len;
    if (!PyArg_ParseTuple(args, "y#", &in_buf, &in_len)) {
        return NULL;
    }
    ensure_rts();

    void *handle = NULL;
    uint8_t *err_buf = NULL;
    size_t err_len = 0;
    int rc;
    Py_BEGIN_ALLOW_THREADS
    rc = corvusOpen((uint8_t *)in_buf, (size_t)in_len,
                    &handle, &err_buf, &err_len);
    Py_END_ALLOW_THREADS

    if (rc == 0) {
        return PyCapsule_New(handle, CAPSULE_NAME, capsule_destructor);
    }
    if (rc == 2 && err_buf) {
        PyObject *bytes = PyBytes_FromStringAndSize((const char *)err_buf,
                                                    (Py_ssize_t)err_len);
        corvusFreeBuffer(err_buf);
        return bytes;
    }
    PyErr_SetString(PyExc_RuntimeError,
                    "corvus_client: internal error opening connection");
    return NULL;
}

/* call_open(capsule, request_bytes) -> bytes */
static PyObject *py_call_open(PyObject *self, PyObject *args) {
    (void)self;
    PyObject *capsule;
    const char *in_buf;
    Py_ssize_t in_len;
    if (!PyArg_ParseTuple(args, "Oy#", &capsule, &in_buf, &in_len)) {
        return NULL;
    }
    void *handle = PyCapsule_GetPointer(capsule, CAPSULE_NAME);
    if (!handle) {
        /* PyCapsule_GetPointer sets a TypeError already. */
        return NULL;
    }
    ensure_rts();

    uint8_t *out_buf = NULL;
    size_t out_len = 0;
    int rc;
    Py_BEGIN_ALLOW_THREADS
    rc = corvusCallOpen(handle, (uint8_t *)in_buf, (size_t)in_len,
                        &out_buf, &out_len);
    Py_END_ALLOW_THREADS

    if (rc != 0) {
        PyErr_SetString(PyExc_RuntimeError,
                        "corvus_client: internal error crossing FFI boundary");
        return NULL;
    }
    PyObject *result = PyBytes_FromStringAndSize((const char *)out_buf,
                                                 (Py_ssize_t)out_len);
    corvusFreeBuffer(out_buf);
    return result;
}

/* close(capsule) -> None. Usually unnecessary (destructor handles it),
 * but exposed so callers can explicitly release sockets early. */
static PyObject *py_close_capsule(PyObject *self, PyObject *args) {
    (void)self;
    PyObject *capsule;
    if (!PyArg_ParseTuple(args, "O", &capsule)) {
        return NULL;
    }
    void *handle = PyCapsule_GetPointer(capsule, CAPSULE_NAME);
    if (handle) {
        /* Clear the destructor before freeing so the capsule GC doesn't
         * try to corvusClose a stale handle. PyCapsule_SetPointer can't
         * be used here because it rejects NULL. */
        PyCapsule_SetDestructor(capsule, NULL);
        corvusClose(handle);
    } else {
        /* No-op if already closed; PyCapsule_GetPointer set an error. */
        PyErr_Clear();
    }
    Py_RETURN_NONE;
}

/* -------------------------------------------------------------------------- */
/* Shutdown                                                                   */
/* -------------------------------------------------------------------------- */

static PyObject *py_shutdown(PyObject *self, PyObject *args) {
    (void)self;
    (void)args;
    if (rts_initialized) {
        corvusShutdown();
        hs_exit();
        rts_initialized = 0;
    }
    Py_RETURN_NONE;
}

/* -------------------------------------------------------------------------- */
/* Module                                                                     */
/* -------------------------------------------------------------------------- */

static PyMethodDef methods[] = {
    {"call",      py_call,      METH_VARARGS,
     "call(request: bytes) -> bytes\n\n"
     "One-shot RPC: opens a connection for one request and closes it."},
    {"open",      py_open,      METH_VARARGS,
     "open(transport: bytes) -> PyCapsule | bytes\n\n"
     "Open a persistent connection. Returns a capsule on success or the\n"
     "JSON error bytes on failure (so the Python side can map kinds)."},
    {"call_open", py_call_open, METH_VARARGS,
     "call_open(capsule, request: bytes) -> bytes\n\n"
     "Issue an RPC on a persistent connection."},
    {"close",     py_close_capsule, METH_VARARGS,
     "close(capsule) -> None\n\nExplicitly close a connection now."},
    {"_shutdown", py_shutdown,  METH_NOARGS,
     "_shutdown() -> None\n\nInternal: tear down the Haskell RTS. atexit hook."},
    {NULL, NULL, 0, NULL}
};

static struct PyModuleDef module_def = {
    PyModuleDef_HEAD_INIT,
    "_corvus",
    "Corvus native client extension (Haskell-backed).",
    -1,
    methods,
    NULL, NULL, NULL, NULL
};

PyMODINIT_FUNC PyInit__corvus(void) {
    PyObject *m = PyModule_Create(&module_def);
    if (!m) return NULL;
    ensure_rts();
    return m;
}
