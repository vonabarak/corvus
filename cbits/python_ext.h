/*
 * Prototypes for the Haskell-exported entry points used by python_ext.c.
 * Signatures must stay in sync with `foreign export ccall` declarations
 * in ffi-src/Corvus/Python.hs.
 */
#ifndef CORVUS_PYTHON_EXT_H
#define CORVUS_PYTHON_EXT_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

void corvusInit(void);
void corvusShutdown(void);

/*
 * One-shot call (opens a connection per call).
 *   0 = response buffer produced (contains ok or err JSON)
 *   nonzero = internal failure; out_buf_ptr not filled
 */
int corvusCall(uint8_t *in_buf,
               size_t   in_len,
               uint8_t **out_buf_ptr,
               size_t   *out_len);

/*
 * Persistent connection: open / call / close.
 *
 * corvusOpen: 0 = out_handle filled; 2 = connection failed (err_buf
 * contains JSON error); other = internal failure.
 * corvusCallOpen: 0 = response produced; nonzero = internal failure.
 * corvusClose: no-op if handle is NULL.
 */
int  corvusOpen(uint8_t *transport_buf,
                size_t   transport_len,
                void    **out_handle,
                uint8_t **out_err_buf,
                size_t   *out_err_len);

int  corvusCallOpen(void    *handle,
                    uint8_t *in_buf,
                    size_t   in_len,
                    uint8_t **out_buf_ptr,
                    size_t   *out_len);

void corvusClose(void *handle);

void corvusFreeBuffer(uint8_t *buf);

#ifdef __cplusplus
}
#endif

#endif
