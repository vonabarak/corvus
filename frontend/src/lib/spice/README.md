# spice-html5 (vendored)

Verbatim copy of `src/` from <https://gitlab.freedesktop.org/spice/spice-html5>,
plus `COPYING` and `COPYING.LESSER`.

Why vendored: the `spice-html5` npm package was unpublished in 2019 and the
upstream project ships only as a folder of ES modules — no build, no
package manifest. Vendoring keeps the dependency closed-loop with the
rest of `frontend/` and avoids a network fetch at build time.

License: **LGPL-3.0-or-later**. We use the library by `import` only and
do not modify it. To stay within LGPL §4 (combined works), keep this
directory intact: any future patches go into siblings, not edits to
these files.

To refresh:

```sh
git clone --depth 1 https://gitlab.freedesktop.org/spice/spice-html5.git /tmp/spice-html5
cp -r /tmp/spice-html5/src/. frontend/src/lib/spice/
cp /tmp/spice-html5/COPYING* frontend/src/lib/spice/
```

Entry point: `main.js` exports `SpiceMainConn`. See the upstream
`spice_auto.html` for a worked example.
