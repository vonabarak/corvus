.PHONY: web-build web-dev web-serve web-lint web-format web-clean desktop-run

# Frontend (Vite + React + TS) build + dev workflow. The SPA lives
# under /frontend; `make web-build` produces /frontend/dist and copies
# it to /python/corvus_web/static/ so the Python wheel ships it as
# package data. Everything is gated on `npm` being available — none
# of the Haskell/Python targets needs Node, but a release tarball or
# `make install` that wants the UI does.
NPM ?= npm
CORVUS_WEB ?= $(if $(wildcard .venv/bin/corvus-web),.venv/bin/corvus-web,corvus-web)

web-build:
	@command -v $(NPM) >/dev/null 2>&1 || { \
	  echo "error: '$(NPM)' not on PATH (install Node.js + npm to build the web UI)" >&2; \
	  exit 1; \
	}
	# `npm ci` is the reproducible install (lockfile is authoritative,
	# fails on drift) and is what CI / release builds want. It requires
	# a pre-existing package-lock.json *in sync* with package.json.
	# We pick the right mode automatically:
	#   * no lockfile yet         -> npm install (cold bootstrap)
	#   * package.json newer      -> npm install (lockfile is stale,
	#                                e.g. a dep was just added)
	#   * lockfile up-to-date     -> npm ci (reproducible)
	# Commit the resulting package-lock.json so CI gets the strict path.
	cd frontend && \
	  if [ ! -f package-lock.json ]; then \
	    echo "no package-lock.json — bootstrapping with 'npm install'"; \
	    $(NPM) install --no-audit --no-fund ; \
	  elif [ package.json -nt package-lock.json ]; then \
	    echo "package.json newer than lockfile — refreshing with 'npm install'"; \
	    $(NPM) install --no-audit --no-fund ; \
	  else \
	    $(NPM) ci --no-audit --no-fund ; \
	  fi
	cd frontend && $(NPM) run build
	rm -rf python/corvus_web/static
	mkdir -p python/corvus_web/static
	cp -r frontend/dist/. python/corvus_web/static/
	@echo
	@echo "Built web UI -> python/corvus_web/static/"

# Vite dev server. Proxies /api and /ws to the corvus-web gateway
# at 127.0.0.1:8080, so you need both running:
#   terminal 1:  make web-serve
#   terminal 2:  make web-dev
web-dev:
	cd frontend && $(NPM) run dev

# Run the corvus-web gateway against the local daemon (the same
# default Unix socket `crv` uses: $XDG_RUNTIME_DIR/corvus/corvus.sock).
# Pair with `make web-dev` in another terminal for the vite SPA dev
# server (HMR + /api and /ws proxied through to this gateway).
#
# No auto-reload: the asyncio + Cap'n Proto kj_loop integration in the
# FastAPI lifespan re-entry under uvicorn --reload is fragile, and the
# gateway is small enough that ctrl-C + re-run is fine. Tweak the
# daemon transport with FLAGS=, e.g.:
#   make web-serve FLAGS="--daemon-host 1.2.3.4 --no-daemon-tls"
web-serve:
	PYTHONPATH=python $(CORVUS_WEB) --log-level debug $(FLAGS)

web-lint:
	cd frontend && $(NPM) run lint
	cd frontend && $(NPM) run format:check
	cd frontend && $(NPM) run typecheck

web-format:
	cd frontend && $(NPM) run format

web-clean:
	rm -rf frontend/dist frontend/node_modules
	rm -rf python/corvus_web/static
	mkdir -p python/corvus_web/static
	touch python/corvus_web/static/.gitkeep

# Run the corvus-desktop GUI against the local daemon (same default
# Unix socket `crv` / `corvus-web` use: $XDG_RUNTIME_DIR/corvus/corvus.sock).
# Pair with a running daemon. Tweak the daemon transport with FLAGS=, e.g.:
#   make desktop-run FLAGS="--daemon-host 1.2.3.4 --no-daemon-tls"
#
# Prefers the project venv (corvus-desktop installed editable with
# the [desktop] extra: `pip install -e .[desktop]`); falls back to
# PATH for global installs.
CORVUS_DESKTOP ?= $(if $(wildcard .venv/bin/corvus-desktop),.venv/bin/corvus-desktop,corvus-desktop)

desktop-run:
	$(CORVUS_DESKTOP) --log-level debug $(FLAGS)
