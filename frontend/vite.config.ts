import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import path from "node:path";

// Dev server: the SPA runs at :5173 (vite default) and proxies
// /api and /ws to the local corvus-web gateway, so the React side
// never knows whether it's being served standalone or by the gateway.
// In production the same FastAPI process serves the build output
// at the SPA's origin — same-origin fetches, no CORS dance.
export default defineConfig({
  plugins: [react()],
  resolve: {
    alias: {
      "@": path.resolve(__dirname, "./src"),
    },
  },
  // `npm run build` writes here; the Makefile's `web-build` target
  // mirrors it into python/corvus_web/static/ so the wheel ships it.
  build: {
    outDir: "dist",
    emptyOutDir: true,
    sourcemap: true,
  },
  server: {
    port: 5173,
    proxy: {
      "/api": {
        target: "http://127.0.0.1:8080",
        changeOrigin: false,
      },
      "/ws": {
        target: "ws://127.0.0.1:8080",
        ws: true,
        changeOrigin: false,
      },
    },
  },
});
