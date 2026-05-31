import React from "react";
import ReactDOM from "react-dom/client";
import { BrowserRouter } from "react-router-dom";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { Toaster } from "sonner";
import App from "./App";
import "./index.css";

// Single QueryClient per app. Cache stays cheap because lists/detail
// fetches are small dicts; we re-fetch aggressively on focus/reconnect
// since the underlying daemon state can change out from under us when
// someone uses `crv` in another window.
const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      refetchOnWindowFocus: true,
      retry: 1,
      staleTime: 0,
    },
  },
});

const root = document.getElementById("root");
if (!root) throw new Error("missing #root element in index.html");

ReactDOM.createRoot(root).render(
  <React.StrictMode>
    <QueryClientProvider client={queryClient}>
      <BrowserRouter>
        <App />
        {/* Single Toaster mounted at the root drives every `toast(...)`
            call site in the app. Top-right keeps it out of the way of
            the page chrome and the action buttons. `richColors` opts
            in to sonner's accent styling so `toast.error` actually
            looks like an error without per-call styling. */}
        <Toaster position="top-right" richColors closeButton />
      </BrowserRouter>
    </QueryClientProvider>
  </React.StrictMode>,
);
