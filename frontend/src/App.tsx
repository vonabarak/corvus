import { Route, Routes } from "react-router-dom";
import Dashboard from "./pages/Dashboard";
import VmList from "./pages/VmList";
import VmDetail from "./pages/VmDetail";
import Layout from "./components/Layout";

export default function App() {
  return (
    <Routes>
      <Route element={<Layout />}>
        <Route index element={<Dashboard />} />
        <Route path="vms" element={<VmList />} />
        <Route path="vms/:id" element={<VmDetail />} />
      </Route>
    </Routes>
  );
}
