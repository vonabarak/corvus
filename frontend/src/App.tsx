import { Route, Routes } from "react-router-dom";
import Dashboard from "./pages/Dashboard";
import VmList from "./pages/VmList";
import VmDetail from "./pages/VmDetail";
import DiskList from "./pages/DiskList";
import DiskDetail from "./pages/DiskDetail";
import DiskCreate from "./pages/DiskCreate";
import NetworkList from "./pages/NetworkList";
import NetworkDetail from "./pages/NetworkDetail";
import NetworkCreate from "./pages/NetworkCreate";
import SshKeyList from "./pages/SshKeyList";
import SshKeyDetail from "./pages/SshKeyDetail";
import TemplateList from "./pages/TemplateList";
import TemplateDetail from "./pages/TemplateDetail";
import TaskList from "./pages/TaskList";
import TaskDetail from "./pages/TaskDetail";
import Apply from "./pages/Apply";
import Console from "./pages/Console";
import VmCreate from "./pages/VmCreate";
import NodeList from "./pages/NodeList";
import NodeDetail from "./pages/NodeDetail";
import Layout from "./components/Layout";

export default function App() {
  return (
    <Routes>
      <Route element={<Layout />}>
        <Route index element={<Dashboard />} />
        <Route path="vms" element={<VmList />} />
        <Route path="vms/new" element={<VmCreate />} />
        <Route path="vms/:id" element={<VmDetail />} />
        <Route path="vms/:id/console" element={<Console />} />
        <Route path="disks" element={<DiskList />} />
        <Route path="disks/new" element={<DiskCreate />} />
        <Route path="disks/:id" element={<DiskDetail />} />
        <Route path="networks" element={<NetworkList />} />
        <Route path="networks/new" element={<NetworkCreate />} />
        <Route path="networks/:id" element={<NetworkDetail />} />
        <Route path="ssh-keys" element={<SshKeyList />} />
        <Route path="ssh-keys/:id" element={<SshKeyDetail />} />
        <Route path="templates" element={<TemplateList />} />
        <Route path="templates/:id" element={<TemplateDetail />} />
        <Route path="tasks" element={<TaskList />} />
        <Route path="tasks/:id" element={<TaskDetail />} />
        <Route path="nodes" element={<NodeList />} />
        <Route path="nodes/:id" element={<NodeDetail />} />
        <Route path="apply" element={<Apply />} />
      </Route>
    </Routes>
  );
}
