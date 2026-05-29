import { Link, NavLink, Outlet } from "react-router-dom";
import { cn } from "@/lib/utils";

const navItemClass = ({ isActive }: { isActive: boolean }) =>
  cn(
    "transition-colors hover:text-foreground",
    isActive ? "font-medium text-foreground" : "text-muted-foreground",
  );

export default function Layout() {
  return (
    <div className="flex min-h-screen flex-col">
      <header className="border-b border-border bg-card">
        <div className="container flex h-14 items-center gap-6">
          <Link to="/" className="text-lg font-semibold tracking-tight">
            Corvus
          </Link>
          <nav className="flex items-center gap-4 text-sm">
            <NavLink to="/" end className={navItemClass}>
              Dashboard
            </NavLink>
            <NavLink to="/vms" className={navItemClass}>
              VMs
            </NavLink>
            <NavLink to="/disks" className={navItemClass}>
              Disks
            </NavLink>
            <NavLink to="/networks" className={navItemClass}>
              Networks
            </NavLink>
            <NavLink to="/templates" className={navItemClass}>
              Templates
            </NavLink>
            <NavLink to="/ssh-keys" className={navItemClass}>
              SSH keys
            </NavLink>
          </nav>
        </div>
      </header>
      <main className="container flex-1 py-8">
        <Outlet />
      </main>
    </div>
  );
}
