-- Test data for Corvus VM management
-- Run with: psql -d corvus -f test_data.sql

-- Clear existing data (optional, comment out if you want to append)
TRUNCATE network_interface, drive, vm RESTART IDENTITY CASCADE;

-- Insert VMs
-- pid is NULL for all VMs initially (they are not actually running)
INSERT INTO vm (name, created_at, status, cpu_count, ram_mb, description, pid) VALUES
  ('almalinux-10', now(), 'stopped', 4, 8192, 'AlmaLinux 10', NULL),
  ('ws25', now(), 'stopped', 8, 8192, 'Windows Server 2025', NULL);

-- Insert Drives
-- almalinux-10 (id=1)
INSERT INTO drive (vm_id, interface, file_path, format, media, read_only, cache_type, discard) VALUES
  (1, 'virtio', 'a10/disk.qcow2', 'qcow2', 'disk', false, 'writeback', true),
  (1, 'ide', '/data/images/AlmaLinux/AlmaLinux-10.0-x86_64-boot.iso', 'raw', 'cdrom', true, 'none', false),
  (1, 'pflash', '/usr/share/edk2/OvmfX64/OVMF_CODE.fd', 'raw', NULL, true, 'none', false),
  (1, 'pflash', 'a10/OVMF_VARS.fd', 'raw', NULL, false, 'none', false);

-- ws25 (id=2)
INSERT INTO drive (vm_id, interface, file_path, format, media, read_only, cache_type, discard) VALUES
  (2, 'virtio', 'ws25/overlay.qcow2', 'qcow2', 'disk', false, 'writeback', true),
  (2, 'pflash', '/usr/share/edk2/OvmfX64/OVMF_CODE.fd', 'raw', NULL, true, 'none', false),
  (2, 'pflash', 'ws25/OVMF_VARS.fd', 'raw', NULL, false, 'none', false);

-- Insert Network Interfaces
-- almalinux-10 (id=1)
INSERT INTO network_interface (vm_id, interface_type, host_device, mac_address) VALUES
  (1, 'vde', '/run/vde2/switch.ctl', '52:54:00:12:34:01');

-- ws25 (id=2)
INSERT INTO network_interface (vm_id, interface_type, host_device, mac_address) VALUES
  (2, 'vde', '/run/vde2/switch.ctl', '08:00:27:4b:3d:ac');


INSERT INTO shared_dir (vm_id, path, tag, cache, read_only) VALUES
  (2, '/home/bobr/Public', 'public', 'auto', false);

-- Verify data
\echo 'VMs:'
SELECT id, name, status, cpu_count, ram_mb, pid FROM vm;

\echo 'Drives:'
SELECT d.id, v.name AS vm, d.interface, d.format, d.media, d.file_path 
FROM drive d JOIN vm v ON d.vm_id = v.id;

\echo 'Network Interfaces:'
SELECT n.id, v.name AS vm, n.interface_type, n.host_device, n.mac_address 
FROM network_interface n JOIN vm v ON n.vm_id = v.id;

\echo 'Shared Directories:'
SELECT s.id, v.name AS vm, s.path, s.tag, s.cache, s.read_only 
FROM shared_dir s JOIN vm v ON s.vm_id = v.id;