-- Test data for Corvus VM management
-- Run with: psql -d corvus -f test_data.sql

-- Clear existing data (optional, comment out if you want to append)
TRUNCATE network_interface, drive, snapshot, disk_image, shared_dir, vm_ssh_key, ssh_key, vm RESTART IDENTITY CASCADE;

-- Insert VMs
INSERT INTO vm (id, name, created_at, status, cpu_count, ram_mb, description, pid, headless) VALUES
  (1, 'a10', now(), 'stopped', 4, 8192, 'AlmaLinux 10', NULL, false),
  (2, 'ws25', now(), 'stopped', 8, 8192, 'Windows Server 2025', NULL, false);
SELECT setval('vm_id_seq', (SELECT MAX(id) FROM vm));

-- Insert Disk Images
INSERT INTO disk_image (id, name, file_path, format, size_mb, created_at) VALUES
  (1, 'a10-system', 'a10/disk.qcow2', 'qcow2', 40960, now()),
  (2, 'almalinux-10-iso', '/data/images/AlmaLinux/AlmaLinux-10.0-x86_64-boot.iso', 'raw', NULL, now()),
  (3, 'ovmf-code', '/usr/share/edk2/OvmfX64/OVMF_CODE.fd', 'raw', NULL, now()),
  (4, 'a10-ovmf-vars', 'a10/OVMF_VARS.fd', 'raw', NULL, now()),
  (5, 'ws25-system', 'ws25/overlay.qcow2', 'qcow2', 102400, now()),
  (6, 'ws25-ovmf-vars', 'ws25/OVMF_VARS.fd', 'raw', NULL, now());
SELECT setval('disk_image_id_seq', (SELECT MAX(id) FROM disk_image));

-- Insert Drives
-- a10 (vm_id=1)
INSERT INTO drive (id, vm_id, disk_image_id, interface, media, read_only, cache_type, discard) VALUES
  (1, 1, 1, 'virtio', 'disk', false, 'writeback', true),
  (2, 1, 2, 'ide', 'cdrom', true, 'none', false),
  (3, 1, 3, 'pflash', NULL, true, 'none', false),
  (4, 1, 4, 'pflash', NULL, false, 'none', false);

-- ws25 (vm_id=2)
INSERT INTO drive (id, vm_id, disk_image_id, interface, media, read_only, cache_type, discard) VALUES
  (5, 2, 5, 'virtio', 'disk', false, 'writeback', true),
  (6, 2, 3, 'pflash', NULL, true, 'none', false),
  (7, 2, 6, 'pflash', NULL, false, 'none', false);
SELECT setval('drive_id_seq', (SELECT MAX(id) FROM drive));

-- Insert Network Interfaces
INSERT INTO network_interface (id, vm_id, interface_type, host_device, mac_address) VALUES
  (1, 1, 'vde', '/run/vde2/switch.ctl', '52:54:00:12:34:01'),
  (2, 2, 'vde', '/run/vde2/switch.ctl', '08:00:27:4b:3d:ac');
SELECT setval('network_interface_id_seq', (SELECT MAX(id) FROM network_interface));

-- Shared directories
INSERT INTO shared_dir (id, vm_id, path, tag, cache, read_only, pid) VALUES
  (1, 2, '/home/bobr/Dropbox', 'dropbox', 'auto', false, NULL);
SELECT setval('shared_dir_id_seq', (SELECT MAX(id) FROM shared_dir));

-- Insert some test snapshots (for the a10-system disk)
INSERT INTO snapshot (id, disk_image_id, name, created_at, size_mb) VALUES
  (1, 1, 'fresh-install', now() - interval '7 days', NULL),
  (2, 1, 'configured', now() - interval '3 days', NULL);
SELECT setval('snapshot_id_seq', (SELECT MAX(id) FROM snapshot));

-- Verify data
\echo 'VMs:'
SELECT id, name, status, cpu_count, ram_mb, pid FROM vm;

\echo 'Disk Images:'
SELECT id, name, format, size_mb, file_path FROM disk_image;

\echo 'Drives:'
SELECT d.id, v.name AS vm, di.name AS disk, d.interface, d.media, d.read_only
FROM drive d 
JOIN vm v ON d.vm_id = v.id
JOIN disk_image di ON d.disk_image_id = di.id;

\echo 'Network Interfaces:'
SELECT n.id, v.name AS vm, n.interface_type, n.host_device, n.mac_address 
FROM network_interface n JOIN vm v ON n.vm_id = v.id;

\echo 'Shared Directories:'
SELECT s.id, v.name AS vm, s.path, s.tag, s.cache, s.read_only 
FROM shared_dir s JOIN vm v ON s.vm_id = v.id;

\echo 'Snapshots:'
SELECT s.id, di.name AS disk, s.name, s.created_at
FROM snapshot s JOIN disk_image di ON s.disk_image_id = di.id;
