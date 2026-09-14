-- Portable sample data for the frozen version-2 schema. Let each backend
-- allocate IDs so its sequences remain valid when the daemon writes later.
INSERT INTO node (name, host, node_agent_port, net_agent_port, base_path, admin_state, created_at) VALUES ('migration-node', '127.0.0.2', 9878, 9877, '/tmp/migration-node', 'maintenance', '2026-09-01 00:00:00');
INSERT INTO vm (name, node_id, created_at, status, cpu_count, ram_mb) VALUES ('migration-vm', 1, '2026-09-01 00:00:00', 'stopped', 1, 128);
INSERT INTO disk_image (name, format, size_mb, created_at) VALUES ('migration-disk', 'raw', 1, '2026-09-01 00:00:00');
INSERT INTO disk_image_node (disk_image_id, node_id, file_path) VALUES (1, 1, '/tmp/migration-disk.raw');
INSERT INTO drive (vm_id, disk_image_id, interface, media, cache_type) VALUES (1, 1, 'ide', 'cdrom', 'none');
CREATE INDEX migration_drive_media ON drive(media);
