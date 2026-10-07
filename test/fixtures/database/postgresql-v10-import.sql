-- Frozen version-10 tables used by the import identity migration.
CREATE TABLE schema_version (id INTEGER PRIMARY KEY, version INTEGER NOT NULL);
INSERT INTO schema_version VALUES (1, 10);
CREATE TABLE disk_image (id BIGSERIAL PRIMARY KEY, name VARCHAR NOT NULL, format VARCHAR NOT NULL, size BIGINT, created_at TIMESTAMP NOT NULL, backing_image_id BIGINT REFERENCES disk_image(id) ON DELETE RESTRICT ON UPDATE RESTRICT, ephemeral BOOLEAN NOT NULL DEFAULT false);
INSERT INTO disk_image (name, format, size, created_at, ephemeral) VALUES ('historical', 'raw', 1024, '2026-09-01 00:00:00', false);
