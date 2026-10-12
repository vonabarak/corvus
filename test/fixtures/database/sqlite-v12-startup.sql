-- Frozen version-12 table excerpt for the startup timeout migration.
CREATE TABLE schema_version (id BIGINT PRIMARY KEY, version BIGINT NOT NULL);
INSERT INTO schema_version VALUES (1, 12);
CREATE TABLE vm (id BIGINT PRIMARY KEY, name VARCHAR NOT NULL, cpu_count BIGINT NOT NULL, ram BIGINT NOT NULL, guest_agent BOOLEAN NOT NULL DEFAULT false);
INSERT INTO vm VALUES (1, 'historical-vm', 1, 268435456, true);
CREATE TABLE template_vm (id BIGINT PRIMARY KEY, name VARCHAR NOT NULL, cpu_count BIGINT NOT NULL, ram BIGINT NOT NULL, guest_agent BOOLEAN NOT NULL DEFAULT false);
INSERT INTO template_vm VALUES (1, 'historical-template', 1, 268435456, true);
