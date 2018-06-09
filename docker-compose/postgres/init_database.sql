CREATE DATABASE people;

CREATE USER family WITH ENCRYPTED PASSWORD 'family';

GRANT CONNECT ON DATABASE people TO family;

\connect people

CREATE SCHEMA family;

GRANT USAGE ON SCHEMA family TO family;

ALTER DEFAULT PRIVILEGES IN SCHEMA family GRANT ALL PRIVILEGES ON TABLES TO family;
ALTER DEFAULT PRIVILEGES IN SCHEMA family GRANT ALL PRIVILEGES ON SEQUENCES TO family;

CREATE TABLE IF NOT EXISTS family.person(
    id SERIAL NOT NULL,
    first_name TEXT NOT NULL,
    last_name TEXT NOT NULL,
    PRIMARY KEY (id)
);

INSERT INTO family.person(first_name, last_name) VALUES ('Volodymyr', 'Prokopyuk');
