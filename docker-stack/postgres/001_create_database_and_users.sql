CREATE DATABASE people;
\connect people

CREATE SCHEMA family;

CREATE USER family_ddl_dml WITH ENCRYPTED PASSWORD 'Password1!';
GRANT USAGE, CREATE ON SCHEMA family TO family_ddl_dml;
