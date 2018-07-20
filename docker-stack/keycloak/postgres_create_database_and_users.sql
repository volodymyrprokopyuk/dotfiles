CREATE DATABASE keycloak;
\connect keycloak

CREATE USER keycloak WITH ENCRYPTED PASSWORD 'Password1!';
GRANT ALL PRIVILEGES ON DATABASE keycloak TO keycloak;
