CREATE DATABASE people;
go
USE people;
go
-- LOGIN > server / instance, authentication principal
CREATE LOGIN family WITH PASSWORD = 'Password!1';
go
-- USER > database, authorization permissions
CREATE USER family FOR LOGIN family;
go
EXEC sp_addrolemember 'db_owner', 'family';
go
-- EXEC sp_addrolemember 'db_ddladmin', 'family';
-- go
-- EXEC sp_addrolemember 'db_datareader', 'family';
-- go
-- EXEC sp_addrolemember 'db_datawriter', 'family';
-- go
