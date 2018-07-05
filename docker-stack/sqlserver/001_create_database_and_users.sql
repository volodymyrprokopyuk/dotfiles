CREATE DATABASE people
go

USE people
go

CREATE SCHEMA family
go

CREATE OR ALTER PROCEDURE dbo.sp_create_family_ddl_user
AS BEGIN
    CREATE LOGIN family_ddl WITH PASSWORD = 'Password1!'
    CREATE USER family_ddl FOR LOGIN family_ddl
    EXECUTE sp_addrolemember 'db_ddladmin', 'family_ddl'
END
go

CREATE OR ALTER PROCEDURE dbo.sp_create_family_dml_user
AS BEGIN
    CREATE LOGIN family_dml WITH PASSWORD = 'Password1!'
    CREATE USER family_dml FOR LOGIN family_dml
    EXECUTE sp_addrolemember 'db_datareader', 'family_dml'
    EXECUTE sp_addrolemember 'db_datawriter', 'family_dml'
    GRANT EXECUTE ON SCHEMA ::family TO family_dml
END
go

EXECUTE dbo.sp_create_family_ddl_user
go

EXECUTE dbo.sp_create_family_dml_user
go
