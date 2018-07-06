CREATE DATABASE people
go

USE people
go

CREATE SCHEMA family
go

CREATE OR ALTER PROCEDURE dbo.sp_create_login
    @login_name NVARCHAR(50),
    @login_password NVARCHAR(50)
AS BEGIN
    IF NOT EXISTS (SELECT sp.name FROM sys.server_principals sp WHERE sp.name = @login_name)    BEGIN
        DECLARE @create_login_sql NVARCHAR(200)
        SET @create_login_sql = 'CREATE LOGIN ' + quotename(@login_name) + ' WITH PASSWORD = ''' + @login_password + ''''
        EXECUTE sp_executesql @create_login_sql
    END
END
go

CREATE OR ALTER PROCEDURE dbo.sp_create_user_for_login
    @user_name NVARCHAR(50),
    @user_password NVARCHAR(50),
    @login_name NVARCHAR(50) = @user_name
AS BEGIN
    IF NOT EXISTS (SELECT dp.name FROM sys.database_principals dp WHERE dp.name = @user_name) BEGIN
        DECLARE @create_user_sql NVARCHAR(200)
        SET @create_user_sql = 'CREATE USER ' + quotename(@user_name) + ' FOR LOGIN ' + @login_name
        EXECUTE sp_executesql @create_user_sql
    END
END
go

CREATE OR ALTER PROCEDURE dbo.sp_grant_execute_on_schema_to_user
    @schema_name NVARCHAR(50),
    @user_name NVARCHAR(50)
AS BEGIN
    DECLARE @grant_execute_sql NVARCHAR(200)
    SET @grant_execute_sql = 'GRANT EXECUTE ON SCHEMA ::' + @schema_name + ' TO ' + @user_name
    EXECUTE sp_executesql @grant_execute_sql
END
go

CREATE OR ALTER PROCEDURE dbo.sp_create_ddl_user
    @user_name NVARCHAR(50),
    @user_password NVARCHAR(50)
AS BEGIN
    EXECUTE dbo.sp_create_login @user_name, @user_password
    EXECUTE dbo.sp_create_user_for_login @user_name, @user_password
    EXECUTE sp_addrolemember 'db_ddladmin', @user_name
END
go

CREATE OR ALTER PROCEDURE dbo.sp_create_dml_user
    @user_name NVARCHAR(50),
    @user_password NVARCHAR(50)
AS BEGIN
    EXECUTE dbo.sp_create_login @user_name, @user_password
    EXECUTE dbo.sp_create_user_for_login @user_name, @user_password
    EXECUTE sp_addrolemember 'db_datareader', @user_name
    EXECUTE sp_addrolemember 'db_datawriter', @user_name
    EXECUTE dbo.sp_grant_execute_on_schema_to_user 'family', @user_name
END
go

EXECUTE dbo.sp_create_ddl_user 'family_ddl', 'Password1!'
go

EXECUTE dbo.sp_create_dml_user 'family_dml', 'Password1!'
go
