CREATE TABLE family.person(
    id INT NOT NULL IDENTITY,
    first_name NVARCHAR(100) NOT NULL,
    last_name NVARCHAR(100) NOT NULL,
    PRIMARY KEY (id)
)
go

CREATE OR ALTER PROCEDURE family.sp_find_person
    @first_name NVARCHAR(100),
    @last_name NVARCHAR(100)
AS BEGIN
    SELECT p.id, p.first_name, p.last_name
    FROM family.person p
    WHERE p.first_name = @first_name AND p.last_name = @last_name
END
go

CREATE OR ALTER FUNCTION family.fn_full_name(
    @first_name NVARCHAR(100),
    @last_name NVARCHAR(100)
) RETURNS NVARCHAR(400)
AS BEGIN
    DECLARE @full_name NVARCHAR(400)
    SET @full_name = @first_name + ' ' + @last_name
    RETURN @full_name
END
go
