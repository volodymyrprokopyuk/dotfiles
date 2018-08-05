CREATE TABLE IF NOT EXISTS family.person(
    id SERIAL NOT NULL,
    first_name TEXT NOT NULL,
    last_name TEXT NOT NULL,
    PRIMARY KEY (id)
);

CREATE OR REPLACE FUNCTION family.fn_find_person(
    ar_first_name TEXT,
    ar_last_name TEXT
) RETURNS SETOF family.person
AS $$
    SELECT p.id, p.first_name, p.last_name
    FROM family.person p
    WHERE p.first_name = ar_first_name AND p.last_name = ar_last_name;
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION family.fn_full_name(
    ar_first_name TEXT,
    ar_last_name TEXT
) RETURNS TEXT
AS $$
DECLARE
    full_name TEXT;
BEGIN
    full_name = ar_first_name || ' ' || ar_last_name;
    RETURN full_name;
END;
$$ LANGUAGE plpgsql;
