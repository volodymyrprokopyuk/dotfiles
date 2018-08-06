CREATE TABLE IF NOT EXISTS person(
    person_id INT NOT NULL auto_increment,
    first_name TEXT NOT NULL,
    last_name TEXT NOT NULL,
    PRIMARY KEY (person_id)
);

DELIMITER //

CREATE OR REPLACE PROCEDURE sp_find_person(
    ar_first_name TEXT,
    ar_last_name TEXT
) BEGIN
    SELECT p.person_id, p.first_name, p.last_name
    FROM person p
    WHERE p.first_name=ar_first_name AND p.last_name=ar_last_name;
END //

CREATE OR REPLACE FUNCTION fn_full_name(
    ar_first_name TEXT,
    ar_last_name TEXT
) RETURNS TEXT
BEGIN
    DECLARE full_name TEXT;
    SELECT CONCAT(ar_first_name, ' ', ar_last_name) INTO full_name;
    RETURN full_name;
END //

DELIMITER ;
