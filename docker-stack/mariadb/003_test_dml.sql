INSERT INTO person(first_name, last_name)
VALUES ('Volodymyr', 'Prokopyuk');

SELECT * FROM person;

CALL sp_find_person('Volodymyr', 'Prokopyuk');

SELECT fn_full_name(p.first_name, p.last_name) full_name FROM person p;
