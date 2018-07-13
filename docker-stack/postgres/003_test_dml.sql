INSERT INTO family.person(first_name, last_name)
VALUES ('Volodymyr', 'Prokopyuk');

SELECT * FROM family.person

-- EXECUTE family.sp_find_person 'Volodymyr', 'Prokopyuk'
-- go

-- SELECT family.fn_full_name(p.first_name, p.last_name) full_name FROM family.person p
-- go
