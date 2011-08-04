BEGIN;

SELECT _v.register_patch('20110804-roles', ARRAY['20110803-versioning'], NULL);

SET search_path = 'bookbrainz';

CREATE TABLE person_role (
    role_id SERIAL NOT NULL PRIMARY KEY,
    name TEXT NOT NULL
);

CREATE TABLE edition_person_role (
    person INT NOT NULL REFERENCES bookbrainz_v.person (version),
    edition INT NOT NULL REFERENCES bookbrainz_v.edition (version),
    role_id INT NOT NULL REFERENCES bookbrainz.person_role (role_id)
);

CREATE TABLE book_person_role (
    person INT NOT NULL REFERENCES bookbrainz_v.person (version),
    book INT NOT NULL REFERENCES bookbrainz_v.book (version),
    role_id INT NOT NULL REFERENCES bookbrainz.person_role (role_id)
);

INSERT INTO person_role (role_id, name)
    VALUES (1, 'Author'), (2, 'Foreword'), (3, 'Illustrator'), (4, 'Translator');
ALTER SEQUENCE person_role_role_id_seq RESTART 3;

INSERT INTO edition_person_role (person, edition, role_id)
   SELECT author_credit_person.person, edition.version, 1
   FROM bookbrainz_v.edition
   JOIN author_credit_person ON author_credit_person.author_credit = edition.author;

INSERT INTO edition_person_role (person, edition, role_id)
   SELECT author_credit_person.person, edition.version, 3
   FROM bookbrainz_v.edition
   JOIN author_credit_person ON author_credit_person.author_credit = edition.illustrator;

INSERT INTO edition_person_role (person, edition, role_id)
   SELECT author_credit_person.person, edition.version, 4
   FROM bookbrainz_v.edition
   JOIN author_credit_person ON author_credit_person.author_credit = edition.translator;

ALTER TABLE bookbrainz_v.edition DROP COLUMN author CASCADE;
ALTER TABLE bookbrainz_v.edition DROP COLUMN translator;
ALTER TABLE bookbrainz_v.edition DROP COLUMN illustrator;

INSERT INTO book_person_role (person, book, role_id)
   SELECT author_credit_person.person, book.version, 4
   FROM bookbrainz_v.book
   JOIN author_credit_person ON author_credit_person.author_credit = book.author_credit;

ALTER TABLE bookbrainz_v.book DROP COLUMN author_credit CASCADE;

CREATE VIEW bookbrainz.edition AS
SELECT edition.gid, name, book, year, publisher, country_iso_code,
       language_iso_code, isbn, barcode, edition_index, format
FROM bookbrainz_v.branch
JOIN bookbrainz_v.edition_revision USING (rev_id)
JOIN bookbrainz_v.edition USING (version)
WHERE branch.master = TRUE;

CREATE VIEW bookbrainz.book AS
SELECT book.gid, name
FROM bookbrainz_v.branch
JOIN bookbrainz_v.book_revision USING (rev_id)
JOIN bookbrainz_v.book USING (version)
WHERE branch.master = TRUE;

COMMIT;
