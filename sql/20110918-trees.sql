BEGIN;

SELECT
  _v.register_patch('20110918-trees', ARRAY['20110917-general-references'], NULL);

--------------------------------------------------------------------------------
CREATE TABLE bookbrainz_v.book_tree (
    book_tree_id SERIAL NOT NULL PRIMARY KEY,
    version INTEGER NOT NULL REFERENCES bookbrainz_v.book_v (version)
);

INSERT INTO bookbrainz_v.book_tree (book_tree_id, version)
SELECT book_id, version FROM bookbrainz.book;

ALTER TABLE bookbrainz_v.book_revision
ADD COLUMN book_tree_id INTEGER REFERENCES bookbrainz_v.book_tree (book_tree_id);

UPDATE bookbrainz_v.book_revision SET book_tree_id = version;

ALTER TABLE bookbrainz_v.book_revision ALTER COLUMN book_tree_id SET NOT NULL;
ALTER TABLE bookbrainz_v.book_revision DROP COLUMN version CASCADE;

ALTER TABLE bookbrainz.book_person_role
ADD COLUMN book_tree_id INTEGER REFERENCES bookbrainz_v.book_tree (book_tree_id);

UPDATE bookbrainz.book_person_role SET book_tree_id = book_revision.book_tree_id
FROM bookbrainz_v.book_revision
WHERE book_revision.rev_id = book_person_role.rev_id;

ALTER TABLE bookbrainz.book_person_role
ALTER COLUMN book_tree_id SET NOT NULL;

ALTER TABLE bookbrainz.book_person_role DROP COLUMN rev_id;

CREATE OR REPLACE VIEW bookbrainz.book AS
SELECT book_id, gid, name, version, rev_id
FROM bookbrainz_v.branch
JOIN bookbrainz_v.book_branch ON branch_id = branch.id
JOIN bookbrainz_v.book_revision USING (rev_id)
JOIN bookbrainz_v.book USING (book_id)
JOIN bookbrainz_v.book_gid USING (book_id)
JOIN bookbrainz_v.book_tree USING (book_tree_id)
JOIN bookbrainz_v.book_v USING (version)
WHERE branch.master = TRUE;

--------------------------------------------------------------------------------
CREATE TABLE bookbrainz_v.edition_tree (
    edition_tree_id SERIAL NOT NULL PRIMARY KEY,
    version INTEGER NOT NULL REFERENCES bookbrainz_v.edition_v (version),
    book_id INTEGER NOT NULL REFERENCES bookbrainz_v.book (book_id),
    publisher_id INTEGER REFERENCES bookbrainz_v.publisher (publisher_id)
);

INSERT INTO bookbrainz_v.edition_tree (edition_tree_id, book_id, version, publisher_id)
SELECT edition_id, book_id, version, publisher_id FROM bookbrainz.edition;

ALTER TABLE bookbrainz_v.edition_revision
ADD COLUMN edition_tree_id INTEGER REFERENCES
  bookbrainz_v.edition_tree (edition_tree_id);

UPDATE bookbrainz_v.edition_revision SET edition_tree_id = version;

ALTER TABLE bookbrainz_v.edition_revision ALTER COLUMN edition_tree_id SET NOT NULL;
ALTER TABLE bookbrainz_v.edition_revision DROP COLUMN version CASCADE;
ALTER TABLE bookbrainz_v.edition_revision DROP COLUMN publisher_id CASCADE;
ALTER TABLE bookbrainz_v.edition DROP COLUMN book_id CASCADE;

ALTER TABLE bookbrainz.edition_person_role
ADD COLUMN edition_tree_id INTEGER REFERENCES bookbrainz_v.edition_tree (edition_tree_id);

UPDATE bookbrainz.edition_person_role SET edition_tree_id = edition_revision.edition_tree_id
FROM bookbrainz_v.edition_revision
WHERE edition_revision.rev_id = edition_person_role.rev_id;

ALTER TABLE bookbrainz.edition_person_role
ALTER COLUMN edition_tree_id SET NOT NULL;

ALTER TABLE bookbrainz.edition_person_role DROP COLUMN rev_id;

CREATE OR REPLACE VIEW bookbrainz.edition AS
SELECT edition_id, gid, name, book_id, year, publisher_id, country_iso_code,
       language_iso_code, isbn, barcode, edition_index, format,
       version, rev_id
FROM bookbrainz_v.branch
JOIN bookbrainz_v.edition_branch ON branch_id = branch.id
JOIN bookbrainz_v.edition_revision USING (rev_id)
JOIN bookbrainz_v.edition USING (edition_id)
JOIN bookbrainz_v.edition_gid USING (edition_id)
JOIN bookbrainz_v.edition_tree USING (edition_tree_id)
JOIN bookbrainz_v.edition_v USING (version)
WHERE branch.master = TRUE;


--------------------------------------------------------------------------------
CREATE TABLE bookbrainz_v.publisher_tree (
    publisher_tree_id SERIAL NOT NULL PRIMARY KEY,
    version INTEGER NOT NULL REFERENCES bookbrainz_v.publisher_v (version)
);

INSERT INTO bookbrainz_v.publisher_tree (publisher_tree_id, version)
SELECT publisher_id, version FROM bookbrainz.publisher;

ALTER TABLE bookbrainz_v.publisher_revision
ADD COLUMN publisher_tree_id INTEGER REFERENCES
  bookbrainz_v.publisher_tree (publisher_tree_id);

UPDATE bookbrainz_v.publisher_revision SET publisher_tree_id = version;

ALTER TABLE bookbrainz_v.publisher_revision ALTER COLUMN publisher_tree_id SET NOT NULL;
ALTER TABLE bookbrainz_v.publisher_revision DROP COLUMN version CASCADE;

CREATE OR REPLACE VIEW bookbrainz.publisher AS
SELECT publisher_id, gid, name, version, rev_id
FROM bookbrainz_v.branch
JOIN bookbrainz_v.publisher_branch ON branch_id = branch.id
JOIN bookbrainz_v.publisher_revision USING (rev_id)
JOIN bookbrainz_v.publisher USING (publisher_id)
JOIN bookbrainz_v.publisher_gid USING (publisher_id)
JOIN bookbrainz_v.publisher_tree USING (publisher_tree_id)
JOIN bookbrainz_v.publisher_v USING (version)
WHERE branch.master = TRUE;

--------------------------------------------------------------------------------
CREATE TABLE bookbrainz_v.person_tree (
    person_tree_id SERIAL NOT NULL PRIMARY KEY,
    version INTEGER NOT NULL REFERENCES bookbrainz_v.person_v (version)
);

INSERT INTO bookbrainz_v.person_tree (person_tree_id, version)
SELECT person_id, version FROM bookbrainz.person;

ALTER TABLE bookbrainz_v.person_revision
ADD COLUMN person_tree_id INTEGER REFERENCES
  bookbrainz_v.person_tree (person_tree_id);

UPDATE bookbrainz_v.person_revision SET person_tree_id = version;

ALTER TABLE bookbrainz_v.person_revision ALTER COLUMN person_tree_id SET NOT NULL;
ALTER TABLE bookbrainz_v.person_revision DROP COLUMN version CASCADE;

CREATE OR REPLACE VIEW bookbrainz.person AS
SELECT person_id, gid, name, version, rev_id
FROM bookbrainz_v.branch
JOIN bookbrainz_v.person_branch ON branch_id = branch.id
JOIN bookbrainz_v.person_revision USING (rev_id)
JOIN bookbrainz_v.person USING (person_id)
JOIN bookbrainz_v.person_gid USING (person_id)
JOIN bookbrainz_v.person_tree USING (person_tree_id)
JOIN bookbrainz_v.person_v USING (version)
WHERE branch.master = TRUE;

COMMIT;
