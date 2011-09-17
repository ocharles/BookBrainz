BEGIN;

SELECT _v.register_patch('20110917-general-references', ARRAY['20110917-redirect'], NULL);

--------------------------------------------------------------------------------
ALTER TABLE bookbrainz_v.book_revision ADD PRIMARY KEY (rev_id);

ALTER TABLE bookbrainz.book_person_role ADD COLUMN person_id INTEGER REFERENCES bookbrainz_v.person (person_id);
ALTER TABLE bookbrainz.book_person_role ADD COLUMN rev_id INTEGER REFERENCES bookbrainz_v.book_revision (rev_id);

UPDATE bookbrainz.book_person_role SET person_id = person;
UPDATE bookbrainz.book_person_role SET rev_id = book_revision.rev_id
FROM bookbrainz_v.book_revision
WHERE book_revision.version = book;

ALTER TABLE bookbrainz.book_person_role DROP COLUMN person;
ALTER TABLE bookbrainz.book_person_role DROP COLUMN book;

ALTER TABLE bookbrainz.book_person_role ALTER COLUMN person_id SET NOT NULL;
ALTER TABLE bookbrainz.book_person_role ALTER COLUMN rev_id SET NOT NULL;

--------------------------------------------------------------------------------
ALTER TABLE bookbrainz_v.edition_revision ADD COLUMN publisher_id INTEGER REFERENCES bookbrainz_v.publisher (publisher_id);

UPDATE bookbrainz_v.edition_revision SET publisher_id = e.publisher
FROM bookbrainz_v.edition_v e
WHERE e.version = edition_revision.version;

ALTER TABLE bookbrainz_v.edition_v DROP COLUMN publisher CASCADE;

CREATE OR REPLACE VIEW bookbrainz.edition AS
SELECT edition_id, gid, name, book, year, publisher_id, country_iso_code,
       language_iso_code, isbn, barcode, edition_index, format,
       version, rev_id
FROM bookbrainz_v.branch
JOIN bookbrainz_v.edition_branch ON branch_id = branch.id
JOIN bookbrainz_v.edition_revision USING (rev_id)
JOIN bookbrainz_v.edition_v USING (version)
JOIN bookbrainz_v.edition USING (edition_id)
JOIN bookbrainz_v.edition_gid USING (edition_id)
WHERE branch.master = TRUE;

--------------------------------------------------------------------------------
ALTER TABLE bookbrainz_v.edition ADD COLUMN book_id INTEGER REFERENCES bookbrainz_v.book (book_id);

UPDATE bookbrainz_v.edition SET book_id = e.book
FROM bookbrainz.edition e
WHERE edition.edition_id = e.edition_id;

ALTER TABLE bookbrainz_v.edition_v DROP COLUMN book CASCADE;
ALTER TABLE bookbrainz_v.edition ALTER COLUMN book_id SET NOT NULL;

CREATE OR REPLACE VIEW bookbrainz.edition AS
SELECT edition_id, gid, name, book_id, year, publisher_id, country_iso_code,
       language_iso_code, isbn, barcode, edition_index, format,
       version, rev_id
FROM bookbrainz_v.branch
JOIN bookbrainz_v.edition_branch ON branch_id = branch.id
JOIN bookbrainz_v.edition_revision USING (rev_id)
JOIN bookbrainz_v.edition_v USING (version)
JOIN bookbrainz_v.edition USING (edition_id)
JOIN bookbrainz_v.edition_gid USING (edition_id)
WHERE branch.master = TRUE;

--------------------------------------------------------------------------------
ALTER TABLE bookbrainz_v.edition_revision ADD PRIMARY KEY (rev_id);

ALTER TABLE bookbrainz.edition_person_role ADD COLUMN person_id INTEGER REFERENCES bookbrainz_v.person (person_id);
ALTER TABLE bookbrainz.edition_person_role ADD COLUMN rev_id INTEGER REFERENCES bookbrainz_v.edition_revision (rev_id);

UPDATE bookbrainz.edition_person_role SET person_id = person;
UPDATE bookbrainz.edition_person_role SET rev_id = edition_revision.rev_id
FROM bookbrainz_v.edition_revision
WHERE edition_revision.version = edition;

ALTER TABLE bookbrainz.edition_person_role DROP COLUMN person;
ALTER TABLE bookbrainz.edition_person_role DROP COLUMN edition;

ALTER TABLE bookbrainz.edition_person_role ALTER COLUMN person_id SET NOT NULL;
ALTER TABLE bookbrainz.edition_person_role ALTER COLUMN rev_id SET NOT NULL;

COMMIT;
