BEGIN;

SELECT
  _v.register_patch('20110927-unique-bbid', ARRAY['20110919-tree-in-view'], NULL);

CREATE TABLE bookbrainz_v.bbid (
    bbid UUID NOT NULL PRIMARY KEY
);

INSERT INTO bookbrainz_v.bbid (bbid)
   SELECT DISTINCT gid FROM bookbrainz.book
   UNION
   SELECT DISTINCT gid FROM bookbrainz.edition
   UNION
   SELECT DISTINCT gid FROM bookbrainz.person
   UNION
   SELECT DISTINCT gid FROM bookbrainz.publisher;

ALTER TABLE bookbrainz_v.book_gid
ADD CONSTRAINT book_gid_bbid
FOREIGN KEY (gid) REFERENCES bookbrainz_v.bbid (bbid);
ALTER TABLE bookbrainz_v.book_gid RENAME COLUMN gid TO bbid;
ALTER TABLE bookbrainz_v.book_gid RENAME TO book_bbid;

ALTER TABLE bookbrainz_v.edition_gid
ADD CONSTRAINT book_gid_bbid
FOREIGN KEY (gid) REFERENCES bookbrainz_v.bbid (bbid);
ALTER TABLE bookbrainz_v.edition_gid RENAME COLUMN gid TO bbid;
ALTER TABLE bookbrainz_v.edition_gid RENAME TO edition_bbid;

ALTER TABLE bookbrainz_v.person_gid
ADD CONSTRAINT book_gid_bbid
FOREIGN KEY (gid) REFERENCES bookbrainz_v.bbid (bbid);
ALTER TABLE bookbrainz_v.person_gid RENAME COLUMN gid TO bbid;
ALTER TABLE bookbrainz_v.person_gid RENAME TO person_bbid;

ALTER TABLE bookbrainz_v.publisher_gid
ADD CONSTRAINT book_gid_bbid
FOREIGN KEY (gid) REFERENCES bookbrainz_v.bbid (bbid);
ALTER TABLE bookbrainz_v.publisher_gid RENAME COLUMN gid TO bbid;
ALTER TABLE bookbrainz_v.publisher_gid RENAME TO publisher_bbid;

DROP VIEW bookbrainz.book;
DROP VIEW bookbrainz.edition;
DROP VIEW bookbrainz.person;
DROP VIEW bookbrainz.publisher;

CREATE OR REPLACE VIEW bookbrainz.book AS
SELECT book_id, bbid, name, book_tree_id, rev_id
FROM bookbrainz_v.branch
JOIN bookbrainz_v.book_branch ON branch_id = branch.id
JOIN bookbrainz_v.book_revision USING (rev_id)
JOIN bookbrainz_v.book USING (book_id)
JOIN bookbrainz_v.book_bbid USING (book_id)
JOIN bookbrainz_v.book_tree USING (book_tree_id)
JOIN bookbrainz_v.book_v USING (version)
WHERE branch.master = TRUE;

CREATE OR REPLACE VIEW bookbrainz.edition AS
SELECT edition_id, bbid, name, book_id, year, publisher_id, country_iso_code,
       language_iso_code, isbn, barcode, edition_index, format,
       edition_tree_id, rev_id
FROM bookbrainz_v.branch
JOIN bookbrainz_v.edition_branch ON branch_id = branch.id
JOIN bookbrainz_v.edition_revision USING (rev_id)
JOIN bookbrainz_v.edition USING (edition_id)
JOIN bookbrainz_v.edition_bbid USING (edition_id)
JOIN bookbrainz_v.edition_tree USING (edition_tree_id)
JOIN bookbrainz_v.edition_v USING (version)
WHERE branch.master = TRUE;

CREATE OR REPLACE VIEW bookbrainz.publisher AS
SELECT publisher_id, bbid, name, publisher_tree_id, rev_id
FROM bookbrainz_v.branch
JOIN bookbrainz_v.publisher_branch ON branch_id = branch.id
JOIN bookbrainz_v.publisher_revision USING (rev_id)
JOIN bookbrainz_v.publisher USING (publisher_id)
JOIN bookbrainz_v.publisher_bbid USING (publisher_id)
JOIN bookbrainz_v.publisher_tree USING (publisher_tree_id)
JOIN bookbrainz_v.publisher_v USING (version)
WHERE branch.master = TRUE;

CREATE OR REPLACE VIEW bookbrainz.person AS
SELECT person_id, bbid, name, person_tree_id, rev_id
FROM bookbrainz_v.branch
JOIN bookbrainz_v.person_branch ON branch_id = branch.id
JOIN bookbrainz_v.person_revision USING (rev_id)
JOIN bookbrainz_v.person USING (person_id)
JOIN bookbrainz_v.person_bbid USING (person_id)
JOIN bookbrainz_v.person_tree USING (person_tree_id)
JOIN bookbrainz_v.person_v USING (version)
WHERE branch.master = TRUE;

COMMIT;
