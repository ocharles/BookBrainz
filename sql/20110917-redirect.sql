BEGIN;

SELECT _v.register_patch('20110917-redirect', ARRAY['20110804-versions'], NULL);

ALTER TABLE bookbrainz_v.book RENAME TO book_v;
ALTER TABLE bookbrainz_v.edition RENAME TO edition_v;
ALTER TABLE bookbrainz_v.person RENAME TO person_v;
ALTER TABLE bookbrainz_v.publisher RENAME TO publisher_v;

--------------------------------------------------------------------------------
CREATE TABLE bookbrainz_v.book (
    book_id SERIAL NOT NULL PRIMARY KEY
);
CREATE TABLE bookbrainz_v.book_gid (
    book_id INTEGER NOT NULL REFERENCES bookbrainz_v.book (book_id),
    gid UUID NOT NULL PRIMARY KEY
);
CREATE TABLE bookbrainz_v.book_branch (
    book_id INTEGER NOT NULL REFERENCES bookbrainz_v.book (book_id),
    branch_id INTEGER NOT NULL REFERENCES bookbrainz_v.branch (id),
    PRIMARY KEY (book_id, branch_id)
);
CREATE INDEX ON bookbrainz_v.book_branch (branch_id);
CREATE INDEX ON bookbrainz_v.book_branch (book_id);

INSERT INTO bookbrainz_v.book (book_id)
  SELECT DISTINCT version FROM bookbrainz_v.book_v;

INSERT INTO bookbrainz_v.book_gid (book_id, gid)
  SELECT DISTINCT version, gid FROM bookbrainz_v.book_v;

INSERT INTO bookbrainz_v.book_branch (branch_id, book_id)
  SELECT id, book_id FROM bookbrainz_v.branch
  JOIN bookbrainz_v.book_gid USING (gid);

ALTER TABLE bookbrainz_v.book_v DROP COLUMN gid CASCADE;
CREATE UNIQUE INDEX ON bookbrainz_v.book_v (name);

--------------------------------------------------------------------------------
CREATE TABLE bookbrainz_v.edition (
    edition_id SERIAL NOT NULL PRIMARY KEY
);
CREATE TABLE bookbrainz_v.edition_gid (
    edition_id INTEGER NOT NULL REFERENCES bookbrainz_v.edition (edition_id),
    gid UUID NOT NULL PRIMARY KEY
);
CREATE TABLE bookbrainz_v.edition_branch (
    edition_id INTEGER NOT NULL REFERENCES bookbrainz_v.edition (edition_id),
    branch_id INTEGER NOT NULL REFERENCES bookbrainz_v.branch (id),
    PRIMARY KEY (edition_id, branch_id)
);
CREATE INDEX ON bookbrainz_v.edition_branch (branch_id);
CREATE INDEX ON bookbrainz_v.edition_branch (edition_id);

INSERT INTO bookbrainz_v.edition (edition_id)
  SELECT DISTINCT version FROM bookbrainz_v.edition_v;

INSERT INTO bookbrainz_v.edition_gid (edition_id, gid)
  SELECT DISTINCT version, gid FROM bookbrainz_v.edition_v;

INSERT INTO bookbrainz_v.edition_branch (branch_id, edition_id)
  SELECT id, edition_id FROM bookbrainz_v.branch
  JOIN bookbrainz_v.edition_gid USING (gid);

ALTER TABLE bookbrainz_v.edition_v DROP COLUMN gid CASCADE;
CREATE UNIQUE INDEX ON bookbrainz_v.edition_v
  (name, book, year, publisher, country_iso_code
  ,language_iso_code, isbn, barcode, edition_index, format);

--------------------------------------------------------------------------------
CREATE TABLE bookbrainz_v.person (
    person_id SERIAL NOT NULL PRIMARY KEY
);
CREATE TABLE bookbrainz_v.person_gid (
    person_id INTEGER NOT NULL REFERENCES bookbrainz_v.person (person_id),
    gid UUID NOT NULL PRIMARY KEY
);
CREATE TABLE bookbrainz_v.person_branch (
    person_id INTEGER NOT NULL REFERENCES bookbrainz_v.person (person_id),
    branch_id INTEGER NOT NULL REFERENCES bookbrainz_v.branch (id),
    PRIMARY KEY (person_id, branch_id)
);
CREATE INDEX ON bookbrainz_v.person_branch (branch_id);
CREATE INDEX ON bookbrainz_v.person_branch (person_id);

INSERT INTO bookbrainz_v.person (person_id)
  SELECT DISTINCT version FROM bookbrainz_v.person_v;

INSERT INTO bookbrainz_v.person_gid (person_id, gid)
  SELECT DISTINCT version, gid FROM bookbrainz_v.person_v;

INSERT INTO bookbrainz_v.person_branch (branch_id, person_id)
  SELECT id, person_id FROM bookbrainz_v.branch
  JOIN bookbrainz_v.person_gid USING (gid);

ALTER TABLE bookbrainz_v.person_v DROP COLUMN gid CASCADE;

--------------------------------------------------------------------------------
CREATE TABLE bookbrainz_v.publisher (
    publisher_id SERIAL NOT NULL PRIMARY KEY
);
CREATE TABLE bookbrainz_v.publisher_gid (
    publisher_id INTEGER NOT NULL REFERENCES bookbrainz_v.publisher (publisher_id),
    gid UUID NOT NULL PRIMARY KEY
);
CREATE TABLE bookbrainz_v.publisher_branch (
    publisher_id INTEGER NOT NULL REFERENCES bookbrainz_v.publisher (publisher_id),
    branch_id INTEGER NOT NULL REFERENCES bookbrainz_v.branch (id),
    PRIMARY KEY (publisher_id, branch_id)
);
CREATE INDEX ON bookbrainz_v.publisher_branch (branch_id);
CREATE INDEX ON bookbrainz_v.publisher_branch (publisher_id);

INSERT INTO bookbrainz_v.publisher (publisher_id)
  SELECT DISTINCT version FROM bookbrainz_v.publisher_v;

INSERT INTO bookbrainz_v.publisher_gid (publisher_id, gid)
  SELECT DISTINCT version, gid FROM bookbrainz_v.publisher_v;

INSERT INTO bookbrainz_v.publisher_branch (branch_id, publisher_id)
  SELECT id, publisher_id FROM bookbrainz_v.branch
  JOIN bookbrainz_v.publisher_gid USING (gid);

ALTER TABLE bookbrainz_v.publisher_v DROP COLUMN gid CASCADE;

--------------------------------------------------------------------------------
ALTER TABLE bookbrainz_v.branch DROP COLUMN gid;

CREATE OR REPLACE VIEW bookbrainz.book AS
SELECT book_id, gid, name, version, rev_id
FROM bookbrainz_v.branch
JOIN bookbrainz_v.book_branch ON branch_id = branch.id
JOIN bookbrainz_v.book_revision USING (rev_id)
JOIN bookbrainz_v.book_v USING (version)
JOIN bookbrainz_v.book USING (book_id)
JOIN bookbrainz_v.book_gid USING (book_id)
WHERE branch.master = TRUE;

CREATE OR REPLACE VIEW bookbrainz.edition AS
SELECT edition_id, gid, name, book, year, publisher, country_iso_code,
       language_iso_code, isbn, barcode, edition_index, format,
       version, rev_id
FROM bookbrainz_v.branch
JOIN bookbrainz_v.edition_branch ON branch_id = branch.id
JOIN bookbrainz_v.edition_revision USING (rev_id)
JOIN bookbrainz_v.edition_v USING (version)
JOIN bookbrainz_v.edition USING (edition_id)
JOIN bookbrainz_v.edition_gid USING (edition_id)
WHERE branch.master = TRUE;

CREATE OR REPLACE VIEW bookbrainz.publisher AS
SELECT publisher_id, gid, name, version, rev_id
FROM bookbrainz_v.branch
JOIN bookbrainz_v.publisher_branch ON branch_id = branch.id
JOIN bookbrainz_v.publisher_revision USING (rev_id)
JOIN bookbrainz_v.publisher_v USING (version)
JOIN bookbrainz_v.publisher USING (publisher_id)
JOIN bookbrainz_v.publisher_gid USING (publisher_id)
WHERE branch.master = TRUE;

CREATE OR REPLACE VIEW bookbrainz.person AS
SELECT person_id, gid, name, version, rev_id
FROM bookbrainz_v.branch
JOIN bookbrainz_v.person_branch ON branch_id = branch.id
JOIN bookbrainz_v.person_revision USING (rev_id)
JOIN bookbrainz_v.person_v USING (version)
JOIN bookbrainz_v.person USING (person_id)
JOIN bookbrainz_v.person_gid USING (person_id)
WHERE branch.master = TRUE;

COMMIT;
