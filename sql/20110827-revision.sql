BEGIN;

SELECT _v.register_patch('20110827-revision', ARRAY['20110804-versions'], NULL);

CREATE OR REPLACE VIEW bookbrainz.edition AS
SELECT edition.gid, name, book, year, publisher, country_iso_code,
       language_iso_code, isbn, barcode, edition_index, format,
       version, rev_id
FROM bookbrainz_v.branch
JOIN bookbrainz_v.edition_revision USING (rev_id)
JOIN bookbrainz_v.edition USING (version)
WHERE branch.master = TRUE;

CREATE OR REPLACE VIEW bookbrainz.book AS
SELECT book.gid, name, version, rev_id
FROM bookbrainz_v.branch
JOIN bookbrainz_v.book_revision USING (rev_id)
JOIN bookbrainz_v.book USING (version)
WHERE branch.master = TRUE;

CREATE OR REPLACE VIEW bookbrainz.publisher AS
SELECT publisher.gid, name, version, rev_id
FROM bookbrainz_v.branch
JOIN bookbrainz_v.publisher_revision USING (rev_id)
JOIN bookbrainz_v.publisher USING (version)
WHERE branch.master = TRUE;

CREATE OR REPLACE VIEW bookbrainz.person AS
SELECT person.gid, name, version, rev_id
FROM bookbrainz_v.branch
JOIN bookbrainz_v.person_revision USING (rev_id)
JOIN bookbrainz_v.person USING (version)
WHERE branch.master = TRUE;

COMMIT;
