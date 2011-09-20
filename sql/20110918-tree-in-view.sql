BEGIN;

SELECT
  _v.register_patch('20110919-tree-in-view', ARRAY['20110918-trees'], NULL);

CREATE OR REPLACE VIEW bookbrainz.book AS
SELECT book_id, gid, name, book_tree_id, rev_id
FROM bookbrainz_v.branch
JOIN bookbrainz_v.book_branch ON branch_id = branch.id
JOIN bookbrainz_v.book_revision USING (rev_id)
JOIN bookbrainz_v.book USING (book_id)
JOIN bookbrainz_v.book_gid USING (book_id)
JOIN bookbrainz_v.book_tree USING (book_tree_id)
JOIN bookbrainz_v.book_v USING (version)
WHERE branch.master = TRUE;

CREATE OR REPLACE VIEW bookbrainz.edition AS
SELECT edition_id, gid, name, book_id, year, publisher_id, country_iso_code,
       language_iso_code, isbn, barcode, edition_index, format,
       edition_tree_id, rev_id
FROM bookbrainz_v.branch
JOIN bookbrainz_v.edition_branch ON branch_id = branch.id
JOIN bookbrainz_v.edition_revision USING (rev_id)
JOIN bookbrainz_v.edition USING (edition_id)
JOIN bookbrainz_v.edition_gid USING (edition_id)
JOIN bookbrainz_v.edition_tree USING (edition_tree_id)
JOIN bookbrainz_v.edition_v USING (version)
WHERE branch.master = TRUE;

CREATE OR REPLACE VIEW bookbrainz.publisher AS
SELECT publisher_id, gid, name, publisher_tree_id, rev_id
FROM bookbrainz_v.branch
JOIN bookbrainz_v.publisher_branch ON branch_id = branch.id
JOIN bookbrainz_v.publisher_revision USING (rev_id)
JOIN bookbrainz_v.publisher USING (publisher_id)
JOIN bookbrainz_v.publisher_gid USING (publisher_id)
JOIN bookbrainz_v.publisher_tree USING (publisher_tree_id)
JOIN bookbrainz_v.publisher_v USING (version)
WHERE branch.master = TRUE;

CREATE OR REPLACE VIEW bookbrainz.person AS
SELECT person_id, gid, name, person_tree_id, rev_id
FROM bookbrainz_v.branch
JOIN bookbrainz_v.person_branch ON branch_id = branch.id
JOIN bookbrainz_v.person_revision USING (rev_id)
JOIN bookbrainz_v.person USING (person_id)
JOIN bookbrainz_v.person_gid USING (person_id)
JOIN bookbrainz_v.person_tree USING (person_tree_id)
JOIN bookbrainz_v.person_v USING (version)
WHERE branch.master = TRUE;

COMMIT;
