BEGIN;

SELECT _v.register_patch('20111004-drop-barcode', ARRAY['20110927-unique-bbid'], NULL);

ALTER TABLE bookbrainz_v.edition_v DROP COLUMN barcode CASCADE;

CREATE OR REPLACE VIEW bookbrainz.edition AS
SELECT edition_id, bbid, name, book_id, year, publisher_id, country_iso_code,
       language_iso_code, isbn, edition_index, format,
       edition_tree_id, rev_id
FROM bookbrainz_v.branch
JOIN bookbrainz_v.edition_branch USING (branch_id)
JOIN bookbrainz_v.edition_revision USING (rev_id)
JOIN bookbrainz_v.edition USING (edition_id)
JOIN bookbrainz_v.edition_bbid USING (edition_id)
JOIN bookbrainz_v.edition_tree USING (edition_tree_id)
JOIN bookbrainz_v.edition_v USING (version)
WHERE branch.master = TRUE;

COMMIT;
