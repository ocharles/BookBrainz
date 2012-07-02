BEGIN;

SET LOCAL SESSION AUTHORIZATION bookbrainz;
SET LOCAL search_path = 'bookbrainz', 'public';

CREATE VIEW canonical_book_bbid AS
WITH RECURSIVE resolution (bbid, canonical) AS (
  SELECT book_bbid AS bbid, book_bbid AS canonical
  FROM book
  WHERE merged_into IS NULL
  UNION
  SELECT book_bbid, resolution.canonical
  FROM book
  JOIN resolution ON book.merged_into = resolution.bbid
  WHERE merged_into IS NOT NULL
)
SELECT * FROM resolution;

CREATE VIEW canonical_book AS
SELECT book_bbid, revision_id, book_tree_id, name
FROM book
JOIN book_revision USING (book_bbid)
JOIN book_tree USING (book_tree_id)
JOIN book_data USING (book_data_id)
WHERE merged_into IS NULL AND master_revision = revision_id;

COMMIT;
