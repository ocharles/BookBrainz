BEGIN;

SELECT
  _v.register_patch('20110918-pks', ARRAY['20110918-trees'], NULL);

DROP TABLE bookbrainz.author_credit CASCADE;

ALTER TABLE bookbrainz.book_person_role
ADD PRIMARY KEY (role_id, person_id, book_tree_id);

ALTER TABLE bookbrainz.edition_person_role
ADD PRIMARY KEY (role_id, person_id, edition_tree_id);

COMMIT;
