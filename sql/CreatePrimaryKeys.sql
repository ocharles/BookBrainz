BEGIN;

SET LOCAL SESSION AUTHORIZATION bookbrainz;
SET LOCAL search_path = 'bookbrainz', 'public';

ALTER TABLE editor ADD PRIMARY KEY (editor_id);
ALTER TABLE revision ADD PRIMARY KEY (revision_id);
ALTER TABLE revision_parent ADD PRIMARY KEY (child_id, parent_id);

ALTER TABLE book ADD PRIMARY KEY (book_bbid);
ALTER TABLE book_data ADD PRIMARY KEY (book_data_id);
ALTER TABLE book_tree ADD PRIMARY KEY (book_tree_id);
ALTER TABLE book_person_role ADD PRIMARY KEY
  (book_tree_id, person_role_id, person_bbid);
ALTER TABLE book_revision ADD PRIMARY KEY (revision_id);

ALTER TABLE edition ADD PRIMARY KEY (edition_bbid);
ALTER TABLE edition_data ADD PRIMARY KEY (edition_data_id);
ALTER TABLE edition_tree ADD PRIMARY KEY (edition_tree_id);
ALTER TABLE edition_person_role ADD PRIMARY KEY
  (edition_tree_id, person_role_id, person_bbid);
ALTER TABLE edition_revision ADD PRIMARY KEY (revision_id);

ALTER TABLE person ADD PRIMARY KEY (person_bbid);
ALTER TABLE person_data ADD PRIMARY KEY (person_data_id);
ALTER TABLE person_tree ADD PRIMARY KEY (person_tree_id);
ALTER TABLE person_revision ADD PRIMARY KEY (revision_id);

ALTER TABLE publisher ADD PRIMARY KEY (publisher_bbid);
ALTER TABLE publisher_data ADD PRIMARY KEY (publisher_data_id);
ALTER TABLE publisher_tree ADD PRIMARY KEY (publisher_tree_id);
ALTER TABLE publisher_revision ADD PRIMARY KEY (revision_id);

ALTER TABLE country ADD PRIMARY KEY (iso_code);
ALTER TABLE language ADD PRIMARY KEY (iso_code);

ALTER TABLE person_role ADD PRIMARY KEY (person_role_id);

COMMIT;
