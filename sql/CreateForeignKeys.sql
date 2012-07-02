BEGIN;

SET LOCAL SESSION AUTHORIZATION bookbrainz;
SET LOCAL search_path = 'bookbrainz', 'public';

--------------------------------------------------------------------------------
ALTER TABLE revision ADD FOREIGN KEY (editor_id)
REFERENCES editor (editor_id);

--------------------------------------------------------------------------------
ALTER TABLE book ADD FOREIGN KEY (master_revision)
REFERENCES book_revision (revision_id)
DEFERRABLE INITIALLY DEFERRED;

ALTER TABLE book ADD FOREIGN KEY (merged_into)
REFERENCES book (book_bbid);

ALTER TABLE book_tree ADD FOREIGN KEY (book_data_id)
REFERENCES book_data (book_data_id);

ALTER TABLE book_person_role ADD FOREIGN KEY (person_role_id)
REFERENCES person_role (person_role_id);

ALTER TABLE book_person_role ADD FOREIGN KEY (book_tree_id)
REFERENCES book_tree (book_tree_id);

ALTER TABLE book_person_role ADD FOREIGN KEY (person_bbid)
REFERENCES person (person_bbid);

ALTER TABLE book_revision ADD FOREIGN KEY (revision_id)
REFERENCES revision (revision_id);

ALTER TABLE book_revision ADD FOREIGN KEY (book_tree_id)
REFERENCES book_tree (book_tree_id);

ALTER TABLE book_revision ADD FOREIGN KEY (book_bbid)
REFERENCES book (book_bbid);

--------------------------------------------------------------------------------
ALTER TABLE edition ADD FOREIGN KEY (master_revision)
REFERENCES edition_revision (revision_id)
DEFERRABLE INITIALLY DEFERRED;

ALTER TABLE edition ADD FOREIGN KEY (merged_into)
REFERENCES edition (edition_bbid);

ALTER TABLE edition_data ADD FOREIGN KEY (country_iso_code)
REFERENCES country (iso_code);

ALTER TABLE edition_data ADD FOREIGN KEY (language_iso_code)
REFERENCES language (iso_code);

ALTER TABLE edition_tree ADD FOREIGN KEY (edition_data_id)
REFERENCES edition_data (edition_data_id);

ALTER TABLE edition_tree ADD FOREIGN KEY (book_bbid)
REFERENCES book (book_bbid);

ALTER TABLE edition_tree ADD FOREIGN KEY (publisher_bbid)
REFERENCES publisher (publisher_bbid);

ALTER TABLE edition_person_role ADD FOREIGN KEY (person_role_id)
REFERENCES person_role (person_role_id);

ALTER TABLE edition_person_role ADD FOREIGN KEY (edition_tree_id)
REFERENCES edition_tree (edition_tree_id);

ALTER TABLE edition_person_role ADD FOREIGN KEY (person_bbid)
REFERENCES person (person_bbid);

ALTER TABLE edition_revision ADD FOREIGN KEY (revision_id)
REFERENCES revision (revision_id);

ALTER TABLE edition_revision ADD FOREIGN KEY (edition_tree_id)
REFERENCES edition_tree (edition_tree_id);

ALTER TABLE edition_revision ADD FOREIGN KEY (edition_bbid)
REFERENCES edition (edition_bbid);

--------------------------------------------------------------------------------
ALTER TABLE person ADD FOREIGN KEY (master_revision)
REFERENCES person_revision (revision_id)
DEFERRABLE INITIALLY DEFERRED;

ALTER TABLE person ADD FOREIGN KEY (merged_into)
REFERENCES person (person_bbid);

ALTER TABLE person_tree ADD FOREIGN KEY (person_data_id)
REFERENCES person_data (person_data_id);

ALTER TABLE person_revision ADD FOREIGN KEY (revision_id)
REFERENCES revision (revision_id);

ALTER TABLE person_revision ADD FOREIGN KEY (person_tree_id)
REFERENCES person_tree (person_tree_id);

ALTER TABLE person_revision ADD FOREIGN KEY (person_bbid)
REFERENCES person (person_bbid);

--------------------------------------------------------------------------------
ALTER TABLE publisher ADD FOREIGN KEY (master_revision)
REFERENCES publisher_revision (revision_id)
DEFERRABLE INITIALLY DEFERRED;

ALTER TABLE publisher ADD FOREIGN KEY (merged_into)
REFERENCES publisher (publisher_bbid);

ALTER TABLE publisher_tree ADD FOREIGN KEY (publisher_data_id)
REFERENCES publisher_data (publisher_data_id);

ALTER TABLE publisher_revision ADD FOREIGN KEY (revision_id)
REFERENCES revision (revision_id);

ALTER TABLE publisher_revision ADD FOREIGN KEY (publisher_tree_id)
REFERENCES publisher_tree (publisher_tree_id);

ALTER TABLE publisher_revision ADD FOREIGN KEY (publisher_bbid)
REFERENCES publisher (publisher_bbid);

COMMIT;
