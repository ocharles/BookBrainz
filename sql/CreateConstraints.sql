BEGIN;

SET LOCAL SESSION AUTHORIZATION bookbrainz;
SET LOCAL search_path = 'bookbrainz', 'public';

ALTER TABLE editor ADD UNIQUE (name);
ALTER TABLE editor ADD CHECK (text_is_normalized(name));

ALTER TABLE revision ADD CHECK (created_at > '2012-07-01');
ALTER TABLE revision_parent ADD CHECK (child_id != parent_id);

--------------------------------------------------------------------------------
ALTER TABLE book ADD UNIQUE (book_bbid);
ALTER TABLE book ADD UNIQUE (master_revision);
ALTER TABLE book_data ADD CHECK (text_is_normalized(name));

--------------------------------------------------------------------------------
ALTER TABLE edition ADD UNIQUE (edition_bbid);
ALTER TABLE edition ADD UNIQUE (master_revision);
ALTER TABLE edition_data ADD CHECK (text_is_normalized(name));

--------------------------------------------------------------------------------
ALTER TABLE person ADD UNIQUE (person_bbid);
ALTER TABLE person ADD UNIQUE (master_revision);
ALTER TABLE person_data ADD CHECK (text_is_normalized(name));

--------------------------------------------------------------------------------
ALTER TABLE publisher ADD UNIQUE (publisher_bbid);
ALTER TABLE publisher ADD UNIQUE (master_revision);
ALTER TABLE publisher_data ADD CHECK (text_is_normalized(name));

--------------------------------------------------------------------------------
ALTER TABLE country ADD CHECK (text_is_normalized(name));
ALTER TABLE country ADD CHECK (text_is_normalized(iso_code));

ALTER TABLE language ADD CHECK (text_is_normalized(name));
ALTER TABLE language ADD CHECK (text_is_normalized(iso_code));

ALTER TABLE person_role ADD CHECK (text_is_normalized(name));

COMMIT;
