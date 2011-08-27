BEGIN;

SELECT _v.register_patch('20110827-revision-parent', ARRAY['20110804-versions'], NULL);

CREATE TABLE bookbrainz_v.revision_parent (
    rev_id INT NOT NULL REFERENCES bookbrainz_v.revision (rev_id),
    parent_id INT NOT NULL REFERENCES bookbrainz_v.revision (rev_id)
);

ALTER TABLE bookbrainz_v.branch ADD COLUMN id SERIAL NOT NULL PRIMARY KEY;

COMMIT;
