BEGIN;

SELECT _v.register_patch('20110918-editor', ARRAY['20110918-pks'], NULL);

CREATE TABLE bookbrainz.editor (
    editor_id SERIAL NOT NULL PRIMARY KEY,
    name TEXT NOT NULL,
    password TEXT NOT NULL
);

CREATE UNIQUE INDEX ON bookbrainz.editor (name);

INSERT INTO bookbrainz.editor (editor_id, name, password)
  VALUES (1, 'BookBrainz', '');

ALTER TABLE bookbrainz_v.revision ADD COLUMN editor INTEGER REFERENCES bookbrainz.editor (editor_id);

UPDATE bookbrainz_v.revision SET editor = 1;

ALTER TABLE bookbrainz_v.revision ALTER COLUMN editor SET NOT NULL;

COMMIT;
