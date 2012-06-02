BEGIN;

SELECT _v.register_patch('20111001-remember', ARRAY['20110918-editor'], NULL);

ALTER TABLE editor ADD COLUMN remember_token VARCHAR(64) DEFAULT NULL;
CREATE UNIQUE INDEX ON editor (remember_token);

COMMIT;
