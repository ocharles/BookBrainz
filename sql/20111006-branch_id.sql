BEGIN;

SELECT _v.register_patch('20111006-branch_id', ARRAY['20110927-unique-bbid'], NULL);

ALTER TABLE bookbrainz_v.branch RENAME COLUMN id TO branch_id;

COMMIT;
