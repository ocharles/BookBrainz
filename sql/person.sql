BEGIN;

SELECT _v.register_patch('2011-07-08', NULL, NULL);
CREATE TABLE person (
    name TEXT NOT NULL PRIMARY KEY,
    gid UUID NOT NULL
);

COMMIT;

