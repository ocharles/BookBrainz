BEGIN;

DROP SCHEMA IF EXISTS bookbrainz_v CASCADE;
DROP SCHEMA IF EXISTS bookbrainz CASCADE;

CREATE SCHEMA bookbrainz_v;
CREATE SCHEMA bookbrainz;

SET search_path = 'bookbrainz_v';
\i sql/bookbrainz_versioning/CreateTables.sql
\i sql/bookbrainz_versioning/CreatePrimaryKeys.sql

SET search_path = 'bookbrainz';
\i sql/public/CreateTables.sql
\i sql/public/CreatePrimaryKeys.sql

SET search_path = 'bookbrainz_v';
\i sql/bookbrainz_versioning/CreateFKConstraints.sql

SET search_path = 'bookbrainz';
\i sql/public/CreateFKConstraints.sql
\i sql/public/CreateViews.sql

COMMIT;
