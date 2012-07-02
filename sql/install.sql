\set ON_ERROR_STOP 1
SET client_min_messages = 'warning';

DROP SCHEMA IF EXISTS bookbrainz CASCADE;

\ir CreateSchema.sql
\ir CreateExtensions.sql
\ir CreateTables.sql
\ir CreateFunctions.sql
\ir CreatePrimaryKeys.sql
\ir CreateForeignKeys.sql
\ir CreateConstraints.sql
\ir CreateIndexes.sql
\ir CreateViews.sql
