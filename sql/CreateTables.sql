BEGIN;

SET LOCAL SESSION AUTHORIZATION bookbrainz;
SET LOCAL search_path = 'bookbrainz', 'public';

--------------------------------------------------------------------------------
-- General versioning
CREATE TABLE editor (
  editor_id serial NOT NULL,
  name text NOT NULL
);

CREATE TABLE revision (
  revision_id serial NOT NULL,
  created_at timestamp WITH TIME ZONE NOT NULL DEFAULT now(),
  editor_id integer NOT NULL
);

CREATE TABLE revision_parent (
  child_id integer NOT NULL,
  parent_id integer NOT NULL
);

--------------------------------------------------------------------------------
-- Books
CREATE TABLE book (
  book_bbid uuid NOT NULL DEFAULT uuid_generate_v4(),
  master_revision integer NOT NULL,
  merged_into uuid
);

CREATE TABLE book_data (
  book_data_id serial NOT NULL,
  name text NOT NULL
);

CREATE TABLE book_tree (
  book_tree_id serial NOT NULL,
  book_data_id integer NOT NULL
);

CREATE TABLE book_person_role (
  book_tree_id integer NOT NULL,
  person_role_id integer NOT NULL,
  person_bbid uuid NOT NULL
);

CREATE TABLE book_revision (
  revision_id integer NOT NULL,
  book_bbid uuid NOT NULL,
  book_tree_id integer NOT NULL
);

--------------------------------------------------------------------------------
-- Editions
CREATE TABLE edition (
  edition_bbid uuid NOT NULL DEFAULT uuid_generate_v4(),
  master_revision integer NOT NULL,
  merged_into uuid
);

CREATE TABLE edition_data (
  edition_data_id serial NOT NULL,
  name text NOT NULL,
  year integer,
  country_iso_code text,
  language_iso_code text,
  isbn text,
  format integer
);

CREATE TABLE edition_tree (
  edition_tree_id serial NOT NULL,
  edition_data_id integer NOT NULL,
  book_bbid uuid NOT NULL,
  publisher_bbid uuid
);

CREATE TABLE edition_person_role (
  edition_tree_id integer NOT NULL,
  person_role_id integer NOT NULL,
  person_bbid uuid NOT NULL
);

CREATE TABLE edition_revision (
  revision_id integer NOT NULL,
  edition_tree_id integer NOT NULL,
  edition_bbid uuid NOT NULL
);

--------------------------------------------------------------------------------
-- Persons
CREATE TABLE person (
  person_bbid uuid NOT NULL DEFAULT uuid_generate_v4(),
  master_revision integer NOT NULL,
  merged_into uuid
);

CREATE TABLE person_data (
  person_data_id serial NOT NULL,
  name text NOT NULL
);

CREATE TABLE person_tree (
  person_tree_id serial NOT NULL,
  person_data_id integer NOT NULL
);

CREATE TABLE person_revision (
  revision_id integer NOT NULL,
  person_tree_id integer NOT NULL,
  person_bbid uuid NOT NULL
);

--------------------------------------------------------------------------------
-- Publishers
CREATE TABLE publisher (
  publisher_id serial NOT NULL,
  publisher_bbid uuid NOT NULL DEFAULT uuid_generate_v4(),
  master_revision integer NOT NULL,
  merged_into uuid
);

CREATE TABLE publisher_data (
  publisher_data_id serial NOT NULL,
  name text NOT NULL
);

CREATE TABLE publisher_tree (
  publisher_tree_id serial NOT NULL,
  publisher_data_id integer NOT NULL
);

CREATE TABLE publisher_revision (
  revision_id integer NOT NULL,
  publisher_tree_id integer NOT NULL,
  publisher_bbid uuid NOT NULL
);

--------------------------------------------------------------------------------
CREATE TABLE country (
  iso_code text NOT NULL,
  name text NOT NULL
);

CREATE TABLE language (
  iso_code text NOT NULL,
  name text NOT NULL
);

--------------------------------------------------------------------------------
CREATE TABLE person_role (
  person_role_id serial NOT NULL,
  name text NOT NULL
);

COMMIT;
