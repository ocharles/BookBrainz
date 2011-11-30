CREATE TABLE country (
    iso_code text NOT NULL, -- PK
    name text NOT NULL
);

CREATE TABLE edition_format (
    id integer NOT NULL, -- PK
    name text NOT NULL
);

CREATE TABLE editor (
    editor_id integer NOT NULL, -- PK
    name text NOT NULL,
    password text NOT NULL,
    remember_token character varying(64) DEFAULT NULL::character varying
);

CREATE TABLE language (
    iso_code text NOT NULL, -- PK
    name text NOT NULL
);

CREATE TABLE person_role (
    role_id integer NOT NULL, -- PK
    name text NOT NULL
);
