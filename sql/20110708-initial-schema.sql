BEGIN;

SELECT _v.register_patch('20110708-initial-schema', ARRAY['2011-07-08'], NULL);

CREATE TABLE author_credit (
    id SERIAL PRIMARY KEY
);

ALTER TABLE person ADD COLUMN id SERIAL;
ALTER TABLE person DROP CONSTRAINT person_pkey;
ALTER TABLE person ADD PRIMARY KEY (id);

CREATE TABLE author_credit_person (
    author_credit INT NOT NULL REFERENCES author_credit (id),
    credited_name TEXT NOT NULL,
    position INT NOT NULL DEFAULT 0,
    person INT NOT NULL REFERENCES person (id),
    join_phrase TEXT NOT NULL DEFAULT '',
    PRIMARY KEY (author_credit, position)
);

CREATE TABLE book (
    gid UUID NOT NULL,
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL,
    author_credit INT NOT NULL REFERENCES author_credit (id)
);

CREATE TABLE publisher (
    id SERIAL PRIMARY KEY,
    gid UUID NOT NULL,
    name TEXT NOT NULL
);

CREATE TABLE edition_format (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL
);

CREATE TABLE country (
    iso_code TEXT NOT NULL PRIMARY KEY,
    name TEXT NOT NULL
);

CREATE TABLE language (
    iso_code TEXT NOT NULL PRIMARY KEY,
    name TEXT NOT NULL
);

CREATE TABLE edition (
    gid UUID NOT NULL,
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL,
    book INT NOT NULL REFERENCES book (id),
    year INT,
    publisher INT REFERENCES publisher (id),
    country_iso_code TEXT REFERENCES country (iso_code),
    illustrator INT REFERENCES author_credit (id),
    translator INT REFERENCES author_credit (id),
    author INT REFERENCES author_credit (id),
    language_iso_code TEXT REFERENCES language (iso_code),
    isbn TEXT,
    barcode TEXT,
    edition_index INT CHECK (edition_index IS NULL OR edition_index > 0),
    format INT REFERENCES edition_format (id)
);

COMMIT;
