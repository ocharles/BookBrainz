BEGIN;

SELECT _v.register_patch('20110803-versioning', ARRAY['20110708-initial-schema'], NULL);

DROP SCHEMA IF EXISTS bookbrainz;
DROP SCHEMA IF EXISTS bookbrainz_v;
CREATE SCHEMA bookbrainz;
CREATE SCHEMA bookbrainz_v;

ALTER TABLE author_credit           SET SCHEMA bookbrainz;
ALTER TABLE author_credit_person    SET SCHEMA bookbrainz;
ALTER TABLE country                 SET SCHEMA bookbrainz;
ALTER TABLE edition_format          SET SCHEMA bookbrainz;
ALTER TABLE language                SET SCHEMA bookbrainz;

CREATE TABLE bookbrainz_v.book (
    gid UUID NOT NULL,
    version SERIAL NOT NULL PRIMARY KEY,
    name TEXT NOT NULL,
    author_credit INT NOT NULL REFERENCES bookbrainz.author_credit (id)
);

INSERT INTO bookbrainz_v.book (gid, name, author_credit, version)
   SELECT gid, name, author_credit, id FROM public.book;

CREATE TABLE bookbrainz_v.person (
    version SERIAL NOT NULL PRIMARY KEY,
    gid UUID NOT NULL,
    name TEXT NOT NULL
);

INSERT INTO bookbrainz_v.person (gid, name, version)
   SELECT gid, name, id FROM public.person;

CREATE TABLE bookbrainz_v.publisher (
    version SERIAL NOT NULL PRIMARY KEY,
    gid UUID NOT NULL,
    name TEXT NOT NULL
);

INSERT INTO bookbrainz_v.publisher (gid, name, version)
   SELECT gid, name, id FROM public.publisher;

CREATE TABLE bookbrainz_v.edition (
    gid UUID NOT NULL,
    version SERIAL NOT NULL PRIMARY KEY,
    name TEXT NOT NULL,
    book INT NOT NULL REFERENCES bookbrainz_v.book (version),
    year INT,
    publisher INT REFERENCES bookbrainz_v.publisher (version),
    country_iso_code TEXT REFERENCES bookbrainz.country (iso_code),
    illustrator INT REFERENCES bookbrainz.author_credit (id),
    translator INT REFERENCES bookbrainz.author_credit (id),
    author INT REFERENCES bookbrainz.author_credit (id),
    language_iso_code TEXT REFERENCES bookbrainz.language (iso_code),
    isbn TEXT,
    barcode TEXT,
    edition_index INT CHECK (edition_index IS NULL OR edition_index > 0),
    format INT REFERENCES bookbrainz.edition_format (id)
);

INSERT INTO bookbrainz_v.edition
   (gid, version, name, book, year, publisher, country_iso_code, illustrator,
    translator, author, language_iso_code, isbn, barcode, edition_index, format)
   SELECT gid, id, name, book, year, publisher, country_iso_code, illustrator,
    translator, author, language_iso_code, isbn, barcode, edition_index, format
    FROM public.edition;

DROP TABLE IF EXISTS public.book CASCADE;
DROP TABLE IF EXISTS public.edition CASCADE;
DROP TABLE IF EXISTS public.person CASCADE;
DROP TABLE IF EXISTS public.publisher CASCADE;

CREATE TABLE bookbrainz_v.revision (
    rev_id SERIAL NOT NULL PRIMARY KEY,
    commited TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
);

CREATE TABLE bookbrainz_v.branch (
    master BOOL NOT NULL,
    rev_id INT NOT NULL REFERENCES bookbrainz_v.revision (rev_id),
    gid UUID NOT NULL
);

CREATE TABLE bookbrainz_v.book_revision (
    rev_id INT NOT NULL REFERENCES bookbrainz_v.revision (rev_id),
    version INT NOT NULL REFERENCES bookbrainz_v.book (version)
);

CREATE VIEW bookbrainz.book AS
SELECT book.gid, name, author_credit
FROM bookbrainz_v.branch
JOIN bookbrainz_v.book_revision USING (rev_id)
JOIN bookbrainz_v.book USING (version)
WHERE branch.master = TRUE;

DO LANGUAGE 'plpgsql' $$
DECLARE
   rev_id INT;
   book bookbrainz_v.book%rowtype;
BEGIN
   FOR book IN SELECT * FROM bookbrainz_v.book
   LOOP
       INSERT INTO bookbrainz_v.revision DEFAULT VALUES RETURNING revision.rev_id INTO rev_id;
       INSERT INTO bookbrainz_v.book_revision (rev_id, version) VALUES (rev_id, book.version);
       INSERT INTO bookbrainz_v.branch (rev_id, master, gid) VALUES (rev_id, TRUE, book.gid);
   END LOOP;
END;
$$;

CREATE TABLE bookbrainz_v.publisher_revision (
    rev_id INT NOT NULL REFERENCES bookbrainz_v.revision (rev_id),
    version INT NOT NULL REFERENCES bookbrainz_v.publisher (version)
);

CREATE VIEW bookbrainz.publisher AS
SELECT publisher.gid, name
FROM bookbrainz_v.branch
JOIN bookbrainz_v.publisher_revision USING (rev_id)
JOIN bookbrainz_v.publisher USING (version)
WHERE branch.master = TRUE;

DO LANGUAGE 'plpgsql' $$
DECLARE
   rev_id INT;
   publisher bookbrainz_v.publisher%rowtype;
BEGIN
   FOR publisher IN SELECT * FROM bookbrainz_v.publisher
   LOOP
       INSERT INTO bookbrainz_v.revision DEFAULT VALUES RETURNING revision.rev_id INTO rev_id;
       INSERT INTO bookbrainz_v.publisher_revision (rev_id, version) VALUES (rev_id, publisher.version);
       INSERT INTO bookbrainz_v.branch (rev_id, master, gid) VALUES (rev_id, TRUE, publisher.gid);
   END LOOP;
END;
$$;

CREATE TABLE bookbrainz_v.person_revision (
    rev_id INT NOT NULL REFERENCES bookbrainz_v.revision (rev_id),
    version INT NOT NULL REFERENCES bookbrainz_v.person (version)
);

CREATE VIEW bookbrainz.person AS
SELECT person.gid, name
FROM bookbrainz_v.branch
JOIN bookbrainz_v.person_revision USING (rev_id)
JOIN bookbrainz_v.person USING (version)
WHERE branch.master = TRUE;

DO LANGUAGE 'plpgsql' $$
DECLARE
   rev_id INT;
   person bookbrainz_v.person%rowtype;
BEGIN
   FOR person IN SELECT * FROM bookbrainz_v.person
   LOOP
       INSERT INTO bookbrainz_v.revision DEFAULT VALUES RETURNING revision.rev_id INTO rev_id;
       INSERT INTO bookbrainz_v.person_revision (rev_id, version) VALUES (rev_id, person.version);
       INSERT INTO bookbrainz_v.branch (rev_id, master, gid) VALUES (rev_id, TRUE, person.gid);
   END LOOP;
END;
$$;

CREATE TABLE bookbrainz_v.edition_revision (
    rev_id INT NOT NULL REFERENCES bookbrainz_v.revision (rev_id),
    version INT NOT NULL REFERENCES bookbrainz_v.edition (version)
);

CREATE VIEW bookbrainz.edition AS
SELECT edition.gid, name, book, year, publisher, country_iso_code, illustrator,
       translator, author, language_iso_code, isbn, barcode, edition_index,
       format
FROM bookbrainz_v.branch
JOIN bookbrainz_v.edition_revision USING (rev_id)
JOIN bookbrainz_v.edition USING (version)
WHERE branch.master = TRUE;

DO LANGUAGE 'plpgsql' $$
DECLARE
   rev_id INT;
   edition bookbrainz_v.edition%rowtype;
BEGIN
   FOR edition IN SELECT * FROM bookbrainz_v.edition
   LOOP
       INSERT INTO bookbrainz_v.revision DEFAULT VALUES RETURNING revision.rev_id INTO rev_id;
       INSERT INTO bookbrainz_v.edition_revision (rev_id, version) VALUES (rev_id, edition.version);
       INSERT INTO bookbrainz_v.branch (rev_id, master, gid) VALUES (rev_id, TRUE, edition.gid);
   END LOOP;
END;
$$;

COMMIT;
