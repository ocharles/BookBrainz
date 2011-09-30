--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = off;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET escape_string_warning = off;

--
-- Name: _v; Type: SCHEMA; Schema: -; Owner: bookbrainz
--

CREATE SCHEMA _v;


ALTER SCHEMA _v OWNER TO bookbrainz;

--
-- Name: SCHEMA _v; Type: COMMENT; Schema: -; Owner: bookbrainz
--

COMMENT ON SCHEMA _v IS 'Schema for versioning data and functionality.';


--
-- Name: bookbrainz; Type: SCHEMA; Schema: -; Owner: bookbrainz
--

CREATE SCHEMA bookbrainz;


ALTER SCHEMA bookbrainz OWNER TO bookbrainz;

--
-- Name: bookbrainz_v; Type: SCHEMA; Schema: -; Owner: bookbrainz
--

CREATE SCHEMA bookbrainz_v;


ALTER SCHEMA bookbrainz_v OWNER TO bookbrainz;

--
-- Name: plpgsql; Type: PROCEDURAL LANGUAGE; Schema: -; Owner: postgres
--

CREATE OR REPLACE PROCEDURAL LANGUAGE plpgsql;


ALTER PROCEDURAL LANGUAGE plpgsql OWNER TO postgres;

SET search_path = _v, pg_catalog;

--
-- Name: register_patch(text); Type: FUNCTION; Schema: _v; Owner: bookbrainz
--

CREATE FUNCTION register_patch(text) RETURNS SETOF integer
    LANGUAGE sql
    AS $_$
    SELECT _v.register_patch( $1, NULL, NULL );
$_$;


ALTER FUNCTION _v.register_patch(text) OWNER TO bookbrainz;

--
-- Name: FUNCTION register_patch(text); Type: COMMENT; Schema: _v; Owner: bookbrainz
--

COMMENT ON FUNCTION register_patch(text) IS 'Wrapper to allow registration of patches without requirements and conflicts.';


--
-- Name: register_patch(text, text[]); Type: FUNCTION; Schema: _v; Owner: bookbrainz
--

CREATE FUNCTION register_patch(text, text[]) RETURNS SETOF integer
    LANGUAGE sql
    AS $_$
    SELECT _v.register_patch( $1, $2, NULL );
$_$;


ALTER FUNCTION _v.register_patch(text, text[]) OWNER TO bookbrainz;

--
-- Name: FUNCTION register_patch(text, text[]); Type: COMMENT; Schema: _v; Owner: bookbrainz
--

COMMENT ON FUNCTION register_patch(text, text[]) IS 'Wrapper to allow registration of patches without conflicts.';


--
-- Name: register_patch(text, text[], text[]); Type: FUNCTION; Schema: _v; Owner: bookbrainz
--

CREATE FUNCTION register_patch(in_patch_name text, in_requirements text[], in_conflicts text[], OUT versioning integer) RETURNS SETOF integer
    LANGUAGE plpgsql
    AS $$
DECLARE
    t_text   TEXT;
    t_text_a TEXT[];
    i INT4;
BEGIN
    -- Thanks to this we know only one patch will be applied at a time
    LOCK TABLE _v.patches IN EXCLUSIVE MODE;

    SELECT patch_name INTO t_text FROM _v.patches WHERE patch_name = in_patch_name;
    IF FOUND THEN
        RAISE EXCEPTION 'Patch % is already applied!', in_patch_name;
    END IF;

    t_text_a := ARRAY( SELECT patch_name FROM _v.patches WHERE patch_name = any( in_conflicts ) );
    IF array_upper( t_text_a, 1 ) IS NOT NULL THEN
        RAISE EXCEPTION 'Versioning patches conflict. Conflicting patche(s) installed: %.', array_to_string( t_text_a, ', ' );
    END IF;

    IF array_upper( in_requirements, 1 ) IS NOT NULL THEN
        t_text_a := '{}';
        FOR i IN array_lower( in_requirements, 1 ) .. array_upper( in_requirements, 1 ) LOOP
            SELECT patch_name INTO t_text FROM _v.patches WHERE patch_name = in_requirements[i];
            IF NOT FOUND THEN
                t_text_a := t_text_a || t_text;
            END IF;
        END LOOP;
        IF array_upper( t_text_a, 1 ) IS NOT NULL THEN
            RAISE EXCEPTION 'Missing prerequisite(s): %.', array_to_string( t_text_a, ', ' );
        END IF;
    END IF;

    INSERT INTO _v.patches (patch_name, applied_tsz, applied_by, requires, conflicts ) VALUES ( in_patch_name, now(), current_user, coalesce( in_requirements, '{}' ), coalesce( in_conflicts, '{}' ) );
    RETURN;
END;
$$;


ALTER FUNCTION _v.register_patch(in_patch_name text, in_requirements text[], in_conflicts text[], OUT versioning integer) OWNER TO bookbrainz;

--
-- Name: FUNCTION register_patch(in_patch_name text, in_requirements text[], in_conflicts text[], OUT versioning integer); Type: COMMENT; Schema: _v; Owner: bookbrainz
--

COMMENT ON FUNCTION register_patch(in_patch_name text, in_requirements text[], in_conflicts text[], OUT versioning integer) IS 'Function to register patches in database. Raises exception if there are conflicts, prerequisites are not installed or the migration has already been installed.';


--
-- Name: unregister_patch(text); Type: FUNCTION; Schema: _v; Owner: bookbrainz
--

CREATE FUNCTION unregister_patch(in_patch_name text, OUT versioning integer) RETURNS SETOF integer
    LANGUAGE plpgsql
    AS $$
DECLARE
    i        INT4;
    t_text_a TEXT[];
BEGIN
    -- Thanks to this we know only one patch will be applied at a time
    LOCK TABLE _v.patches IN EXCLUSIVE MODE;

    t_text_a := ARRAY( SELECT patch_name FROM _v.patches WHERE in_patch_name = ANY( requires ) );
    IF array_upper( t_text_a, 1 ) IS NOT NULL THEN
        RAISE EXCEPTION 'Cannot uninstall %, as it is required by: %.', in_patch_name, array_to_string( t_text_a, ', ' );
    END IF;

    DELETE FROM _v.patches WHERE patch_name = in_patch_name;
    GET DIAGNOSTICS i = ROW_COUNT;
    IF i < 1 THEN
        RAISE EXCEPTION 'Patch % is not installed, so it can''t be uninstalled!', in_patch_name;
    END IF;

    RETURN;
END;
$$;


ALTER FUNCTION _v.unregister_patch(in_patch_name text, OUT versioning integer) OWNER TO bookbrainz;

--
-- Name: FUNCTION unregister_patch(in_patch_name text, OUT versioning integer); Type: COMMENT; Schema: _v; Owner: bookbrainz
--

COMMENT ON FUNCTION unregister_patch(in_patch_name text, OUT versioning integer) IS 'Function to unregister patches in database. Dies if the patch is not registered, or if unregistering it would break dependencies.';


SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: patches; Type: TABLE; Schema: _v; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE patches (
    patch_name text NOT NULL,
    applied_tsz timestamp with time zone DEFAULT now() NOT NULL,
    applied_by text NOT NULL,
    requires text[],
    conflicts text[]
);


ALTER TABLE _v.patches OWNER TO bookbrainz;

--
-- Name: TABLE patches; Type: COMMENT; Schema: _v; Owner: bookbrainz
--

COMMENT ON TABLE patches IS 'Contains information about what patches are currently applied on database.';


--
-- Name: COLUMN patches.patch_name; Type: COMMENT; Schema: _v; Owner: bookbrainz
--

COMMENT ON COLUMN patches.patch_name IS 'Name of patch, has to be unique for every patch.';


--
-- Name: COLUMN patches.applied_tsz; Type: COMMENT; Schema: _v; Owner: bookbrainz
--

COMMENT ON COLUMN patches.applied_tsz IS 'When the patch was applied.';


--
-- Name: COLUMN patches.applied_by; Type: COMMENT; Schema: _v; Owner: bookbrainz
--

COMMENT ON COLUMN patches.applied_by IS 'Who applied this patch (PostgreSQL username)';


--
-- Name: COLUMN patches.requires; Type: COMMENT; Schema: _v; Owner: bookbrainz
--

COMMENT ON COLUMN patches.requires IS 'List of patches that are required for given patch.';


--
-- Name: COLUMN patches.conflicts; Type: COMMENT; Schema: _v; Owner: bookbrainz
--

COMMENT ON COLUMN patches.conflicts IS 'List of patches that conflict with given patch.';


SET search_path = bookbrainz, pg_catalog;

--
-- Name: author_credit_person; Type: TABLE; Schema: bookbrainz; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE author_credit_person (
    author_credit integer NOT NULL,
    credited_name text NOT NULL,
    "position" integer DEFAULT 0 NOT NULL,
    person integer NOT NULL,
    join_phrase text DEFAULT ''::text NOT NULL
);


ALTER TABLE bookbrainz.author_credit_person OWNER TO bookbrainz;

SET search_path = bookbrainz_v, pg_catalog;

--
-- Name: book; Type: TABLE; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE book (
    book_id integer NOT NULL
);


ALTER TABLE bookbrainz_v.book OWNER TO bookbrainz;

--
-- Name: book_bbid; Type: TABLE; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE book_bbid (
    book_id integer NOT NULL,
    bbid uuid NOT NULL
);


ALTER TABLE bookbrainz_v.book_bbid OWNER TO bookbrainz;

--
-- Name: book_branch; Type: TABLE; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE book_branch (
    book_id integer NOT NULL,
    branch_id integer NOT NULL
);


ALTER TABLE bookbrainz_v.book_branch OWNER TO bookbrainz;

--
-- Name: book_revision; Type: TABLE; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE book_revision (
    rev_id integer NOT NULL,
    book_tree_id integer NOT NULL
);


ALTER TABLE bookbrainz_v.book_revision OWNER TO bookbrainz;

--
-- Name: book_tree; Type: TABLE; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE book_tree (
    book_tree_id integer NOT NULL,
    version integer NOT NULL
);


ALTER TABLE bookbrainz_v.book_tree OWNER TO bookbrainz;

--
-- Name: book_v; Type: TABLE; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE book_v (
    version integer NOT NULL,
    name text NOT NULL
);


ALTER TABLE bookbrainz_v.book_v OWNER TO bookbrainz;

--
-- Name: branch; Type: TABLE; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE branch (
    master boolean NOT NULL,
    rev_id integer NOT NULL,
    id integer NOT NULL
);


ALTER TABLE bookbrainz_v.branch OWNER TO bookbrainz;

SET search_path = bookbrainz, pg_catalog;

--
-- Name: book; Type: VIEW; Schema: bookbrainz; Owner: bookbrainz
--

CREATE VIEW book AS
    SELECT book_branch.book_id, book_bbid.bbid, book_v.name, book_revision.book_tree_id, branch.rev_id FROM ((((((bookbrainz_v.branch JOIN bookbrainz_v.book_branch ON ((book_branch.branch_id = branch.id))) JOIN bookbrainz_v.book_revision USING (rev_id)) JOIN bookbrainz_v.book USING (book_id)) JOIN bookbrainz_v.book_bbid USING (book_id)) JOIN bookbrainz_v.book_tree USING (book_tree_id)) JOIN bookbrainz_v.book_v USING (version)) WHERE (branch.master = true);


ALTER TABLE bookbrainz.book OWNER TO bookbrainz;

--
-- Name: book_person_role; Type: TABLE; Schema: bookbrainz; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE book_person_role (
    role_id integer NOT NULL,
    person_id integer NOT NULL,
    book_tree_id integer NOT NULL
);


ALTER TABLE bookbrainz.book_person_role OWNER TO bookbrainz;

--
-- Name: country; Type: TABLE; Schema: bookbrainz; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE country (
    iso_code text NOT NULL,
    name text NOT NULL
);


ALTER TABLE bookbrainz.country OWNER TO bookbrainz;

SET search_path = bookbrainz_v, pg_catalog;

--
-- Name: edition; Type: TABLE; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE edition (
    edition_id integer NOT NULL
);


ALTER TABLE bookbrainz_v.edition OWNER TO bookbrainz;

--
-- Name: edition_bbid; Type: TABLE; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE edition_bbid (
    edition_id integer NOT NULL,
    bbid uuid NOT NULL
);


ALTER TABLE bookbrainz_v.edition_bbid OWNER TO bookbrainz;

--
-- Name: edition_branch; Type: TABLE; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE edition_branch (
    edition_id integer NOT NULL,
    branch_id integer NOT NULL
);


ALTER TABLE bookbrainz_v.edition_branch OWNER TO bookbrainz;

--
-- Name: edition_revision; Type: TABLE; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE edition_revision (
    rev_id integer NOT NULL,
    edition_tree_id integer NOT NULL
);


ALTER TABLE bookbrainz_v.edition_revision OWNER TO bookbrainz;

--
-- Name: edition_tree; Type: TABLE; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE edition_tree (
    edition_tree_id integer NOT NULL,
    version integer NOT NULL,
    book_id integer NOT NULL,
    publisher_id integer
);


ALTER TABLE bookbrainz_v.edition_tree OWNER TO bookbrainz;

--
-- Name: edition_v; Type: TABLE; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE edition_v (
    version integer NOT NULL,
    name text NOT NULL,
    year integer,
    country_iso_code text,
    language_iso_code text,
    isbn text,
    barcode text,
    edition_index integer,
    format integer,
    CONSTRAINT edition_edition_index_check CHECK (((edition_index IS NULL) OR (edition_index > 0)))
);


ALTER TABLE bookbrainz_v.edition_v OWNER TO bookbrainz;

SET search_path = bookbrainz, pg_catalog;

--
-- Name: edition; Type: VIEW; Schema: bookbrainz; Owner: bookbrainz
--

CREATE VIEW edition AS
    SELECT edition_branch.edition_id, edition_bbid.bbid, edition_v.name, edition_tree.book_id, edition_v.year, edition_tree.publisher_id, edition_v.country_iso_code, edition_v.language_iso_code, edition_v.isbn, edition_v.barcode, edition_v.edition_index, edition_v.format, edition_revision.edition_tree_id, branch.rev_id FROM ((((((bookbrainz_v.branch JOIN bookbrainz_v.edition_branch ON ((edition_branch.branch_id = branch.id))) JOIN bookbrainz_v.edition_revision USING (rev_id)) JOIN bookbrainz_v.edition USING (edition_id)) JOIN bookbrainz_v.edition_bbid USING (edition_id)) JOIN bookbrainz_v.edition_tree USING (edition_tree_id)) JOIN bookbrainz_v.edition_v USING (version)) WHERE (branch.master = true);


ALTER TABLE bookbrainz.edition OWNER TO bookbrainz;

--
-- Name: edition_format; Type: TABLE; Schema: bookbrainz; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE edition_format (
    id integer NOT NULL,
    name text NOT NULL
);


ALTER TABLE bookbrainz.edition_format OWNER TO bookbrainz;

--
-- Name: edition_format_id_seq; Type: SEQUENCE; Schema: bookbrainz; Owner: bookbrainz
--

CREATE SEQUENCE edition_format_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE bookbrainz.edition_format_id_seq OWNER TO bookbrainz;

--
-- Name: edition_format_id_seq; Type: SEQUENCE OWNED BY; Schema: bookbrainz; Owner: bookbrainz
--

ALTER SEQUENCE edition_format_id_seq OWNED BY edition_format.id;


--
-- Name: edition_format_id_seq; Type: SEQUENCE SET; Schema: bookbrainz; Owner: bookbrainz
--

SELECT pg_catalog.setval('edition_format_id_seq', 1, false);


--
-- Name: edition_person_role; Type: TABLE; Schema: bookbrainz; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE edition_person_role (
    role_id integer NOT NULL,
    person_id integer NOT NULL,
    edition_tree_id integer NOT NULL
);


ALTER TABLE bookbrainz.edition_person_role OWNER TO bookbrainz;

--
-- Name: editor; Type: TABLE; Schema: bookbrainz; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE editor (
    editor_id integer NOT NULL,
    name text NOT NULL,
    password text NOT NULL
);


ALTER TABLE bookbrainz.editor OWNER TO bookbrainz;

--
-- Name: editor_editor_id_seq; Type: SEQUENCE; Schema: bookbrainz; Owner: bookbrainz
--

CREATE SEQUENCE editor_editor_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE bookbrainz.editor_editor_id_seq OWNER TO bookbrainz;

--
-- Name: editor_editor_id_seq; Type: SEQUENCE OWNED BY; Schema: bookbrainz; Owner: bookbrainz
--

ALTER SEQUENCE editor_editor_id_seq OWNED BY editor.editor_id;


--
-- Name: editor_editor_id_seq; Type: SEQUENCE SET; Schema: bookbrainz; Owner: bookbrainz
--

SELECT pg_catalog.setval('editor_editor_id_seq', 1, false);


--
-- Name: language; Type: TABLE; Schema: bookbrainz; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE language (
    iso_code text NOT NULL,
    name text NOT NULL
);


ALTER TABLE bookbrainz.language OWNER TO bookbrainz;

SET search_path = bookbrainz_v, pg_catalog;

--
-- Name: person; Type: TABLE; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE person (
    person_id integer NOT NULL
);


ALTER TABLE bookbrainz_v.person OWNER TO bookbrainz;

--
-- Name: person_bbid; Type: TABLE; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE person_bbid (
    person_id integer NOT NULL,
    bbid uuid NOT NULL
);


ALTER TABLE bookbrainz_v.person_bbid OWNER TO bookbrainz;

--
-- Name: person_branch; Type: TABLE; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE person_branch (
    person_id integer NOT NULL,
    branch_id integer NOT NULL
);


ALTER TABLE bookbrainz_v.person_branch OWNER TO bookbrainz;

--
-- Name: person_revision; Type: TABLE; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE person_revision (
    rev_id integer NOT NULL,
    person_tree_id integer NOT NULL
);


ALTER TABLE bookbrainz_v.person_revision OWNER TO bookbrainz;

--
-- Name: person_tree; Type: TABLE; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE person_tree (
    person_tree_id integer NOT NULL,
    version integer NOT NULL
);


ALTER TABLE bookbrainz_v.person_tree OWNER TO bookbrainz;

--
-- Name: person_v; Type: TABLE; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE person_v (
    version integer NOT NULL,
    name text NOT NULL
);


ALTER TABLE bookbrainz_v.person_v OWNER TO bookbrainz;

SET search_path = bookbrainz, pg_catalog;

--
-- Name: person; Type: VIEW; Schema: bookbrainz; Owner: bookbrainz
--

CREATE VIEW person AS
    SELECT person_branch.person_id, person_bbid.bbid, person_v.name, person_revision.person_tree_id, branch.rev_id FROM ((((((bookbrainz_v.branch JOIN bookbrainz_v.person_branch ON ((person_branch.branch_id = branch.id))) JOIN bookbrainz_v.person_revision USING (rev_id)) JOIN bookbrainz_v.person USING (person_id)) JOIN bookbrainz_v.person_bbid USING (person_id)) JOIN bookbrainz_v.person_tree USING (person_tree_id)) JOIN bookbrainz_v.person_v USING (version)) WHERE (branch.master = true);


ALTER TABLE bookbrainz.person OWNER TO bookbrainz;

--
-- Name: person_role; Type: TABLE; Schema: bookbrainz; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE person_role (
    role_id integer NOT NULL,
    name text NOT NULL
);


ALTER TABLE bookbrainz.person_role OWNER TO bookbrainz;

--
-- Name: person_role_role_id_seq; Type: SEQUENCE; Schema: bookbrainz; Owner: bookbrainz
--

CREATE SEQUENCE person_role_role_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE bookbrainz.person_role_role_id_seq OWNER TO bookbrainz;

--
-- Name: person_role_role_id_seq; Type: SEQUENCE OWNED BY; Schema: bookbrainz; Owner: bookbrainz
--

ALTER SEQUENCE person_role_role_id_seq OWNED BY person_role.role_id;


--
-- Name: person_role_role_id_seq; Type: SEQUENCE SET; Schema: bookbrainz; Owner: bookbrainz
--

SELECT pg_catalog.setval('person_role_role_id_seq', 3, false);


SET search_path = bookbrainz_v, pg_catalog;

--
-- Name: publisher; Type: TABLE; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE publisher (
    publisher_id integer NOT NULL
);


ALTER TABLE bookbrainz_v.publisher OWNER TO bookbrainz;

--
-- Name: publisher_bbid; Type: TABLE; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE publisher_bbid (
    publisher_id integer NOT NULL,
    bbid uuid NOT NULL
);


ALTER TABLE bookbrainz_v.publisher_bbid OWNER TO bookbrainz;

--
-- Name: publisher_branch; Type: TABLE; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE publisher_branch (
    publisher_id integer NOT NULL,
    branch_id integer NOT NULL
);


ALTER TABLE bookbrainz_v.publisher_branch OWNER TO bookbrainz;

--
-- Name: publisher_revision; Type: TABLE; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE publisher_revision (
    rev_id integer NOT NULL,
    publisher_tree_id integer NOT NULL
);


ALTER TABLE bookbrainz_v.publisher_revision OWNER TO bookbrainz;

--
-- Name: publisher_tree; Type: TABLE; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE publisher_tree (
    publisher_tree_id integer NOT NULL,
    version integer NOT NULL
);


ALTER TABLE bookbrainz_v.publisher_tree OWNER TO bookbrainz;

--
-- Name: publisher_v; Type: TABLE; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE publisher_v (
    version integer NOT NULL,
    name text NOT NULL
);


ALTER TABLE bookbrainz_v.publisher_v OWNER TO bookbrainz;

SET search_path = bookbrainz, pg_catalog;

--
-- Name: publisher; Type: VIEW; Schema: bookbrainz; Owner: bookbrainz
--

CREATE VIEW publisher AS
    SELECT publisher_branch.publisher_id, publisher_bbid.bbid, publisher_v.name, publisher_revision.publisher_tree_id, branch.rev_id FROM ((((((bookbrainz_v.branch JOIN bookbrainz_v.publisher_branch ON ((publisher_branch.branch_id = branch.id))) JOIN bookbrainz_v.publisher_revision USING (rev_id)) JOIN bookbrainz_v.publisher USING (publisher_id)) JOIN bookbrainz_v.publisher_bbid USING (publisher_id)) JOIN bookbrainz_v.publisher_tree USING (publisher_tree_id)) JOIN bookbrainz_v.publisher_v USING (version)) WHERE (branch.master = true);


ALTER TABLE bookbrainz.publisher OWNER TO bookbrainz;

SET search_path = bookbrainz_v, pg_catalog;

--
-- Name: bbid; Type: TABLE; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE bbid (
    bbid uuid NOT NULL
);


ALTER TABLE bookbrainz_v.bbid OWNER TO bookbrainz;

--
-- Name: book_book_id_seq; Type: SEQUENCE; Schema: bookbrainz_v; Owner: bookbrainz
--

CREATE SEQUENCE book_book_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE bookbrainz_v.book_book_id_seq OWNER TO bookbrainz;

--
-- Name: book_book_id_seq; Type: SEQUENCE OWNED BY; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER SEQUENCE book_book_id_seq OWNED BY book.book_id;


--
-- Name: book_book_id_seq; Type: SEQUENCE SET; Schema: bookbrainz_v; Owner: bookbrainz
--

SELECT pg_catalog.setval('book_book_id_seq', 1, false);


--
-- Name: book_tree_book_tree_id_seq; Type: SEQUENCE; Schema: bookbrainz_v; Owner: bookbrainz
--

CREATE SEQUENCE book_tree_book_tree_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE bookbrainz_v.book_tree_book_tree_id_seq OWNER TO bookbrainz;

--
-- Name: book_tree_book_tree_id_seq; Type: SEQUENCE OWNED BY; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER SEQUENCE book_tree_book_tree_id_seq OWNED BY book_tree.book_tree_id;


--
-- Name: book_tree_book_tree_id_seq; Type: SEQUENCE SET; Schema: bookbrainz_v; Owner: bookbrainz
--

SELECT pg_catalog.setval('book_tree_book_tree_id_seq', 1, false);


--
-- Name: book_version_seq; Type: SEQUENCE; Schema: bookbrainz_v; Owner: bookbrainz
--

CREATE SEQUENCE book_version_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE bookbrainz_v.book_version_seq OWNER TO bookbrainz;

--
-- Name: book_version_seq; Type: SEQUENCE OWNED BY; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER SEQUENCE book_version_seq OWNED BY book_v.version;


--
-- Name: book_version_seq; Type: SEQUENCE SET; Schema: bookbrainz_v; Owner: bookbrainz
--

SELECT pg_catalog.setval('book_version_seq', 1, false);


--
-- Name: branch_id_seq; Type: SEQUENCE; Schema: bookbrainz_v; Owner: bookbrainz
--

CREATE SEQUENCE branch_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE bookbrainz_v.branch_id_seq OWNER TO bookbrainz;

--
-- Name: branch_id_seq; Type: SEQUENCE OWNED BY; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER SEQUENCE branch_id_seq OWNED BY branch.id;


--
-- Name: branch_id_seq; Type: SEQUENCE SET; Schema: bookbrainz_v; Owner: bookbrainz
--

SELECT pg_catalog.setval('branch_id_seq', 1, false);


--
-- Name: edition_edition_id_seq; Type: SEQUENCE; Schema: bookbrainz_v; Owner: bookbrainz
--

CREATE SEQUENCE edition_edition_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE bookbrainz_v.edition_edition_id_seq OWNER TO bookbrainz;

--
-- Name: edition_edition_id_seq; Type: SEQUENCE OWNED BY; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER SEQUENCE edition_edition_id_seq OWNED BY edition.edition_id;


--
-- Name: edition_edition_id_seq; Type: SEQUENCE SET; Schema: bookbrainz_v; Owner: bookbrainz
--

SELECT pg_catalog.setval('edition_edition_id_seq', 1, false);


--
-- Name: edition_tree_edition_tree_id_seq; Type: SEQUENCE; Schema: bookbrainz_v; Owner: bookbrainz
--

CREATE SEQUENCE edition_tree_edition_tree_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE bookbrainz_v.edition_tree_edition_tree_id_seq OWNER TO bookbrainz;

--
-- Name: edition_tree_edition_tree_id_seq; Type: SEQUENCE OWNED BY; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER SEQUENCE edition_tree_edition_tree_id_seq OWNED BY edition_tree.edition_tree_id;


--
-- Name: edition_tree_edition_tree_id_seq; Type: SEQUENCE SET; Schema: bookbrainz_v; Owner: bookbrainz
--

SELECT pg_catalog.setval('edition_tree_edition_tree_id_seq', 1, false);


--
-- Name: edition_version_seq; Type: SEQUENCE; Schema: bookbrainz_v; Owner: bookbrainz
--

CREATE SEQUENCE edition_version_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE bookbrainz_v.edition_version_seq OWNER TO bookbrainz;

--
-- Name: edition_version_seq; Type: SEQUENCE OWNED BY; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER SEQUENCE edition_version_seq OWNED BY edition_v.version;


--
-- Name: edition_version_seq; Type: SEQUENCE SET; Schema: bookbrainz_v; Owner: bookbrainz
--

SELECT pg_catalog.setval('edition_version_seq', 1, false);


--
-- Name: person_person_id_seq; Type: SEQUENCE; Schema: bookbrainz_v; Owner: bookbrainz
--

CREATE SEQUENCE person_person_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE bookbrainz_v.person_person_id_seq OWNER TO bookbrainz;

--
-- Name: person_person_id_seq; Type: SEQUENCE OWNED BY; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER SEQUENCE person_person_id_seq OWNED BY person.person_id;


--
-- Name: person_person_id_seq; Type: SEQUENCE SET; Schema: bookbrainz_v; Owner: bookbrainz
--

SELECT pg_catalog.setval('person_person_id_seq', 1, false);


--
-- Name: person_tree_person_tree_id_seq; Type: SEQUENCE; Schema: bookbrainz_v; Owner: bookbrainz
--

CREATE SEQUENCE person_tree_person_tree_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE bookbrainz_v.person_tree_person_tree_id_seq OWNER TO bookbrainz;

--
-- Name: person_tree_person_tree_id_seq; Type: SEQUENCE OWNED BY; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER SEQUENCE person_tree_person_tree_id_seq OWNED BY person_tree.person_tree_id;


--
-- Name: person_tree_person_tree_id_seq; Type: SEQUENCE SET; Schema: bookbrainz_v; Owner: bookbrainz
--

SELECT pg_catalog.setval('person_tree_person_tree_id_seq', 1, false);


--
-- Name: person_version_seq; Type: SEQUENCE; Schema: bookbrainz_v; Owner: bookbrainz
--

CREATE SEQUENCE person_version_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE bookbrainz_v.person_version_seq OWNER TO bookbrainz;

--
-- Name: person_version_seq; Type: SEQUENCE OWNED BY; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER SEQUENCE person_version_seq OWNED BY person_v.version;


--
-- Name: person_version_seq; Type: SEQUENCE SET; Schema: bookbrainz_v; Owner: bookbrainz
--

SELECT pg_catalog.setval('person_version_seq', 1, false);


--
-- Name: publisher_publisher_id_seq; Type: SEQUENCE; Schema: bookbrainz_v; Owner: bookbrainz
--

CREATE SEQUENCE publisher_publisher_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE bookbrainz_v.publisher_publisher_id_seq OWNER TO bookbrainz;

--
-- Name: publisher_publisher_id_seq; Type: SEQUENCE OWNED BY; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER SEQUENCE publisher_publisher_id_seq OWNED BY publisher.publisher_id;


--
-- Name: publisher_publisher_id_seq; Type: SEQUENCE SET; Schema: bookbrainz_v; Owner: bookbrainz
--

SELECT pg_catalog.setval('publisher_publisher_id_seq', 1, false);


--
-- Name: publisher_tree_publisher_tree_id_seq; Type: SEQUENCE; Schema: bookbrainz_v; Owner: bookbrainz
--

CREATE SEQUENCE publisher_tree_publisher_tree_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE bookbrainz_v.publisher_tree_publisher_tree_id_seq OWNER TO bookbrainz;

--
-- Name: publisher_tree_publisher_tree_id_seq; Type: SEQUENCE OWNED BY; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER SEQUENCE publisher_tree_publisher_tree_id_seq OWNED BY publisher_tree.publisher_tree_id;


--
-- Name: publisher_tree_publisher_tree_id_seq; Type: SEQUENCE SET; Schema: bookbrainz_v; Owner: bookbrainz
--

SELECT pg_catalog.setval('publisher_tree_publisher_tree_id_seq', 1, false);


--
-- Name: publisher_version_seq; Type: SEQUENCE; Schema: bookbrainz_v; Owner: bookbrainz
--

CREATE SEQUENCE publisher_version_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE bookbrainz_v.publisher_version_seq OWNER TO bookbrainz;

--
-- Name: publisher_version_seq; Type: SEQUENCE OWNED BY; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER SEQUENCE publisher_version_seq OWNED BY publisher_v.version;


--
-- Name: publisher_version_seq; Type: SEQUENCE SET; Schema: bookbrainz_v; Owner: bookbrainz
--

SELECT pg_catalog.setval('publisher_version_seq', 1, false);


--
-- Name: revision; Type: TABLE; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE revision (
    rev_id integer NOT NULL,
    commited timestamp with time zone DEFAULT now() NOT NULL,
    editor integer NOT NULL
);


ALTER TABLE bookbrainz_v.revision OWNER TO bookbrainz;

--
-- Name: revision_parent; Type: TABLE; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE TABLE revision_parent (
    rev_id integer NOT NULL,
    parent_id integer NOT NULL
);


ALTER TABLE bookbrainz_v.revision_parent OWNER TO bookbrainz;

--
-- Name: revision_rev_id_seq; Type: SEQUENCE; Schema: bookbrainz_v; Owner: bookbrainz
--

CREATE SEQUENCE revision_rev_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE bookbrainz_v.revision_rev_id_seq OWNER TO bookbrainz;

--
-- Name: revision_rev_id_seq; Type: SEQUENCE OWNED BY; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER SEQUENCE revision_rev_id_seq OWNED BY revision.rev_id;


--
-- Name: revision_rev_id_seq; Type: SEQUENCE SET; Schema: bookbrainz_v; Owner: bookbrainz
--

SELECT pg_catalog.setval('revision_rev_id_seq', 1, false);


SET search_path = bookbrainz, pg_catalog;

--
-- Name: id; Type: DEFAULT; Schema: bookbrainz; Owner: bookbrainz
--

ALTER TABLE edition_format ALTER COLUMN id SET DEFAULT nextval('edition_format_id_seq'::regclass);


--
-- Name: editor_id; Type: DEFAULT; Schema: bookbrainz; Owner: bookbrainz
--

ALTER TABLE editor ALTER COLUMN editor_id SET DEFAULT nextval('editor_editor_id_seq'::regclass);


--
-- Name: role_id; Type: DEFAULT; Schema: bookbrainz; Owner: bookbrainz
--

ALTER TABLE person_role ALTER COLUMN role_id SET DEFAULT nextval('person_role_role_id_seq'::regclass);


SET search_path = bookbrainz_v, pg_catalog;

--
-- Name: book_id; Type: DEFAULT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE book ALTER COLUMN book_id SET DEFAULT nextval('book_book_id_seq'::regclass);


--
-- Name: book_tree_id; Type: DEFAULT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE book_tree ALTER COLUMN book_tree_id SET DEFAULT nextval('book_tree_book_tree_id_seq'::regclass);


--
-- Name: version; Type: DEFAULT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE book_v ALTER COLUMN version SET DEFAULT nextval('book_version_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE branch ALTER COLUMN id SET DEFAULT nextval('branch_id_seq'::regclass);


--
-- Name: edition_id; Type: DEFAULT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE edition ALTER COLUMN edition_id SET DEFAULT nextval('edition_edition_id_seq'::regclass);


--
-- Name: edition_tree_id; Type: DEFAULT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE edition_tree ALTER COLUMN edition_tree_id SET DEFAULT nextval('edition_tree_edition_tree_id_seq'::regclass);


--
-- Name: version; Type: DEFAULT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE edition_v ALTER COLUMN version SET DEFAULT nextval('edition_version_seq'::regclass);


--
-- Name: person_id; Type: DEFAULT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE person ALTER COLUMN person_id SET DEFAULT nextval('person_person_id_seq'::regclass);


--
-- Name: person_tree_id; Type: DEFAULT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE person_tree ALTER COLUMN person_tree_id SET DEFAULT nextval('person_tree_person_tree_id_seq'::regclass);


--
-- Name: version; Type: DEFAULT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE person_v ALTER COLUMN version SET DEFAULT nextval('person_version_seq'::regclass);


--
-- Name: publisher_id; Type: DEFAULT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE publisher ALTER COLUMN publisher_id SET DEFAULT nextval('publisher_publisher_id_seq'::regclass);


--
-- Name: publisher_tree_id; Type: DEFAULT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE publisher_tree ALTER COLUMN publisher_tree_id SET DEFAULT nextval('publisher_tree_publisher_tree_id_seq'::regclass);


--
-- Name: version; Type: DEFAULT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE publisher_v ALTER COLUMN version SET DEFAULT nextval('publisher_version_seq'::regclass);


--
-- Name: rev_id; Type: DEFAULT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE revision ALTER COLUMN rev_id SET DEFAULT nextval('revision_rev_id_seq'::regclass);


SET search_path = _v, pg_catalog;

--
-- Data for Name: patches; Type: TABLE DATA; Schema: _v; Owner: bookbrainz
--

COPY patches (patch_name, applied_tsz, applied_by, requires, conflicts) FROM stdin;
2011-07-08	2011-09-30 21:38:09.092003+01	bookbrainz	{}	{}
20110708-initial-schema	2011-09-30 21:38:12.884031+01	bookbrainz	{2011-07-08}	{}
20110803-versioning	2011-09-30 21:38:18.503687+01	bookbrainz	{20110708-initial-schema}	{}
20110804-roles	2011-09-30 21:38:22.66255+01	bookbrainz	{20110803-versioning}	{}
20110804-versions	2011-09-30 21:38:24.668134+01	bookbrainz	{20110804-roles}	{}
20110827-revision-parent	2011-09-30 21:38:28.579972+01	bookbrainz	{20110804-versions}	{}
20110827-revision	2011-09-30 21:38:34.355979+01	bookbrainz	{20110804-versions}	{}
20110917-redirect	2011-09-30 21:38:44.05218+01	bookbrainz	{20110804-versions}	{}
20110917-general-references	2011-09-30 21:38:48.078482+01	bookbrainz	{20110917-redirect}	{}
20110918-trees	2011-09-30 21:39:05.643946+01	bookbrainz	{20110917-general-references}	{}
20110918-pks	2011-09-30 21:39:12.211949+01	bookbrainz	{20110918-trees}	{}
20110918-editor	2011-09-30 21:39:15.019962+01	bookbrainz	{20110918-pks}	{}
20110919-tree-in-view	2011-09-30 21:39:23.158589+01	bookbrainz	{20110918-trees}	{}
20110927-unique-bbid	2011-09-30 21:39:27.98022+01	bookbrainz	{20110919-tree-in-view}	{}
\.


SET search_path = bookbrainz, pg_catalog;

--
-- Data for Name: author_credit_person; Type: TABLE DATA; Schema: bookbrainz; Owner: bookbrainz
--

COPY author_credit_person (author_credit, credited_name, "position", person, join_phrase) FROM stdin;
\.


--
-- Data for Name: book_person_role; Type: TABLE DATA; Schema: bookbrainz; Owner: bookbrainz
--

COPY book_person_role (role_id, person_id, book_tree_id) FROM stdin;
\.


--
-- Data for Name: country; Type: TABLE DATA; Schema: bookbrainz; Owner: bookbrainz
--

COPY country (iso_code, name) FROM stdin;
\.


--
-- Data for Name: edition_format; Type: TABLE DATA; Schema: bookbrainz; Owner: bookbrainz
--

COPY edition_format (id, name) FROM stdin;
\.


--
-- Data for Name: edition_person_role; Type: TABLE DATA; Schema: bookbrainz; Owner: bookbrainz
--

COPY edition_person_role (role_id, person_id, edition_tree_id) FROM stdin;
\.


--
-- Data for Name: editor; Type: TABLE DATA; Schema: bookbrainz; Owner: bookbrainz
--

COPY editor (editor_id, name, password) FROM stdin;
1	BookBrainz	
\.


--
-- Data for Name: language; Type: TABLE DATA; Schema: bookbrainz; Owner: bookbrainz
--

COPY language (iso_code, name) FROM stdin;
\.


--
-- Data for Name: person_role; Type: TABLE DATA; Schema: bookbrainz; Owner: bookbrainz
--

COPY person_role (role_id, name) FROM stdin;
1	Author
2	Foreword
3	Illustrator
4	Translator
\.


SET search_path = bookbrainz_v, pg_catalog;

--
-- Data for Name: bbid; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY bbid (bbid) FROM stdin;
\.


--
-- Data for Name: book; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY book (book_id) FROM stdin;
\.


--
-- Data for Name: book_bbid; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY book_bbid (book_id, bbid) FROM stdin;
\.


--
-- Data for Name: book_branch; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY book_branch (book_id, branch_id) FROM stdin;
\.


--
-- Data for Name: book_revision; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY book_revision (rev_id, book_tree_id) FROM stdin;
\.


--
-- Data for Name: book_tree; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY book_tree (book_tree_id, version) FROM stdin;
\.


--
-- Data for Name: book_v; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY book_v (version, name) FROM stdin;
\.


--
-- Data for Name: branch; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY branch (master, rev_id, id) FROM stdin;
\.


--
-- Data for Name: edition; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY edition (edition_id) FROM stdin;
\.


--
-- Data for Name: edition_bbid; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY edition_bbid (edition_id, bbid) FROM stdin;
\.


--
-- Data for Name: edition_branch; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY edition_branch (edition_id, branch_id) FROM stdin;
\.


--
-- Data for Name: edition_revision; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY edition_revision (rev_id, edition_tree_id) FROM stdin;
\.


--
-- Data for Name: edition_tree; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY edition_tree (edition_tree_id, version, book_id, publisher_id) FROM stdin;
\.


--
-- Data for Name: edition_v; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY edition_v (version, name, year, country_iso_code, language_iso_code, isbn, barcode, edition_index, format) FROM stdin;
\.


--
-- Data for Name: person; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY person (person_id) FROM stdin;
\.


--
-- Data for Name: person_bbid; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY person_bbid (person_id, bbid) FROM stdin;
\.


--
-- Data for Name: person_branch; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY person_branch (person_id, branch_id) FROM stdin;
\.


--
-- Data for Name: person_revision; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY person_revision (rev_id, person_tree_id) FROM stdin;
\.


--
-- Data for Name: person_tree; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY person_tree (person_tree_id, version) FROM stdin;
\.


--
-- Data for Name: person_v; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY person_v (version, name) FROM stdin;
\.


--
-- Data for Name: publisher; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY publisher (publisher_id) FROM stdin;
\.


--
-- Data for Name: publisher_bbid; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY publisher_bbid (publisher_id, bbid) FROM stdin;
\.


--
-- Data for Name: publisher_branch; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY publisher_branch (publisher_id, branch_id) FROM stdin;
\.


--
-- Data for Name: publisher_revision; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY publisher_revision (rev_id, publisher_tree_id) FROM stdin;
\.


--
-- Data for Name: publisher_tree; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY publisher_tree (publisher_tree_id, version) FROM stdin;
\.


--
-- Data for Name: publisher_v; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY publisher_v (version, name) FROM stdin;
\.


--
-- Data for Name: revision; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY revision (rev_id, commited, editor) FROM stdin;
\.


--
-- Data for Name: revision_parent; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY revision_parent (rev_id, parent_id) FROM stdin;
\.


SET search_path = _v, pg_catalog;

--
-- Name: patches_pkey; Type: CONSTRAINT; Schema: _v; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY patches
    ADD CONSTRAINT patches_pkey PRIMARY KEY (patch_name);


SET search_path = bookbrainz, pg_catalog;

--
-- Name: author_credit_person_pkey; Type: CONSTRAINT; Schema: bookbrainz; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY author_credit_person
    ADD CONSTRAINT author_credit_person_pkey PRIMARY KEY (author_credit, "position");


--
-- Name: book_person_role_pkey; Type: CONSTRAINT; Schema: bookbrainz; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY book_person_role
    ADD CONSTRAINT book_person_role_pkey PRIMARY KEY (role_id, person_id, book_tree_id);


--
-- Name: country_pkey; Type: CONSTRAINT; Schema: bookbrainz; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY country
    ADD CONSTRAINT country_pkey PRIMARY KEY (iso_code);


--
-- Name: edition_format_pkey; Type: CONSTRAINT; Schema: bookbrainz; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY edition_format
    ADD CONSTRAINT edition_format_pkey PRIMARY KEY (id);


--
-- Name: edition_person_role_pkey; Type: CONSTRAINT; Schema: bookbrainz; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY edition_person_role
    ADD CONSTRAINT edition_person_role_pkey PRIMARY KEY (role_id, person_id, edition_tree_id);


--
-- Name: editor_pkey; Type: CONSTRAINT; Schema: bookbrainz; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY editor
    ADD CONSTRAINT editor_pkey PRIMARY KEY (editor_id);


--
-- Name: language_pkey; Type: CONSTRAINT; Schema: bookbrainz; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY language
    ADD CONSTRAINT language_pkey PRIMARY KEY (iso_code);


--
-- Name: person_role_pkey; Type: CONSTRAINT; Schema: bookbrainz; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY person_role
    ADD CONSTRAINT person_role_pkey PRIMARY KEY (role_id);


SET search_path = bookbrainz_v, pg_catalog;

--
-- Name: bbid_pkey; Type: CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY bbid
    ADD CONSTRAINT bbid_pkey PRIMARY KEY (bbid);


--
-- Name: book_branch_pkey; Type: CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY book_branch
    ADD CONSTRAINT book_branch_pkey PRIMARY KEY (book_id, branch_id);


--
-- Name: book_gid_pkey; Type: CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY book_bbid
    ADD CONSTRAINT book_gid_pkey PRIMARY KEY (bbid);


--
-- Name: book_pkey; Type: CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY book_v
    ADD CONSTRAINT book_pkey PRIMARY KEY (version);


--
-- Name: book_pkey1; Type: CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY book
    ADD CONSTRAINT book_pkey1 PRIMARY KEY (book_id);


--
-- Name: book_revision_pkey; Type: CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY book_revision
    ADD CONSTRAINT book_revision_pkey PRIMARY KEY (rev_id);


--
-- Name: book_tree_pkey; Type: CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY book_tree
    ADD CONSTRAINT book_tree_pkey PRIMARY KEY (book_tree_id);


--
-- Name: branch_pkey; Type: CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY branch
    ADD CONSTRAINT branch_pkey PRIMARY KEY (id);


--
-- Name: edition_branch_pkey; Type: CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY edition_branch
    ADD CONSTRAINT edition_branch_pkey PRIMARY KEY (edition_id, branch_id);


--
-- Name: edition_gid_pkey; Type: CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY edition_bbid
    ADD CONSTRAINT edition_gid_pkey PRIMARY KEY (bbid);


--
-- Name: edition_pkey; Type: CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY edition_v
    ADD CONSTRAINT edition_pkey PRIMARY KEY (version);


--
-- Name: edition_pkey1; Type: CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY edition
    ADD CONSTRAINT edition_pkey1 PRIMARY KEY (edition_id);


--
-- Name: edition_revision_pkey; Type: CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY edition_revision
    ADD CONSTRAINT edition_revision_pkey PRIMARY KEY (rev_id);


--
-- Name: edition_tree_pkey; Type: CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY edition_tree
    ADD CONSTRAINT edition_tree_pkey PRIMARY KEY (edition_tree_id);


--
-- Name: person_branch_pkey; Type: CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY person_branch
    ADD CONSTRAINT person_branch_pkey PRIMARY KEY (person_id, branch_id);


--
-- Name: person_gid_pkey; Type: CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY person_bbid
    ADD CONSTRAINT person_gid_pkey PRIMARY KEY (bbid);


--
-- Name: person_pkey; Type: CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY person_v
    ADD CONSTRAINT person_pkey PRIMARY KEY (version);


--
-- Name: person_pkey1; Type: CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY person
    ADD CONSTRAINT person_pkey1 PRIMARY KEY (person_id);


--
-- Name: person_tree_pkey; Type: CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY person_tree
    ADD CONSTRAINT person_tree_pkey PRIMARY KEY (person_tree_id);


--
-- Name: publisher_branch_pkey; Type: CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY publisher_branch
    ADD CONSTRAINT publisher_branch_pkey PRIMARY KEY (publisher_id, branch_id);


--
-- Name: publisher_gid_pkey; Type: CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY publisher_bbid
    ADD CONSTRAINT publisher_gid_pkey PRIMARY KEY (bbid);


--
-- Name: publisher_pkey; Type: CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY publisher_v
    ADD CONSTRAINT publisher_pkey PRIMARY KEY (version);


--
-- Name: publisher_pkey1; Type: CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY publisher
    ADD CONSTRAINT publisher_pkey1 PRIMARY KEY (publisher_id);


--
-- Name: publisher_tree_pkey; Type: CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY publisher_tree
    ADD CONSTRAINT publisher_tree_pkey PRIMARY KEY (publisher_tree_id);


--
-- Name: revision_pkey; Type: CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

ALTER TABLE ONLY revision
    ADD CONSTRAINT revision_pkey PRIMARY KEY (rev_id);


SET search_path = bookbrainz, pg_catalog;

--
-- Name: editor_name_idx; Type: INDEX; Schema: bookbrainz; Owner: bookbrainz; Tablespace: 
--

CREATE UNIQUE INDEX editor_name_idx ON editor USING btree (name);


SET search_path = bookbrainz_v, pg_catalog;

--
-- Name: book_branch_book_id_idx; Type: INDEX; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE INDEX book_branch_book_id_idx ON book_branch USING btree (book_id);


--
-- Name: book_branch_branch_id_idx; Type: INDEX; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE INDEX book_branch_branch_id_idx ON book_branch USING btree (branch_id);


--
-- Name: book_v_name_idx; Type: INDEX; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE UNIQUE INDEX book_v_name_idx ON book_v USING btree (name);


--
-- Name: edition_branch_branch_id_idx; Type: INDEX; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE INDEX edition_branch_branch_id_idx ON edition_branch USING btree (branch_id);


--
-- Name: edition_branch_edition_id_idx; Type: INDEX; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE INDEX edition_branch_edition_id_idx ON edition_branch USING btree (edition_id);


--
-- Name: person_branch_branch_id_idx; Type: INDEX; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE INDEX person_branch_branch_id_idx ON person_branch USING btree (branch_id);


--
-- Name: person_branch_person_id_idx; Type: INDEX; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE INDEX person_branch_person_id_idx ON person_branch USING btree (person_id);


--
-- Name: publisher_branch_branch_id_idx; Type: INDEX; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE INDEX publisher_branch_branch_id_idx ON publisher_branch USING btree (branch_id);


--
-- Name: publisher_branch_publisher_id_idx; Type: INDEX; Schema: bookbrainz_v; Owner: bookbrainz; Tablespace: 
--

CREATE INDEX publisher_branch_publisher_id_idx ON publisher_branch USING btree (publisher_id);


SET search_path = bookbrainz, pg_catalog;

--
-- Name: book_person_role_book_tree_id_fkey; Type: FK CONSTRAINT; Schema: bookbrainz; Owner: bookbrainz
--

ALTER TABLE ONLY book_person_role
    ADD CONSTRAINT book_person_role_book_tree_id_fkey FOREIGN KEY (book_tree_id) REFERENCES bookbrainz_v.book_tree(book_tree_id);


--
-- Name: book_person_role_person_id_fkey; Type: FK CONSTRAINT; Schema: bookbrainz; Owner: bookbrainz
--

ALTER TABLE ONLY book_person_role
    ADD CONSTRAINT book_person_role_person_id_fkey FOREIGN KEY (person_id) REFERENCES bookbrainz_v.person(person_id);


--
-- Name: book_person_role_role_id_fkey; Type: FK CONSTRAINT; Schema: bookbrainz; Owner: bookbrainz
--

ALTER TABLE ONLY book_person_role
    ADD CONSTRAINT book_person_role_role_id_fkey FOREIGN KEY (role_id) REFERENCES person_role(role_id);


--
-- Name: edition_person_role_edition_tree_id_fkey; Type: FK CONSTRAINT; Schema: bookbrainz; Owner: bookbrainz
--

ALTER TABLE ONLY edition_person_role
    ADD CONSTRAINT edition_person_role_edition_tree_id_fkey FOREIGN KEY (edition_tree_id) REFERENCES bookbrainz_v.edition_tree(edition_tree_id);


--
-- Name: edition_person_role_person_id_fkey; Type: FK CONSTRAINT; Schema: bookbrainz; Owner: bookbrainz
--

ALTER TABLE ONLY edition_person_role
    ADD CONSTRAINT edition_person_role_person_id_fkey FOREIGN KEY (person_id) REFERENCES bookbrainz_v.person(person_id);


--
-- Name: edition_person_role_role_id_fkey; Type: FK CONSTRAINT; Schema: bookbrainz; Owner: bookbrainz
--

ALTER TABLE ONLY edition_person_role
    ADD CONSTRAINT edition_person_role_role_id_fkey FOREIGN KEY (role_id) REFERENCES person_role(role_id);


SET search_path = bookbrainz_v, pg_catalog;

--
-- Name: book_branch_book_id_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY book_branch
    ADD CONSTRAINT book_branch_book_id_fkey FOREIGN KEY (book_id) REFERENCES book(book_id);


--
-- Name: book_branch_branch_id_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY book_branch
    ADD CONSTRAINT book_branch_branch_id_fkey FOREIGN KEY (branch_id) REFERENCES branch(id);


--
-- Name: book_gid_bbid; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY book_bbid
    ADD CONSTRAINT book_gid_bbid FOREIGN KEY (bbid) REFERENCES bbid(bbid);


--
-- Name: book_gid_bbid; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY edition_bbid
    ADD CONSTRAINT book_gid_bbid FOREIGN KEY (bbid) REFERENCES bbid(bbid);


--
-- Name: book_gid_bbid; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY person_bbid
    ADD CONSTRAINT book_gid_bbid FOREIGN KEY (bbid) REFERENCES bbid(bbid);


--
-- Name: book_gid_bbid; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY publisher_bbid
    ADD CONSTRAINT book_gid_bbid FOREIGN KEY (bbid) REFERENCES bbid(bbid);


--
-- Name: book_gid_book_id_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY book_bbid
    ADD CONSTRAINT book_gid_book_id_fkey FOREIGN KEY (book_id) REFERENCES book(book_id);


--
-- Name: book_revision_book_tree_id_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY book_revision
    ADD CONSTRAINT book_revision_book_tree_id_fkey FOREIGN KEY (book_tree_id) REFERENCES book_tree(book_tree_id);


--
-- Name: book_revision_rev_id_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY book_revision
    ADD CONSTRAINT book_revision_rev_id_fkey FOREIGN KEY (rev_id) REFERENCES revision(rev_id);


--
-- Name: book_tree_version_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY book_tree
    ADD CONSTRAINT book_tree_version_fkey FOREIGN KEY (version) REFERENCES book_v(version);


--
-- Name: branch_rev_id_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY branch
    ADD CONSTRAINT branch_rev_id_fkey FOREIGN KEY (rev_id) REFERENCES revision(rev_id);


--
-- Name: edition_branch_branch_id_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY edition_branch
    ADD CONSTRAINT edition_branch_branch_id_fkey FOREIGN KEY (branch_id) REFERENCES branch(id);


--
-- Name: edition_branch_edition_id_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY edition_branch
    ADD CONSTRAINT edition_branch_edition_id_fkey FOREIGN KEY (edition_id) REFERENCES edition(edition_id);


--
-- Name: edition_country_iso_code_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY edition_v
    ADD CONSTRAINT edition_country_iso_code_fkey FOREIGN KEY (country_iso_code) REFERENCES bookbrainz.country(iso_code);


--
-- Name: edition_format_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY edition_v
    ADD CONSTRAINT edition_format_fkey FOREIGN KEY (format) REFERENCES bookbrainz.edition_format(id);


--
-- Name: edition_gid_edition_id_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY edition_bbid
    ADD CONSTRAINT edition_gid_edition_id_fkey FOREIGN KEY (edition_id) REFERENCES edition(edition_id);


--
-- Name: edition_language_iso_code_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY edition_v
    ADD CONSTRAINT edition_language_iso_code_fkey FOREIGN KEY (language_iso_code) REFERENCES bookbrainz.language(iso_code);


--
-- Name: edition_revision_edition_tree_id_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY edition_revision
    ADD CONSTRAINT edition_revision_edition_tree_id_fkey FOREIGN KEY (edition_tree_id) REFERENCES edition_tree(edition_tree_id);


--
-- Name: edition_revision_rev_id_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY edition_revision
    ADD CONSTRAINT edition_revision_rev_id_fkey FOREIGN KEY (rev_id) REFERENCES revision(rev_id);


--
-- Name: edition_tree_book_id_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY edition_tree
    ADD CONSTRAINT edition_tree_book_id_fkey FOREIGN KEY (book_id) REFERENCES book(book_id);


--
-- Name: edition_tree_publisher_id_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY edition_tree
    ADD CONSTRAINT edition_tree_publisher_id_fkey FOREIGN KEY (publisher_id) REFERENCES publisher(publisher_id);


--
-- Name: edition_tree_version_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY edition_tree
    ADD CONSTRAINT edition_tree_version_fkey FOREIGN KEY (version) REFERENCES edition_v(version);


--
-- Name: person_branch_branch_id_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY person_branch
    ADD CONSTRAINT person_branch_branch_id_fkey FOREIGN KEY (branch_id) REFERENCES branch(id);


--
-- Name: person_branch_person_id_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY person_branch
    ADD CONSTRAINT person_branch_person_id_fkey FOREIGN KEY (person_id) REFERENCES person(person_id);


--
-- Name: person_gid_person_id_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY person_bbid
    ADD CONSTRAINT person_gid_person_id_fkey FOREIGN KEY (person_id) REFERENCES person(person_id);


--
-- Name: person_revision_person_tree_id_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY person_revision
    ADD CONSTRAINT person_revision_person_tree_id_fkey FOREIGN KEY (person_tree_id) REFERENCES person_tree(person_tree_id);


--
-- Name: person_revision_rev_id_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY person_revision
    ADD CONSTRAINT person_revision_rev_id_fkey FOREIGN KEY (rev_id) REFERENCES revision(rev_id);


--
-- Name: person_tree_version_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY person_tree
    ADD CONSTRAINT person_tree_version_fkey FOREIGN KEY (version) REFERENCES person_v(version);


--
-- Name: publisher_branch_branch_id_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY publisher_branch
    ADD CONSTRAINT publisher_branch_branch_id_fkey FOREIGN KEY (branch_id) REFERENCES branch(id);


--
-- Name: publisher_branch_publisher_id_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY publisher_branch
    ADD CONSTRAINT publisher_branch_publisher_id_fkey FOREIGN KEY (publisher_id) REFERENCES publisher(publisher_id);


--
-- Name: publisher_gid_publisher_id_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY publisher_bbid
    ADD CONSTRAINT publisher_gid_publisher_id_fkey FOREIGN KEY (publisher_id) REFERENCES publisher(publisher_id);


--
-- Name: publisher_revision_publisher_tree_id_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY publisher_revision
    ADD CONSTRAINT publisher_revision_publisher_tree_id_fkey FOREIGN KEY (publisher_tree_id) REFERENCES publisher_tree(publisher_tree_id);


--
-- Name: publisher_revision_rev_id_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY publisher_revision
    ADD CONSTRAINT publisher_revision_rev_id_fkey FOREIGN KEY (rev_id) REFERENCES revision(rev_id);


--
-- Name: publisher_tree_version_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY publisher_tree
    ADD CONSTRAINT publisher_tree_version_fkey FOREIGN KEY (version) REFERENCES publisher_v(version);


--
-- Name: revision_editor_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY revision
    ADD CONSTRAINT revision_editor_fkey FOREIGN KEY (editor) REFERENCES bookbrainz.editor(editor_id);


--
-- Name: revision_parent_parent_id_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY revision_parent
    ADD CONSTRAINT revision_parent_parent_id_fkey FOREIGN KEY (parent_id) REFERENCES revision(rev_id);


--
-- Name: revision_parent_rev_id_fkey; Type: FK CONSTRAINT; Schema: bookbrainz_v; Owner: bookbrainz
--

ALTER TABLE ONLY revision_parent
    ADD CONSTRAINT revision_parent_rev_id_fkey FOREIGN KEY (rev_id) REFERENCES revision(rev_id);


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

