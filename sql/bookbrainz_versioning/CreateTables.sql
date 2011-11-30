CREATE TABLE bbid (
    bbid uuid NOT NULL -- PK
);

CREATE TABLE branch (
    master boolean NOT NULL,
    rev_id integer NOT NULL, -- references revision.rev_id
    branch_id serial NOT NULL -- PK
);

CREATE TABLE revision (
    rev_id serial NOT NULL, -- PK
    commited timestamp with time zone DEFAULT now() NOT NULL,
    editor integer NOT NULL -- references bookbrainz.editor.editor_id
);

CREATE TABLE revision_parent (
    rev_id integer NOT NULL,  -- PK, references revision.rev_id
    parent_id integer NOT NULL  -- PK, references revision.rev_id
);



CREATE TABLE book (
    book_id serial NOT NULL -- PK
);

CREATE TABLE book_bbid (
    book_id serial NOT NULL, -- PK, references book.book_id
    bbid uuid NOT NULL -- PK, references bbid.bbid
);

CREATE TABLE book_branch (
    book_id integer NOT NULL, -- references book.book_id
    branch_id integer NOT NULL -- PK, references branch.branch_id
);

CREATE TABLE book_revision (
    rev_id integer NOT NULL, -- PK, references revision.rev_id
    book_tree_id integer NOT NULL -- references book_tree.book_tree_id
);

CREATE TABLE book_tree (
    book_tree_id serial NOT NULL, -- PK
    version integer NOT NULL -- references book_v.version
);

CREATE TABLE book_v (
    version serial NOT NULL, -- PK
    name text NOT NULL
);

CREATE TABLE book_person_role (
    role_id integer NOT NULL, -- PK
    person_id integer NOT NULL, -- PK, references person.person_id
    book_tree_id integer NOT NULL -- PK, references book_tree.book_tree_id
);



CREATE TABLE edition (
    edition_id serial NOT NULL -- PK
);

CREATE TABLE edition_bbid (
    edition_id integer NOT NULL, -- PK, references edition.edition_id
    bbid uuid NOT NULL -- PK, references bbid.bbid
);

CREATE TABLE edition_branch (
    edition_id integer NOT NULL, -- references edition.edition_id
    branch_id integer NOT NULL -- PK, references branch.branch_id
);

CREATE TABLE edition_revision (
    rev_id integer NOT NULL, -- PK, references revision.rev_id
    edition_tree_id integer NOT NULL -- references edition_tree.edition_tree_id
);

CREATE TABLE edition_tree (
    edition_tree_id serial NOT NULL, -- PK
    version integer NOT NULL, -- references edition_v.version
    book_id integer NOT NULL, -- references book.book_id
    publisher_id integer -- references publisher.publisher_id
);

CREATE TABLE edition_v (
    version serial NOT NULL, -- PK
    name text NOT NULL,
    year integer,
    country_iso_code text, -- references bookbrainz.country.iso_code
    language_iso_code text, -- references bookbrainz.country.iso_code
    isbn text,
    edition_index integer,
    format integer, -- references bookbrainz.edition_format.id
    CONSTRAINT edition_edition_index_check CHECK (((edition_index IS NULL) OR (edition_index > 0)))
);

CREATE TABLE edition_person_role (
    role_id integer NOT NULL, -- PK
    person_id integer NOT NULL, -- PK, references person.person_id
    edition_tree_id integer NOT NULL -- PK, references edition_tree.edition_tree_id
);



CREATE TABLE person (
    person_id serial NOT NULL -- PK
);

CREATE TABLE person_bbid (
    person_id integer NOT NULL, -- PK, references person.person_id
    bbid uuid NOT NULL -- references bbid.bbid
);

CREATE TABLE person_branch (
    person_id integer NOT NULL, -- references person.person_id
    branch_id integer NOT NULL -- PK, references branch.branch_id
);

CREATE TABLE person_revision (
    rev_id integer NOT NULL, -- PK, references revision.rev_id
    person_tree_id integer NOT NULL -- references person_tree.person_tree_id
);

CREATE TABLE person_tree (
    person_tree_id serial NOT NULL, -- PK
    version integer NOT NULL
);

CREATE TABLE person_v (
    version serial NOT NULL, -- PK
    name text NOT NULL
);



CREATE TABLE publisher (
    publisher_id serial NOT NULL -- PK
);

CREATE TABLE publisher_bbid (
    publisher_id integer NOT NULL, -- PK, references publisher.publisher_id.
    bbid uuid NOT NULL -- PK, references bbid.bbid
);

CREATE TABLE publisher_branch (
    publisher_id integer NOT NULL, -- references publisher.publisher_id
    branch_id integer NOT NULL -- PK, references branch.branch_id
);

CREATE TABLE publisher_revision (
    rev_id integer NOT NULL, -- PK, references revision.rev_id
    publisher_tree_id integer NOT NULL -- references publisher_tree.publisher_tree_id
);

CREATE TABLE publisher_tree (
    publisher_tree_id serial NOT NULL, -- PK
    version integer NOT NULL
);

CREATE TABLE publisher_v (
    version integer NOT NULL, -- PK
    name text NOT NULL
);
