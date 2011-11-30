-- Automatically generated, do not edit.
\set ON_ERROR_STOP 1

ALTER TABLE book_bbid
   ADD CONSTRAINT book_bbid_fk_book_id
   FOREIGN KEY (book_id)
   REFERENCES book(book_id);

ALTER TABLE book_bbid
   ADD CONSTRAINT book_bbid_fk_bbid
   FOREIGN KEY (bbid)
   REFERENCES bbid(bbid);

ALTER TABLE book_branch
   ADD CONSTRAINT book_branch_fk_book_id
   FOREIGN KEY (book_id)
   REFERENCES book(book_id);

ALTER TABLE book_branch
   ADD CONSTRAINT book_branch_fk_branch_id
   FOREIGN KEY (branch_id)
   REFERENCES branch(branch_id);

ALTER TABLE book_person_role
   ADD CONSTRAINT book_person_role_fk_person_id
   FOREIGN KEY (person_id)
   REFERENCES person(person_id);

ALTER TABLE book_person_role
   ADD CONSTRAINT book_person_role_fk_book_tree_id
   FOREIGN KEY (book_tree_id)
   REFERENCES book_tree(book_tree_id);

ALTER TABLE book_revision
   ADD CONSTRAINT book_revision_fk_rev_id
   FOREIGN KEY (rev_id)
   REFERENCES revision(rev_id);

ALTER TABLE book_revision
   ADD CONSTRAINT book_revision_fk_book_tree_id
   FOREIGN KEY (book_tree_id)
   REFERENCES book_tree(book_tree_id);

ALTER TABLE book_tree
   ADD CONSTRAINT book_tree_fk_version
   FOREIGN KEY (version)
   REFERENCES book_v(version);

ALTER TABLE branch
   ADD CONSTRAINT branch_fk_rev_id
   FOREIGN KEY (rev_id)
   REFERENCES revision(rev_id);

ALTER TABLE edition_bbid
   ADD CONSTRAINT edition_bbid_fk_edition_id
   FOREIGN KEY (edition_id)
   REFERENCES edition(edition_id);

ALTER TABLE edition_bbid
   ADD CONSTRAINT edition_bbid_fk_bbid
   FOREIGN KEY (bbid)
   REFERENCES bbid(bbid);

ALTER TABLE edition_branch
   ADD CONSTRAINT edition_branch_fk_edition_id
   FOREIGN KEY (edition_id)
   REFERENCES edition(edition_id);

ALTER TABLE edition_branch
   ADD CONSTRAINT edition_branch_fk_branch_id
   FOREIGN KEY (branch_id)
   REFERENCES branch(branch_id);

ALTER TABLE edition_person_role
   ADD CONSTRAINT edition_person_role_fk_person_id
   FOREIGN KEY (person_id)
   REFERENCES person(person_id);

ALTER TABLE edition_person_role
   ADD CONSTRAINT edition_person_role_fk_edition_tree_id
   FOREIGN KEY (edition_tree_id)
   REFERENCES edition_tree(edition_tree_id);

ALTER TABLE edition_revision
   ADD CONSTRAINT edition_revision_fk_rev_id
   FOREIGN KEY (rev_id)
   REFERENCES revision(rev_id);

ALTER TABLE edition_revision
   ADD CONSTRAINT edition_revision_fk_edition_tree_id
   FOREIGN KEY (edition_tree_id)
   REFERENCES edition_tree(edition_tree_id);

ALTER TABLE edition_tree
   ADD CONSTRAINT edition_tree_fk_version
   FOREIGN KEY (version)
   REFERENCES edition_v(version);

ALTER TABLE edition_tree
   ADD CONSTRAINT edition_tree_fk_book_id
   FOREIGN KEY (book_id)
   REFERENCES book(book_id);

ALTER TABLE edition_tree
   ADD CONSTRAINT edition_tree_fk_publisher_id
   FOREIGN KEY (publisher_id)
   REFERENCES publisher(publisher_id);

ALTER TABLE edition_v
   ADD CONSTRAINT edition_v_fk_country_iso_code
   FOREIGN KEY (country_iso_code)
   REFERENCES bookbrainz.country(iso_code);

ALTER TABLE edition_v
   ADD CONSTRAINT edition_v_fk_language_iso_code
   FOREIGN KEY (language_iso_code)
   REFERENCES bookbrainz.country(iso_code);

ALTER TABLE edition_v
   ADD CONSTRAINT edition_v_fk_format
   FOREIGN KEY (format)
   REFERENCES bookbrainz.edition_format(id);

ALTER TABLE person_bbid
   ADD CONSTRAINT person_bbid_fk_person_id
   FOREIGN KEY (person_id)
   REFERENCES person(person_id);

ALTER TABLE person_bbid
   ADD CONSTRAINT person_bbid_fk_bbid
   FOREIGN KEY (bbid)
   REFERENCES bbid(bbid);

ALTER TABLE person_branch
   ADD CONSTRAINT person_branch_fk_person_id
   FOREIGN KEY (person_id)
   REFERENCES person(person_id);

ALTER TABLE person_branch
   ADD CONSTRAINT person_branch_fk_branch_id
   FOREIGN KEY (branch_id)
   REFERENCES branch(branch_id);

ALTER TABLE person_revision
   ADD CONSTRAINT person_revision_fk_rev_id
   FOREIGN KEY (rev_id)
   REFERENCES revision(rev_id);

ALTER TABLE person_revision
   ADD CONSTRAINT person_revision_fk_person_tree_id
   FOREIGN KEY (person_tree_id)
   REFERENCES person_tree(person_tree_id);

ALTER TABLE publisher_bbid
   ADD CONSTRAINT publisher_bbid_fk_publisher_id
   FOREIGN KEY (publisher_id)
   REFERENCES publisher(publisher_id);

ALTER TABLE publisher_bbid
   ADD CONSTRAINT publisher_bbid_fk_bbid
   FOREIGN KEY (bbid)
   REFERENCES bbid(bbid);

ALTER TABLE publisher_branch
   ADD CONSTRAINT publisher_branch_fk_publisher_id
   FOREIGN KEY (publisher_id)
   REFERENCES publisher(publisher_id);

ALTER TABLE publisher_branch
   ADD CONSTRAINT publisher_branch_fk_branch_id
   FOREIGN KEY (branch_id)
   REFERENCES branch(branch_id);

ALTER TABLE publisher_revision
   ADD CONSTRAINT publisher_revision_fk_rev_id
   FOREIGN KEY (rev_id)
   REFERENCES revision(rev_id);

ALTER TABLE publisher_revision
   ADD CONSTRAINT publisher_revision_fk_publisher_tree_id
   FOREIGN KEY (publisher_tree_id)
   REFERENCES publisher_tree(publisher_tree_id);

ALTER TABLE revision
   ADD CONSTRAINT revision_fk_editor
   FOREIGN KEY (editor)
   REFERENCES bookbrainz.editor(editor_id);

ALTER TABLE revision_parent
   ADD CONSTRAINT revision_parent_fk_rev_id
   FOREIGN KEY (rev_id)
   REFERENCES revision(rev_id);

ALTER TABLE revision_parent
   ADD CONSTRAINT revision_parent_fk_parent_id
   FOREIGN KEY (parent_id)
   REFERENCES revision(rev_id);

