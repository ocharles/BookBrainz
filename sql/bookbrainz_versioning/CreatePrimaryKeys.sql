-- Automatically generated, do not edit.
\set ON_ERROR_STOP 1

ALTER TABLE bbid ADD CONSTRAINT bbid_pkey PRIMARY KEY (bbid);
ALTER TABLE book ADD CONSTRAINT book_pkey PRIMARY KEY (book_id);
ALTER TABLE book_bbid ADD CONSTRAINT book_bbid_pkey PRIMARY KEY (book_id, bbid);
ALTER TABLE book_branch ADD CONSTRAINT book_branch_pkey PRIMARY KEY (branch_id);
ALTER TABLE book_person_role ADD CONSTRAINT book_person_role_pkey PRIMARY KEY (role_id, person_id, book_tree_id);
ALTER TABLE book_revision ADD CONSTRAINT book_revision_pkey PRIMARY KEY (rev_id);
ALTER TABLE book_tree ADD CONSTRAINT book_tree_pkey PRIMARY KEY (book_tree_id);
ALTER TABLE book_v ADD CONSTRAINT book_v_pkey PRIMARY KEY (version);
ALTER TABLE branch ADD CONSTRAINT branch_pkey PRIMARY KEY (branch_id);
ALTER TABLE edition ADD CONSTRAINT edition_pkey PRIMARY KEY (edition_id);
ALTER TABLE edition_bbid ADD CONSTRAINT edition_bbid_pkey PRIMARY KEY (edition_id, bbid);
ALTER TABLE edition_branch ADD CONSTRAINT edition_branch_pkey PRIMARY KEY (branch_id);
ALTER TABLE edition_person_role ADD CONSTRAINT edition_person_role_pkey PRIMARY KEY (role_id, person_id, edition_tree_id);
ALTER TABLE edition_revision ADD CONSTRAINT edition_revision_pkey PRIMARY KEY (rev_id);
ALTER TABLE edition_tree ADD CONSTRAINT edition_tree_pkey PRIMARY KEY (edition_tree_id);
ALTER TABLE edition_v ADD CONSTRAINT edition_v_pkey PRIMARY KEY (version);
ALTER TABLE person ADD CONSTRAINT person_pkey PRIMARY KEY (person_id);
ALTER TABLE person_bbid ADD CONSTRAINT person_bbid_pkey PRIMARY KEY (person_id);
ALTER TABLE person_branch ADD CONSTRAINT person_branch_pkey PRIMARY KEY (branch_id);
ALTER TABLE person_revision ADD CONSTRAINT person_revision_pkey PRIMARY KEY (rev_id);
ALTER TABLE person_tree ADD CONSTRAINT person_tree_pkey PRIMARY KEY (person_tree_id);
ALTER TABLE person_v ADD CONSTRAINT person_v_pkey PRIMARY KEY (version);
ALTER TABLE publisher ADD CONSTRAINT publisher_pkey PRIMARY KEY (publisher_id);
ALTER TABLE publisher_bbid ADD CONSTRAINT publisher_bbid_pkey PRIMARY KEY (publisher_id, bbid);
ALTER TABLE publisher_branch ADD CONSTRAINT publisher_branch_pkey PRIMARY KEY (branch_id);
ALTER TABLE publisher_revision ADD CONSTRAINT publisher_revision_pkey PRIMARY KEY (rev_id);
ALTER TABLE publisher_tree ADD CONSTRAINT publisher_tree_pkey PRIMARY KEY (publisher_tree_id);
ALTER TABLE publisher_v ADD CONSTRAINT publisher_v_pkey PRIMARY KEY (version);
ALTER TABLE revision ADD CONSTRAINT revision_pkey PRIMARY KEY (rev_id);
ALTER TABLE revision_parent ADD CONSTRAINT revision_parent_pkey PRIMARY KEY (rev_id, parent_id);
