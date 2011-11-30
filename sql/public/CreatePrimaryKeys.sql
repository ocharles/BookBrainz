-- Automatically generated, do not edit.
\set ON_ERROR_STOP 1

ALTER TABLE country ADD CONSTRAINT country_pkey PRIMARY KEY (iso_code);
ALTER TABLE edition_format ADD CONSTRAINT edition_format_pkey PRIMARY KEY (id);
ALTER TABLE editor ADD CONSTRAINT editor_pkey PRIMARY KEY (editor_id);
ALTER TABLE language ADD CONSTRAINT language_pkey PRIMARY KEY (iso_code);
ALTER TABLE person_role ADD CONSTRAINT person_role_pkey PRIMARY KEY (role_id);
