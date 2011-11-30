-- Automatically generated, do not edit.
\unset ON_ERROR_STOP

SELECT setval('branch_branch_id_seq', (SELECT MAX(branch_id) FROM branch));
SELECT setval('revision_rev_id_seq', (SELECT MAX(rev_id) FROM revision));
SELECT setval('book_book_id_seq', (SELECT MAX(book_id) FROM book));
SELECT setval('book_bbid_book_id_seq', (SELECT MAX(book_id) FROM book_bbid));
SELECT setval('book_tree_book_tree_id_seq', (SELECT MAX(book_tree_id) FROM book_tree));
SELECT setval('book_v_version_seq', (SELECT MAX(version) FROM book_v));
SELECT setval('edition_edition_id_seq', (SELECT MAX(edition_id) FROM edition));
SELECT setval('edition_tree_edition_tree_id_seq', (SELECT MAX(edition_tree_id) FROM edition_tree));
SELECT setval('edition_v_version_seq', (SELECT MAX(version) FROM edition_v));
SELECT setval('person_person_id_seq', (SELECT MAX(person_id) FROM person));
SELECT setval('person_tree_person_tree_id_seq', (SELECT MAX(person_tree_id) FROM person_tree));
SELECT setval('person_v_version_seq', (SELECT MAX(version) FROM person_v));
SELECT setval('publisher_publisher_id_seq', (SELECT MAX(publisher_id) FROM publisher));
SELECT setval('publisher_tree_publisher_tree_id_seq', (SELECT MAX(publisher_tree_id) FROM publisher_tree));
