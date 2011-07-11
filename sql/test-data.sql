BEGIN;

INSERT INTO person (id, name, gid) VALUES (1, 'Douglas Adams', '2d1746de-2696-4498-98f3-04f310a38b05');
INSERT INTO person (id, name, gid) VALUES (2, 'Mark Carwardine', '6ada1733-a4e8-429b-acbb-bb4114b7ee1a');
INSERT INTO person (id, name, gid) VALUES (3, 'Stephen Fry', 'f0cc7318-0767-4284-97e2-96c405ce9a8a');
INSERT INTO person (id, name, gid) VALUES (4, 'Terry Jones', 'f9ae46fc-64c2-418e-9b97-e70af767cc39');
INSERT INTO person (id, name, gid) VALUES (5, 'Benjamin Schwarz', 'a75ca825-5292-40ce-bea6-aaab8530ebb3');
INSERT INTO person (id, name, gid) VALUES (6, 'Sven Böttcher', '5b069f84-43a5-48f3-ad9a-7033e06ac06f');

INSERT INTO author_credit (id) VALUES (1);
INSERT INTO author_credit (id) VALUES (2);
INSERT INTO author_credit (id) VALUES (3);
INSERT INTO author_credit (id) VALUES (4);
INSERT INTO author_credit (id) VALUES (5);

INSERT INTO author_credit_person (author_credit, credited_name, position, person, join_phrase) VALUES (1, 'Douglas Adams', 0, 1, '');
INSERT INTO author_credit_person (author_credit, credited_name, position, person, join_phrase) VALUES (2, 'Douglas Adams', 0, 1, ' and ');
INSERT INTO author_credit_person (author_credit, credited_name, position, person, join_phrase) VALUES (2, 'Mark Carwadine', 1, 2, '');
INSERT INTO author_credit_person (author_credit, credited_name, position, person, join_phrase) VALUES (3, 'Douglas Adams', 0, 1, '');
INSERT INTO author_credit_person (author_credit, credited_name, position, person, join_phrase) VALUES (3, 'Stephen Fry', 1, 3, ' Foreword by ');
INSERT INTO author_credit_person (author_credit, credited_name, position, person, join_phrase) VALUES (4, 'Benjamin Schwarz', 0, 5, '');
INSERT INTO author_credit_person (author_credit, credited_name, position, person, join_phrase) VALUES (5, 'Sven Böttcher', 0, 6, '');

INSERT INTO country (iso_code, name) VALUES ('GB', 'United Kingdom');
INSERT INTO country (iso_code, name) VALUES ('DE', 'Germany');

INSERT INTO edition_format (id, name) VALUES (1, 'paperback');

INSERT INTO language (iso_code, name) VALUES ('eng', 'English');
INSERT INTO language (iso_code, name) VALUES ('deu', 'German');

INSERT INTO publisher (id, name, gid) VALUES (1, 'Pan Books', 'd1a77bb8-20a4-4f42-8321-2d4ba7e17fc7');
INSERT INTO publisher (id, name, gid) VALUES (2, 'Ullstein', '2e2b7910-bf83-4f20-af88-788b7741eac5');
INSERT INTO publisher (id, name, gid) VALUES (3, 'Heyne', 'ad9a2ce9-952d-4a00-b20e-78eed25ee512');
INSERT INTO publisher (id, name, gid) VALUES (4, 'Goldmann', '09c5c77a-462c-48df-86f6-6e0f3f4804f9');

INSERT INTO book (id, name, author_credit, gid) VALUES (1, 'The Hitch Hiker''s Guide to the Galaxy', 1, '3e46b224-df24-4720-b570-19e8fd916c13');
INSERT INTO book (id, name, author_credit, gid) VALUES (2, 'The Restaurant at the End of the Universe', 1, '4f737225-3f62-4715-addf-3716c5f4280f');
INSERT INTO book (id, name, author_credit, gid) VALUES (3, 'Life, the Universe and Everything', 1, '70efc1ef-4f82-45db-9e15-0b1ccc0486fb');
INSERT INTO book (id, name, author_credit, gid) VALUES (4, 'So Long, and Thanks for all the Fish', 1, '2f41bcaf-9d2b-4c57-8216-3f19d035c12c');
INSERT INTO book (id, name, author_credit, gid) VALUES (5, 'Mostly Harmless', 1, '09b903c7-9ea0-4d53-992b-2e54661e92ab');
INSERT INTO book (id, name, author_credit, gid) VALUES (6, 'The Salmon of Doubt', 3, 'e88b03d0-37dc-4e3c-a307-c16eca3d4480');
INSERT INTO book (id, name, author_credit, gid) VALUES (7, 'Dirk Gently''s Holistic Detective Agency', 1, 'ee4add1e-5134-4eee-b8a5-a09247d55b8c');
INSERT INTO book (id, name, author_credit, gid) VALUES (8, 'The Long Dark Tea-Time of the Soul', 1, 'd856eb42-c73e-4459-9fb5-c98e199af43d');
INSERT INTO book (id, name, author_credit, gid) VALUES (9, 'Last Chance to See', 2, '4e6f3b75-762a-47da-9d49-cfe676f99ed6');
INSERT INTO book (id, name, author_credit, gid) VALUES (10, 'Das Leben, das Universum und der ganze Rest / Macht''s gut, und danke für den Fisch', 1, '158f3083-c5a4-49b3-a0ff-ec8712fa841c');

INSERT INTO edition (id, name, book, year, publisher, country_iso_code, illustrator, translator, author, language_iso_code, isbn, barcode, edition_index, format, gid) VALUES (1, 'The Hitch Hiker''s Guide to the Galaxy', 1, 1979, 1, 'GB', null, null, 1, 'eng', '9780330258648', '9780330258648', null, 1, '98e30771-ba82-42bc-9199-33d67e5c583a');

INSERT INTO edition (id, name, book, year, publisher, country_iso_code, illustrator, translator, author, language_iso_code, isbn, barcode, edition_index, format, gid) VALUES (2, 'The Restaurant at the End of the Universe', 2, 1980, 1, 'GB', null, null, 1, 'eng', '9780330262132', '9780330262132', null, 1, '4260ff1c-549d-4395-9947-921323bfd500');

INSERT INTO edition (id, name, book, year, publisher, country_iso_code, illustrator, translator, author, language_iso_code, isbn, barcode, edition_index, format, gid) VALUES (3, 'Life, the Universe and Everything', 3, 1982, 1, 'GB', null, null, 1, 'eng', '9780330267380', '9780330267380', null, 1, 'b4a31d50-0d3a-4257-a1d7-f77781446b03');

INSERT INTO edition (id, name, book, year, publisher, country_iso_code, illustrator, translator, author, language_iso_code, isbn, barcode, edition_index, format, gid) VALUES (4, 'So Long, and Thanks for all the Fish', 4, 1985, 1, 'GB', null, null, 1, 'eng', '9780330287005', '9780330287005', null, 1, '69b63a49-3ed2-49b1-bb9a-be3646ad1d55');

INSERT INTO edition (id, name, book, year, publisher, country_iso_code, illustrator, translator, author, language_iso_code, isbn, barcode, edition_index, format, gid) VALUES (5, 'Mostly Harmless', 5, 1993, 1, 'GB', null, null, 1, 'eng', '9780330323116', '9780330323116', null, 1, 'e47d6c20-8c6e-440b-a87e-66b80df7ac32');

INSERT INTO edition (id, name, book, year, publisher, country_iso_code, illustrator, translator, author, language_iso_code, isbn, barcode, edition_index, format, gid) VALUES (6, 'The Salmon of Doubt', 6, 2002, 1, 'GB', null, null, 3, 'eng', '9780330323123', '9780330323123', null, 1, '90aca3c0-013b-42ef-b1cc-280d68eb323e');

INSERT INTO edition (id, name, book, year, publisher, country_iso_code, illustrator, translator, author, language_iso_code, isbn, barcode, edition_index, format, gid) VALUES (7, 'Dirk Gently''s Holistic Detective Agency', 7, 1988, 1, 'GB', null, null, 1, 'eng', '9780330301626', '9780330301626', null, 1, '940be86a-d75d-4ca1-b522-5d58e268a9cb');

INSERT INTO edition (id, name, book, year, publisher, country_iso_code, illustrator, translator, author, language_iso_code, isbn, barcode, edition_index, format, gid) VALUES (8, 'The Long Dark Tea-Time of the Soul', 8, 1989, 1, 'GB', null, null, 1, 'eng', '9780330309554', '9780330309554', null, 1, '936d55ec-c075-489d-a937-d16214e91b47');

INSERT INTO edition (id, name, book, year, publisher, country_iso_code, illustrator, translator, author, language_iso_code, isbn, barcode, edition_index, format, gid) VALUES (9, 'Last Chance to See', 9, 1991, 1, 'GB', null, null, 2, 'eng', '9780330320023', '9780330320023', null, 1, '3a1013fd-6748-433e-8604-070919279b2e');

INSERT INTO edition (id, name, book, year, publisher, country_iso_code, illustrator, translator, author, language_iso_code, isbn, barcode, edition_index, format, gid) VALUES (10, 'Per Anhalter durch die Galaxis', 1, null, 1, 'DE', null, 4, 1, 'deu', '9783548310701', '9783548310701', null, 1, 'a68a8c06-87e7-4166-b830-59d5db3adfcb');

INSERT INTO edition (id, name, book, year, publisher, country_iso_code, illustrator, translator, author, language_iso_code, isbn, barcode, edition_index, format, gid) VALUES (11, 'Das Restaurant am Ende des Universums', 2, 1900, 1, 'DE', null, 4, 1, 'deu', '9783548224923', '9783548224923', null, 1, '661b0794-c4af-4235-81c0-f36333232d38');

INSERT INTO edition (id, name, book, year, publisher, country_iso_code, illustrator, translator, author, language_iso_code, isbn, barcode, edition_index, format, gid) VALUES (12, 'Das Leben, das Universum und der ganze Rest / Macht''s gut, und danke für den Fisch', 10, 1900, 1, 'DE', null, 4, 1, 'deu', '9783548236810', '9783548236810', null, 1, '6609df12-7435-4780-9a6e-99f90a59e720');

INSERT INTO edition (id, name, book, year, publisher, country_iso_code, illustrator, translator, author, language_iso_code, isbn, barcode, edition_index, format, gid) VALUES (13, 'Einmal Rupert und zurück', 1, 1900, 1, 'DE', null, 5, 1, 'deu', '9783453082304', '9783453082304', null, 1, 'e1a157b2-5a1f-4c82-8f24-ccca12e8cf4a');

COMMIT;
