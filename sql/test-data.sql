BEGIN;

SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = off;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET escape_string_warning = off;

SET search_path = bookbrainz, pg_catalog;

--
-- Name: edition_format_id_seq; Type: SEQUENCE SET; Schema: bookbrainz; Owner: bookbrainz
--

SELECT pg_catalog.setval('edition_format_id_seq', 1, false);


SET search_path = bookbrainz_v, pg_catalog;

--
-- Name: book_book_id_seq; Type: SEQUENCE SET; Schema: bookbrainz_v; Owner: bookbrainz
--

SELECT pg_catalog.setval('book_book_id_seq', 1, false);


--
-- Name: book_tree_book_tree_id_seq; Type: SEQUENCE SET; Schema: bookbrainz_v; Owner: bookbrainz
--

SELECT pg_catalog.setval('book_tree_book_tree_id_seq', 1, false);


--
-- Name: book_version_seq; Type: SEQUENCE SET; Schema: bookbrainz_v; Owner: bookbrainz
--

SELECT pg_catalog.setval('book_version_seq', 1, false);


--
-- Name: branch_id_seq; Type: SEQUENCE SET; Schema: bookbrainz_v; Owner: bookbrainz
--

SELECT pg_catalog.setval('branch_id_seq', 33, true);


--
-- Name: edition_edition_id_seq; Type: SEQUENCE SET; Schema: bookbrainz_v; Owner: bookbrainz
--

SELECT pg_catalog.setval('edition_edition_id_seq', 1, false);


--
-- Name: edition_tree_edition_tree_id_seq; Type: SEQUENCE SET; Schema: bookbrainz_v; Owner: bookbrainz
--

SELECT pg_catalog.setval('edition_tree_edition_tree_id_seq', 1, false);


--
-- Name: edition_version_seq; Type: SEQUENCE SET; Schema: bookbrainz_v; Owner: bookbrainz
--

SELECT pg_catalog.setval('edition_version_seq', 1, false);


--
-- Name: person_person_id_seq; Type: SEQUENCE SET; Schema: bookbrainz_v; Owner: bookbrainz
--

SELECT pg_catalog.setval('person_person_id_seq', 1, false);


--
-- Name: person_tree_person_tree_id_seq; Type: SEQUENCE SET; Schema: bookbrainz_v; Owner: bookbrainz
--

SELECT pg_catalog.setval('person_tree_person_tree_id_seq', 1, false);


--
-- Name: person_version_seq; Type: SEQUENCE SET; Schema: bookbrainz_v; Owner: bookbrainz
--

SELECT pg_catalog.setval('person_version_seq', 1, false);


--
-- Name: publisher_publisher_id_seq; Type: SEQUENCE SET; Schema: bookbrainz_v; Owner: bookbrainz
--

SELECT pg_catalog.setval('publisher_publisher_id_seq', 1, false);


--
-- Name: publisher_tree_publisher_tree_id_seq; Type: SEQUENCE SET; Schema: bookbrainz_v; Owner: bookbrainz
--

SELECT pg_catalog.setval('publisher_tree_publisher_tree_id_seq', 1, false);


--
-- Name: publisher_version_seq; Type: SEQUENCE SET; Schema: bookbrainz_v; Owner: bookbrainz
--

SELECT pg_catalog.setval('publisher_version_seq', 1, false);


--
-- Name: revision_rev_id_seq; Type: SEQUENCE SET; Schema: bookbrainz_v; Owner: bookbrainz
--

SELECT pg_catalog.setval('revision_rev_id_seq', 33, true);


SET search_path = bookbrainz, pg_catalog;

--
-- Data for Name: author_credit_person; Type: TABLE DATA; Schema: bookbrainz; Owner: bookbrainz
--

COPY author_credit_person (author_credit, credited_name, "position", person, join_phrase) FROM stdin;
1	Douglas Adams	0	1	
2	Douglas Adams	0	1	 and 
2	Mark Carwadine	1	2	
3	Douglas Adams	0	1	
3	Stephen Fry	1	3	 Foreword by 
4	Benjamin Schwarz	0	5	
5	Sven Böttcher	0	6	
\.


SET search_path = bookbrainz_v, pg_catalog;

--
-- Data for Name: book_v; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY book_v (version, name) FROM stdin;
1	The Hitch Hiker's Guide to the Galaxy
2	The Restaurant at the End of the Universe
3	Life, the Universe and Everything
4	So Long, and Thanks for all the Fish
5	Mostly Harmless
6	The Salmon of Doubt
7	Dirk Gently's Holistic Detective Agency
8	The Long Dark Tea-Time of the Soul
9	Last Chance to See
10	Das Leben, das Universum und der ganze Rest / Macht's gut, und danke für den Fisch
\.


--
-- Data for Name: book_tree; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY book_tree (book_tree_id, version) FROM stdin;
8	8
4	4
6	6
3	3
10	10
1	1
5	5
2	2
9	9
7	7
\.


--
-- Data for Name: person; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY person (person_id) FROM stdin;
2
5
6
4
1
3
\.


SET search_path = bookbrainz, pg_catalog;

--
-- Data for Name: book_person_role; Type: TABLE DATA; Schema: bookbrainz; Owner: bookbrainz
--

COPY book_person_role (role_id, person_id, book_tree_id) FROM stdin;
4	1	1
4	1	2
4	1	3
4	1	4
4	1	5
4	3	6
4	1	6
4	1	7
4	1	8
4	2	9
4	1	9
4	1	10
\.


--
-- Data for Name: country; Type: TABLE DATA; Schema: bookbrainz; Owner: bookbrainz
--

COPY country (iso_code, name) FROM stdin;
GB	United Kingdom
DE	Germany
\.


--
-- Data for Name: edition_format; Type: TABLE DATA; Schema: bookbrainz; Owner: bookbrainz
--

COPY edition_format (id, name) FROM stdin;
1	paperback
\.


--
-- Data for Name: language; Type: TABLE DATA; Schema: bookbrainz; Owner: bookbrainz
--

COPY language (iso_code, name) FROM stdin;
eng	English
deu	German
\.


SET search_path = bookbrainz_v, pg_catalog;

--
-- Data for Name: book; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY book (book_id) FROM stdin;
10
2
5
8
6
4
1
3
9
7
\.


--
-- Data for Name: edition_v; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY edition_v (version, name, year, country_iso_code, language_iso_code, isbn, edition_index, format) FROM stdin;
1	The Hitch Hiker's Guide to the Galaxy	1979	GB	eng	9780330258648	\N	1
2	The Restaurant at the End of the Universe	1980	GB	eng	9780330262132	\N	1
3	Life, the Universe and Everything	1982	GB	eng	9780330267380	\N	1
4	So Long, and Thanks for all the Fish	1985	GB	eng	9780330287005	\N	1
5	Mostly Harmless	1993	GB	eng	9780330323116	\N	1
6	The Salmon of Doubt	2002	GB	eng	9780330323123	\N	1
7	Dirk Gently's Holistic Detective Agency	1988	GB	eng	9780330301626	\N	1
8	The Long Dark Tea-Time of the Soul	1989	GB	eng	9780330309554	\N	1
9	Last Chance to See	1991	GB	eng	9780330320023	\N	1
10	Per Anhalter durch die Galaxis	\N	DE	deu	9783548310701	\N	1
11	Das Restaurant am Ende des Universums	1900	DE	deu	9783548224923	\N	1
12	Das Leben, das Universum und der ganze Rest / Macht's gut, und danke für den Fisch	1900	DE	deu	9783548236810	\N	1
13	Einmal Rupert und zurück	1900	DE	deu	9783453082304	\N	1
\.


--
-- Data for Name: publisher; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY publisher (publisher_id) FROM stdin;
2
4
1
3
\.


--
-- Data for Name: edition_tree; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY edition_tree (edition_tree_id, version, book_id, publisher_id) FROM stdin;
10	10	1	1
13	13	1	1
8	8	8	1
4	4	4	1
11	11	2	1
12	12	10	1
7	7	7	1
5	5	5	1
2	2	2	1
3	3	3	1
9	9	9	1
1	1	1	1
6	6	6	1
\.


SET search_path = bookbrainz, pg_catalog;

--
-- Data for Name: edition_person_role; Type: TABLE DATA; Schema: bookbrainz; Owner: bookbrainz
--

COPY edition_person_role (role_id, person_id, edition_tree_id) FROM stdin;
1	1	13
1	1	12
1	1	11
1	1	10
1	1	8
1	1	7
1	1	5
1	1	4
1	1	3
1	1	2
1	1	1
1	1	9
1	2	9
1	1	6
1	3	6
4	5	12
4	5	11
4	5	10
4	6	13
\.

SET search_path = bookbrainz_v, pg_catalog;

--
-- Data for Name: bbid; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY bbid (bbid) FROM stdin;
ee4add1e-5134-4eee-b8a5-a09247d55b8c
f0cc7318-0767-4284-97e2-96c405ce9a8a
69b63a49-3ed2-49b1-bb9a-be3646ad1d55
e88b03d0-37dc-4e3c-a307-c16eca3d4480
d1a77bb8-20a4-4f42-8321-2d4ba7e17fc7
e47d6c20-8c6e-440b-a87e-66b80df7ac32
2f41bcaf-9d2b-4c57-8216-3f19d035c12c
09b903c7-9ea0-4d53-992b-2e54661e92ab
ad9a2ce9-952d-4a00-b20e-78eed25ee512
a75ca825-5292-40ce-bea6-aaab8530ebb3
661b0794-c4af-4235-81c0-f36333232d38
4e6f3b75-762a-47da-9d49-cfe676f99ed6
b4a31d50-0d3a-4257-a1d7-f77781446b03
90aca3c0-013b-42ef-b1cc-280d68eb323e
a68a8c06-87e7-4166-b830-59d5db3adfcb
6ada1733-a4e8-429b-acbb-bb4114b7ee1a
3a1013fd-6748-433e-8604-070919279b2e
2d1746de-2696-4498-98f3-04f310a38b05
158f3083-c5a4-49b3-a0ff-ec8712fa841c
940be86a-d75d-4ca1-b522-5d58e268a9cb
6609df12-7435-4780-9a6e-99f90a59e720
09c5c77a-462c-48df-86f6-6e0f3f4804f9
2e2b7910-bf83-4f20-af88-788b7741eac5
4260ff1c-549d-4395-9947-921323bfd500
d856eb42-c73e-4459-9fb5-c98e199af43d
936d55ec-c075-489d-a937-d16214e91b47
f9ae46fc-64c2-418e-9b97-e70af767cc39
98e30771-ba82-42bc-9199-33d67e5c583a
e1a157b2-5a1f-4c82-8f24-ccca12e8cf4a
4f737225-3f62-4715-addf-3716c5f4280f
3e46b224-df24-4720-b570-19e8fd916c13
5b069f84-43a5-48f3-ad9a-7033e06ac06f
70efc1ef-4f82-45db-9e15-0b1ccc0486fb
\.


--
-- Data for Name: book_bbid; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY book_bbid (book_id, bbid) FROM stdin;
8	d856eb42-c73e-4459-9fb5-c98e199af43d
4	2f41bcaf-9d2b-4c57-8216-3f19d035c12c
6	e88b03d0-37dc-4e3c-a307-c16eca3d4480
3	70efc1ef-4f82-45db-9e15-0b1ccc0486fb
10	158f3083-c5a4-49b3-a0ff-ec8712fa841c
1	3e46b224-df24-4720-b570-19e8fd916c13
5	09b903c7-9ea0-4d53-992b-2e54661e92ab
2	4f737225-3f62-4715-addf-3716c5f4280f
9	4e6f3b75-762a-47da-9d49-cfe676f99ed6
7	ee4add1e-5134-4eee-b8a5-a09247d55b8c
\.


--
-- Data for Name: revision; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY revision (rev_id, commited, editor) FROM stdin;
1	2012-06-03 12:23:08.706127+02	1
2	2012-06-03 12:23:08.706127+02	1
3	2012-06-03 12:23:08.706127+02	1
4	2012-06-03 12:23:08.706127+02	1
5	2012-06-03 12:23:08.706127+02	1
6	2012-06-03 12:23:08.706127+02	1
7	2012-06-03 12:23:08.706127+02	1
8	2012-06-03 12:23:08.706127+02	1
9	2012-06-03 12:23:08.706127+02	1
10	2012-06-03 12:23:08.706127+02	1
11	2012-06-03 12:23:08.706127+02	1
12	2012-06-03 12:23:08.706127+02	1
13	2012-06-03 12:23:08.706127+02	1
14	2012-06-03 12:23:08.706127+02	1
15	2012-06-03 12:23:08.706127+02	1
16	2012-06-03 12:23:08.706127+02	1
17	2012-06-03 12:23:08.706127+02	1
18	2012-06-03 12:23:08.706127+02	1
19	2012-06-03 12:23:08.706127+02	1
20	2012-06-03 12:23:08.706127+02	1
21	2012-06-03 12:23:08.706127+02	1
22	2012-06-03 12:23:08.706127+02	1
23	2012-06-03 12:23:08.706127+02	1
24	2012-06-03 12:23:08.706127+02	1
25	2012-06-03 12:23:08.706127+02	1
26	2012-06-03 12:23:08.706127+02	1
27	2012-06-03 12:23:08.706127+02	1
28	2012-06-03 12:23:08.706127+02	1
29	2012-06-03 12:23:08.706127+02	1
30	2012-06-03 12:23:08.706127+02	1
31	2012-06-03 12:23:08.706127+02	1
32	2012-06-03 12:23:08.706127+02	1
33	2012-06-03 12:23:08.706127+02	1
\.


--
-- Data for Name: branch; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY branch (master, rev_id, branch_id) FROM stdin;
t	1	1
t	2	2
t	3	3
t	4	4
t	5	5
t	6	6
t	7	7
t	8	8
t	9	9
t	10	10
t	11	11
t	12	12
t	13	13
t	14	14
t	15	15
t	16	16
t	17	17
t	18	18
t	19	19
t	20	20
t	21	21
t	22	22
t	23	23
t	24	24
t	25	25
t	26	26
t	27	27
t	28	28
t	29	29
t	30	30
t	31	31
t	32	32
t	33	33
\.


--
-- Data for Name: book_branch; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY book_branch (book_id, branch_id) FROM stdin;
8	8
4	4
6	6
3	3
10	10
1	1
5	5
2	2
9	9
7	7
\.


--
-- Data for Name: book_revision; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY book_revision (rev_id, book_tree_id) FROM stdin;
1	1
2	2
3	3
4	4
5	5
6	6
7	7
8	8
9	9
10	10
\.


--
-- Data for Name: edition; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY edition (edition_id) FROM stdin;
10
13
8
4
11
12
7
5
2
3
9
1
6
\.


--
-- Data for Name: edition_bbid; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY edition_bbid (edition_id, bbid) FROM stdin;
10	a68a8c06-87e7-4166-b830-59d5db3adfcb
13	e1a157b2-5a1f-4c82-8f24-ccca12e8cf4a
8	936d55ec-c075-489d-a937-d16214e91b47
4	69b63a49-3ed2-49b1-bb9a-be3646ad1d55
11	661b0794-c4af-4235-81c0-f36333232d38
12	6609df12-7435-4780-9a6e-99f90a59e720
7	940be86a-d75d-4ca1-b522-5d58e268a9cb
5	e47d6c20-8c6e-440b-a87e-66b80df7ac32
2	4260ff1c-549d-4395-9947-921323bfd500
3	b4a31d50-0d3a-4257-a1d7-f77781446b03
9	3a1013fd-6748-433e-8604-070919279b2e
1	98e30771-ba82-42bc-9199-33d67e5c583a
6	90aca3c0-013b-42ef-b1cc-280d68eb323e
\.


--
-- Data for Name: edition_branch; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY edition_branch (edition_id, branch_id) FROM stdin;
10	30
13	33
8	28
4	24
11	31
12	32
7	27
5	25
2	22
3	23
9	29
1	21
6	26
\.


--
-- Data for Name: edition_revision; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY edition_revision (rev_id, edition_tree_id) FROM stdin;
21	1
22	2
23	3
24	4
25	5
26	6
27	7
28	8
29	9
30	10
31	11
32	12
33	13
\.


--
-- Data for Name: person_bbid; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY person_bbid (person_id, bbid) FROM stdin;
5	a75ca825-5292-40ce-bea6-aaab8530ebb3
1	2d1746de-2696-4498-98f3-04f310a38b05
6	5b069f84-43a5-48f3-ad9a-7033e06ac06f
4	f9ae46fc-64c2-418e-9b97-e70af767cc39
2	6ada1733-a4e8-429b-acbb-bb4114b7ee1a
3	f0cc7318-0767-4284-97e2-96c405ce9a8a
\.


--
-- Data for Name: person_branch; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY person_branch (person_id, branch_id) FROM stdin;
5	19
1	15
6	20
4	18
2	16
3	17
\.


--
-- Data for Name: person_v; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY person_v (version, name) FROM stdin;
1	Douglas Adams
2	Mark Carwardine
3	Stephen Fry
4	Terry Jones
5	Benjamin Schwarz
6	Sven Böttcher
\.


--
-- Data for Name: person_tree; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY person_tree (person_tree_id, version) FROM stdin;
5	5
1	1
6	6
4	4
2	2
3	3
\.


--
-- Data for Name: person_revision; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY person_revision (rev_id, person_tree_id) FROM stdin;
15	1
16	2
17	3
18	4
19	5
20	6
\.


--
-- Data for Name: publisher_bbid; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY publisher_bbid (publisher_id, bbid) FROM stdin;
2	2e2b7910-bf83-4f20-af88-788b7741eac5
3	ad9a2ce9-952d-4a00-b20e-78eed25ee512
1	d1a77bb8-20a4-4f42-8321-2d4ba7e17fc7
4	09c5c77a-462c-48df-86f6-6e0f3f4804f9
\.


--
-- Data for Name: publisher_branch; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY publisher_branch (publisher_id, branch_id) FROM stdin;
2	12
3	13
1	11
4	14
\.


--
-- Data for Name: publisher_v; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY publisher_v (version, name) FROM stdin;
1	Pan Books
2	Ullstein
3	Heyne
4	Goldmann
\.


--
-- Data for Name: publisher_tree; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY publisher_tree (publisher_tree_id, version) FROM stdin;
2	2
3	3
1	1
4	4
\.


--
-- Data for Name: publisher_revision; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY publisher_revision (rev_id, publisher_tree_id) FROM stdin;
11	1
12	2
13	3
14	4
\.


--
-- Data for Name: revision_parent; Type: TABLE DATA; Schema: bookbrainz_v; Owner: bookbrainz
--

COPY revision_parent (rev_id, parent_id) FROM stdin;
\.


--
-- PostgreSQL database dump complete
--

COMMIT;

