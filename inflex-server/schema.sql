--
-- PostgreSQL database dump
--

-- Dumped from database version 12.9
-- Dumped by pg_dump version 12.9

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: pg_buffercache; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS pg_buffercache WITH SCHEMA public;


--
-- Name: EXTENSION pg_buffercache; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION pg_buffercache IS 'examine the shared buffer cache';


--
-- Name: pg_stat_statements; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS pg_stat_statements WITH SCHEMA public;


--
-- Name: EXTENSION pg_stat_statements; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION pg_stat_statements IS 'track execution statistics of all SQL statements executed';


SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: account; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.account (
    id bigint NOT NULL,
    username character varying,
    email character varying NOT NULL,
    password bytea NOT NULL,
    salt character varying NOT NULL,
    customer_id character varying NOT NULL,
    subscribed boolean NOT NULL
);


--
-- Name: account_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.account_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: account_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.account_id_seq OWNED BY public.account.id;


--
-- Name: cell; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.cell (
    id bigint NOT NULL,
    account bigint NOT NULL,
    document bigint NOT NULL,
    code bigint NOT NULL,
    created timestamp with time zone NOT NULL,
    name character varying NOT NULL,
    uuid character varying NOT NULL
)
WITH (toast_tuple_target='128');


--
-- Name: cell_dependency; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.cell_dependency (
    id bigint NOT NULL,
    origin bigint NOT NULL,
    target bigint NOT NULL
);


--
-- Name: cell_dependency_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.cell_dependency_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: cell_dependency_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.cell_dependency_id_seq OWNED BY public.cell_dependency.id;


--
-- Name: cell_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.cell_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: cell_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.cell_id_seq OWNED BY public.cell.id;


--
-- Name: code; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.code (
    id bigint NOT NULL,
    source character varying NOT NULL,
    hash bytea NOT NULL,
    created timestamp with time zone NOT NULL
)
WITH (toast_tuple_target='128');


--
-- Name: code_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.code_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: code_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.code_id_seq OWNED BY public.code.id;


--
-- Name: document; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.document (
    id bigint NOT NULL,
    account bigint NOT NULL,
    name character varying NOT NULL,
    created timestamp with time zone NOT NULL,
    updated timestamp with time zone NOT NULL,
    revision bigint
);


--
-- Name: document_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.document_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: document_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.document_id_seq OWNED BY public.document.id;


--
-- Name: early_access_request; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.early_access_request (
    id bigint NOT NULL,
    created timestamp with time zone NOT NULL,
    email character varying NOT NULL,
    approved timestamp with time zone
);


--
-- Name: early_access_request_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.early_access_request_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: early_access_request_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.early_access_request_id_seq OWNED BY public.early_access_request.id;


--
-- Name: file; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.file (
    id bigint NOT NULL,
    account bigint NOT NULL,
    name character varying NOT NULL,
    created timestamp with time zone NOT NULL,
    hash bytea NOT NULL,
    bytes bigint NOT NULL,
    mime character varying NOT NULL,
    content bytea NOT NULL
)
WITH (toast_tuple_target='128');


--
-- Name: file_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.file_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: file_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.file_id_seq OWNED BY public.file.id;


--
-- Name: revision; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.revision (
    id bigint NOT NULL,
    account bigint NOT NULL,
    document bigint NOT NULL,
    created timestamp with time zone NOT NULL
);


--
-- Name: revision_cell; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.revision_cell (
    id bigint NOT NULL,
    revision bigint NOT NULL,
    cell bigint NOT NULL,
    "order" bigint NOT NULL,
    msource_hash bytea,
    x bigint,
    y bigint
);


--
-- Name: revision_cell_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.revision_cell_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: revision_cell_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.revision_cell_id_seq OWNED BY public.revision_cell.id;


--
-- Name: revision_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.revision_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: revision_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.revision_id_seq OWNED BY public.revision.id;


--
-- Name: schema_versions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.schema_versions (
    version integer,
    date timestamp without time zone DEFAULT now()
);


--
-- Name: session; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.session (
    id bigint NOT NULL,
    uuid character varying NOT NULL,
    state character varying NOT NULL,
    nonce character varying
);


--
-- Name: session_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.session_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: session_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.session_id_seq OWNED BY public.session.id;


--
-- Name: account id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.account ALTER COLUMN id SET DEFAULT nextval('public.account_id_seq'::regclass);


--
-- Name: cell id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cell ALTER COLUMN id SET DEFAULT nextval('public.cell_id_seq'::regclass);


--
-- Name: cell_dependency id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cell_dependency ALTER COLUMN id SET DEFAULT nextval('public.cell_dependency_id_seq'::regclass);


--
-- Name: code id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.code ALTER COLUMN id SET DEFAULT nextval('public.code_id_seq'::regclass);


--
-- Name: document id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.document ALTER COLUMN id SET DEFAULT nextval('public.document_id_seq'::regclass);


--
-- Name: early_access_request id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.early_access_request ALTER COLUMN id SET DEFAULT nextval('public.early_access_request_id_seq'::regclass);


--
-- Name: file id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.file ALTER COLUMN id SET DEFAULT nextval('public.file_id_seq'::regclass);


--
-- Name: revision id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.revision ALTER COLUMN id SET DEFAULT nextval('public.revision_id_seq'::regclass);


--
-- Name: revision_cell id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.revision_cell ALTER COLUMN id SET DEFAULT nextval('public.revision_cell_id_seq'::regclass);


--
-- Name: session id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.session ALTER COLUMN id SET DEFAULT nextval('public.session_id_seq'::regclass);


--
-- Name: account account_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.account
    ADD CONSTRAINT account_pkey PRIMARY KEY (id);


--
-- Name: cell_dependency cell_dependency_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cell_dependency
    ADD CONSTRAINT cell_dependency_pkey PRIMARY KEY (id);


--
-- Name: cell cell_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cell
    ADD CONSTRAINT cell_pkey PRIMARY KEY (id);


--
-- Name: code code_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.code
    ADD CONSTRAINT code_pkey PRIMARY KEY (id);


--
-- Name: document document_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.document
    ADD CONSTRAINT document_pkey PRIMARY KEY (id);


--
-- Name: early_access_request early_access_request_early_access_email; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.early_access_request
    ADD CONSTRAINT early_access_request_early_access_email UNIQUE (email);


--
-- Name: early_access_request early_access_request_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.early_access_request
    ADD CONSTRAINT early_access_request_pkey PRIMARY KEY (id);


--
-- Name: file file_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.file
    ADD CONSTRAINT file_pkey PRIMARY KEY (id);


--
-- Name: revision_cell revision_cell_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.revision_cell
    ADD CONSTRAINT revision_cell_pkey PRIMARY KEY (id);


--
-- Name: revision revision_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.revision
    ADD CONSTRAINT revision_pkey PRIMARY KEY (id);


--
-- Name: session session_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.session
    ADD CONSTRAINT session_pkey PRIMARY KEY (id);


--
-- Name: session session_unique_uuid; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.session
    ADD CONSTRAINT session_unique_uuid UNIQUE (uuid);


--
-- Name: cell unique_cell; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cell
    ADD CONSTRAINT unique_cell UNIQUE (document, code, name, uuid);


--
-- Name: cell_dependency unique_cell_dependency; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cell_dependency
    ADD CONSTRAINT unique_cell_dependency UNIQUE (origin, target);


--
-- Name: code unique_code; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.code
    ADD CONSTRAINT unique_code UNIQUE (hash);


--
-- Name: document_account; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX document_account ON public.document USING btree (account);


--
-- Name: idx_doc_name; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_doc_name ON public.document USING btree (name);


--
-- Name: cell cell_account_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cell
    ADD CONSTRAINT cell_account_fkey FOREIGN KEY (account) REFERENCES public.account(id);


--
-- Name: cell cell_code_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cell
    ADD CONSTRAINT cell_code_fkey FOREIGN KEY (code) REFERENCES public.code(id);


--
-- Name: cell_dependency cell_dependency_origin_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cell_dependency
    ADD CONSTRAINT cell_dependency_origin_fkey FOREIGN KEY (origin) REFERENCES public.cell(id);


--
-- Name: cell_dependency cell_dependency_target_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cell_dependency
    ADD CONSTRAINT cell_dependency_target_fkey FOREIGN KEY (target) REFERENCES public.cell(id);


--
-- Name: cell cell_document_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cell
    ADD CONSTRAINT cell_document_fkey FOREIGN KEY (document) REFERENCES public.document(id);


--
-- Name: document document_account_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.document
    ADD CONSTRAINT document_account_fkey FOREIGN KEY (account) REFERENCES public.account(id);


--
-- Name: document document_revision_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.document
    ADD CONSTRAINT document_revision_fkey FOREIGN KEY (revision) REFERENCES public.revision(id);


--
-- Name: file file_account_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.file
    ADD CONSTRAINT file_account_fkey FOREIGN KEY (account) REFERENCES public.account(id);


--
-- Name: revision revision_account_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.revision
    ADD CONSTRAINT revision_account_fkey FOREIGN KEY (account) REFERENCES public.account(id);


--
-- Name: revision_cell revision_cell_cell_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.revision_cell
    ADD CONSTRAINT revision_cell_cell_fkey FOREIGN KEY (cell) REFERENCES public.cell(id);


--
-- Name: revision_cell revision_cell_revision_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.revision_cell
    ADD CONSTRAINT revision_cell_revision_fkey FOREIGN KEY (revision) REFERENCES public.revision(id);


--
-- Name: revision revision_document_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.revision
    ADD CONSTRAINT revision_document_fkey FOREIGN KEY (document) REFERENCES public.document(id);


--
-- PostgreSQL database dump complete
--

