CREATE TABLE public.account (
    id bigint NOT NULL PRIMARY KEY,
    username character varying,
    email character varying NOT NULL,
    password bytea NOT NULL,
    salt character varying NOT NULL,
    customer_id character varying NOT NULL,
    subscribed boolean DEFAULT false NOT NULL
);

CREATE TABLE public.document (
    id bigint NOT NULL PRIMARY KEY,
    account bigint NOT NULL,
    name character varying NOT NULL,
    created timestamp with time zone NOT NULL,
    updated timestamp with time zone NOT NULL
);


CREATE TABLE public.early_access_request (
    id bigint NOT NULL PRIMARY KEY,
    created timestamp with time zone NOT NULL,
    email character varying NOT NULL,
    approved timestamp with time zone
);


CREATE TABLE public.file (
    id bigint NOT NULL,
    account bigint NOT NULL,
    name character varying NOT NULL,
    created timestamp with time zone NOT NULL,
    hash bytea NOT NULL,
    bytes bigint NOT NULL,
    mime character varying NOT NULL
);


CREATE TABLE public.revision (
    id bigint NOT NULL,
    account bigint NOT NULL,
    document bigint NOT NULL,
    created timestamp with time zone NOT NULL,
    content character varying NOT NULL,
    active boolean NOT NULL,
    activated timestamp with time zone NOT NULL
);


CREATE TABLE public.schema_versions (
    version integer,
    date timestamp without time zone DEFAULT now()
);


CREATE TABLE public.session (
    id bigint NOT NULL,
    uuid character varying NOT NULL,
    state character varying NOT NULL,
    nonce character varying
);
