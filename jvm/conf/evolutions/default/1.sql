# --- !Ups

create table users (
    user_id varchar primary key,
    name_ varchar not null,
    email varchar not null,
    avatar_url varchar,
    activated bool not null,

    provider_id varchar not null,
    provider_key varchar not null,

    auth_info jsonb not null
);

create table documents (
    document_id varchar primary key,
    root_node_id varchar not null,
    current_version bigint not null
);

create table permissions (
    user_id varchar not null references users(user_id),
    document_id varchar not null references documents(document_id),
    permission_level integer not null
);

create table nodes (
    document_id varchar not null references documents(document_id),
    node_id varchar primary key, /* this is NOT unique, because a node can have multiple versions */
    childs varchar[], /* null means root node */
    attrs bytea,
    cont bytea not null
);

create table changes (
    document_id varchar not null references documents(document_id),
    from_version bigint not null,
    cont bytea not null
);

# --- !Downs

drop table changes;
drop table nodes;
drop table permissions;
drop table documents;
drop table users;

