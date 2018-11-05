# --- !Ups

create table if not exists users (
    user_id varchar primary key,
    created_time bigint not null,
    name_ varchar not null,
    email varchar not null,
    avatar_url varchar,
    activated bool not null,

    provider_id varchar not null,
    provider_key varchar not null,

    auth_info jsonb not null
);

create table if not exists documents (
    document_id varchar primary key,
    root_node_id varchar not null,
    created_time bigint not null,
    last_updated_time bigint not null,
    current_version bigint not null
);

create table if not exists permissions (
    user_id varchar not null references users(user_id),
    document_id varchar not null references documents(document_id),
    permission_level integer not null
);


create table if not exists nodes (
    document_id varchar not null references documents(document_id),
    node_id varchar primary key,
    created_time bigint not null,
    last_updated_time bigint not null,
    childs varchar[],
    attrs jsonb not null,
    cont jsonb not null,
    creator_id varchar not null references users(user_id)
);


create table if not exists changes (
    document_id varchar not null references documents(document_id),
    from_version bigint not null,
    updated_time bigint not null,
    cont bytea not null
);

# --- !Downs

drop table changes;
drop table nodes;
drop table permissions;
drop table documents;
drop table users;

