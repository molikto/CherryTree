# --- !Ups

create table users (
    user_id varchar not null primary key,
    name_ varchar not null,
    email varchar not null,
    avatar_url varchar,
    activated bool not null,

    provider_id varchar not null,
    provider_key varchar not null,

    auth_info jsonb not null
);

create table documents (
    document_id varchar not null primary key,
    owner_id varchar not null references users(user_id) /* owner can delete a document */
);

/* currently our access control is very simple, users have permission of a document has full access to that document */
create table permissions(
    user_id varchar not null references users(user_id),
    document_id varchar not null references documents(document_id)
)

create table document_node(
    document_id varchar not null references documents(document_id),
    node_id varchar not null, /* this is NOT unique, because a node can have multiple versions */
    from_version bigint not null,
    until_version bigint not null,
    parent_node_id varchar references document_node(node_id), /* null means root node */
    attrs jsonb, /* we store it as jsonb, but it is just a map(string -> string) */
    content
)

# --- !Downs

drop table users;

