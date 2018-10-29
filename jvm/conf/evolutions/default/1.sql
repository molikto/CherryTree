# --- !Ups

create table users (
    user_id varchar not null primary key,
    name_ varchar not null,
    email varchar not null,
    avatar_url varchar,
    activated bool,

    provider_id varchar not null,
    provider_key varchar not null,

    auth_info jsonb not null
);


# --- !Downs

drop table users;

