# --- !Ups

CREATE TABLE "user" (
    "user_id" VARCHAR NOT NULL PRIMARY KEY,
    "name" VARCHAR,
    "email" VARCHAR,
    "avatar_url" VARCHAR,
    "activated" BIT,

    "provider_id" VARCHAR NOT NULL,
    "provider_key" VARCHAR NOT NULL,

    "auth_info" VARBINARY
);


# --- !Downs

DROP TABLE "user";