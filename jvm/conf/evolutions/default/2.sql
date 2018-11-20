# --- !Ups

alter table changes drop constraint if exists changes_pkey;
alter table changes add constraint changes_pkey PRIMARY KEY (document_id, change_id);

alter table permissions drop constraint if exists permissions_pkey;
alter table permissions add constraint permissions_pkey PRIMARY KEY (user_id, document_id);

alter table nodes drop constraint nodes_pkey;
alter table nodes add constraint nodes_pkey PRIMARY KEY (document_id, node_id);

# --- !Downs




