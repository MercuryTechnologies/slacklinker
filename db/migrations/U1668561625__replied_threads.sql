create table "replied_threads" (
  "id" uuid
    primary key
    unique default uuid_generate_v1mc(),
  "workspace_id" uuid not null,
  "reply_ts" varchar null,
  "conversation_id" varchar not null,
  "thread_ts" varchar not null,
  "created_at" timestamp with time zone not null,
  "updated_at" timestamp with time zone not null
);

alter table "replied_threads"
  add constraint "unique_replied_thread"
    unique ("workspace_id",
            "conversation_id", "thread_ts");

alter table "replied_threads"
  add constraint "replied_threads_workspace_id_fkey"
    foreign key
      ("workspace_id")
      references "workspaces" ("id")
      on delete restrict
      on
        update restrict;

create trigger replied_threads_insert
  before insert
  on replied_threads
  for each row
execute procedure create_timestamps();

create trigger replied_threads_update
  before update
  on replied_threads
  for each row
execute procedure update_timestamps();

create table "linked_messages" (
    "id" uuid primary key unique default uuid_generate_v1mc (),
    "replied_thread_id" uuid not null,
    "joined_channel_id" uuid not null,
    "message_ts" varchar not null,
    "thread_ts" varchar null,
    "sent" boolean not null,
    "created_at" timestamp with time zone not null,
    "updated_at" timestamp with time zone not null
);

alter table "linked_messages"
  add constraint "unique_linked_message"
    unique ("replied_thread_id",
            "joined_channel_id", "message_ts", "thread_ts");

-- Unique constraint behaviour in the presence of nulls is to allow
-- multiple nulls when the rest of the constraint matches. This is
-- what can best be described as "very surprising":
--
-- https://www.enterprisedb.com/postgres-tutorials/postgresql-unique-constraint-null-allowing-only-one-null
create unique index "unique_linked_message_null" on "linked_messages"
  ("replied_thread_id",
   "joined_channel_id", "message_ts",
   ("thread_ts" is null)) where "thread_ts" is null;

alter table "linked_messages"
  add constraint "linked_messages_replied_thread_id_fkey"
    foreign key
      ("replied_thread_id")
      references "replied_threads" ("id")
      on delete
        restrict
      on update restrict;

alter table "linked_messages"
    add constraint "linked_messages_joined_channel_id_fkey" foreign key
	("joined_channel_id") references "joined_channels" ("id") on delete
	restrict on update restrict;

create trigger linked_messages_insert
  before insert
  on linked_messages
  for each row
execute procedure create_timestamps();

create trigger linked_messages_update
  before update
  on linked_messages
  for each row
execute procedure update_timestamps();

create index "linked_messages_replied_thread_id_idx"
  on linked_messages using hash (replied_thread_id);
