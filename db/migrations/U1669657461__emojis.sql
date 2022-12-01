alter table "linked_messages"
    add column "known_user_id" uuid null;

create table "known_users" (
    "id" uuid primary key unique default uuid_generate_v1mc (),
    "workspace_id" uuid not null,
    "slack_user_id" varchar not null,
    "emoji" varchar null,
    "created_at" timestamp with time zone not null,
    "updated_at" timestamp with time zone not null
);

alter table "linked_messages"
    add constraint "linked_messages_known_user_id_fkey" foreign key
	("known_user_id") references "known_users" ("id") on delete restrict
	on update restrict;

alter table "known_users"
    add constraint "known_users_workspace_slack_user_key" unique ("workspace_id",
	"slack_user_id");

alter table "known_users"
    add constraint "known_users_workspace_id_fkey" foreign key ("workspace_id")
	references "workspaces" ("id") on delete restrict on update restrict;


create trigger known_users_insert
  before insert
  on known_users
  for each row
execute procedure create_timestamps();

create trigger known_users_update
  before update
  on known_users
  for each row
execute procedure update_timestamps();
