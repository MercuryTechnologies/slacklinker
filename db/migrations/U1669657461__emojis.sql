alter table "linked_messages"
    add column "user_id" uuid null;

create table "users" (
    "id" uuid primary key unique default uuid_generate_v1mc (),
    "workspace_id" uuid not null,
    "slack_user_id" varchar not null,
    "emoji" varchar null,
    "created_at" timestamp with time zone not null,
    "updated_at" timestamp with time zone not null
);

alter table "users"
    add constraint "users_workspace_slack_user_key" unique ("workspace_id",
	"slack_user_id");

alter table "users"
    add constraint "users_workspace_id_fkey" foreign key ("workspace_id")
	references "workspaces" ("id") on delete restrict on update restrict;
