create table "linear_organizations" (
    "id" uuid primary key unique default uuid_generate_v1mc (),
    "workspace_id" uuid not null,
    "linear_id" varchar not null,
    "url_key" varchar not null,
    "display_name" varchar not null,
    "created_at" timestamp with time zone not null,
    "updated_at" timestamp with time zone not null
);

alter table "linear_organizations"
    add constraint "unique_linear_organization" unique ("workspace_id", "linear_id");

alter table "linear_organizations"
    add constraint "unique_one_linear_organization_per_tenant" unique ("workspace_id");

alter table "linear_organizations"
    add constraint "linear_organizations_workspace_id_fkey" foreign key
	("workspace_id") references "workspaces" ("id") on delete restrict on
	update restrict;

create trigger linear_organizations_insert
  before insert
  on linear_organizations
  for each row
execute procedure create_timestamps();

create trigger linear_organizations_update
  before update
  on linear_organizations
  for each row
execute procedure update_timestamps();


create table "linear_api_auth_sessions" (
    "id" uuid primary key unique default uuid_generate_v1mc (),
    "linear_organization_id" uuid not null,
    "token" varchar not null,
    "expires_at" timestamp with time zone null,
    "created_at" timestamp with time zone not null,
    "updated_at" timestamp with time zone not null
);

alter table "linear_api_auth_sessions"
    add constraint "unique_linear_auth_session" unique ("linear_organization_id");

alter table "linear_api_auth_sessions"
    add constraint "linear_api_auth_sessions_linear_organization_id_fkey" foreign key
	("linear_organization_id") references "linear_organizations" ("id") on delete restrict on
	update restrict;

create trigger linear_api_auth_sessions_insert
  before insert
  on linear_api_auth_sessions
  for each row
execute procedure create_timestamps();

create trigger linear_api_auth_sessions_update
  before update
  on linear_api_auth_sessions
  for each row
execute procedure update_timestamps();


create table "linear_nonces" (
    "id" uuid primary key unique default uuid_generate_v1mc (),
    "nonce_value" bytea not null,
    "workspace_id" uuid not null,
    "expires_at" timestamp with time zone not null
);

alter table "linear_nonces"
    add constraint "linear_nonces_nonce_value_key" unique ("nonce_value");

alter table "linear_nonces"
    add constraint "linear_nonces_workspace_id_fkey" foreign key
	("workspace_id") references "workspaces" ("id") on delete restrict on
	update restrict;


create table "linear_teams" (
    "id" uuid primary key unique default uuid_generate_v1mc (),
    "linear_organization_id" uuid not null,
    "url_key" varchar not null,
    "created_at" timestamp with time zone not null,
    "updated_at" timestamp with time zone not null
);

alter table "linear_teams"
    add constraint "unique_linear_team_url_key" unique ("linear_organization_id", "url_key");

alter table "linear_teams"
    add constraint "linear_teams_linear_organization_id_fkey" foreign key
	("linear_organization_id") references "linear_organizations" ("id") on delete restrict on
	update restrict;

create trigger linear_teams_insert
  before insert
  on linear_teams
  for each row
execute procedure create_timestamps();

create trigger linear_teams_update
  before update
  on linear_teams
  for each row
execute procedure update_timestamps();


create table "linked_linear_tickets" (
    "id" uuid primary key unique default uuid_generate_v1mc (),
    "linear_organization_id" uuid not null,
    "linear_ticket_id" varchar not null,
    "joined_channel_id" uuid not null,
    "known_user_id" uuid not null,
    "message_ts" varchar not null,
    "thread_ts" varchar null,
    "created_at" timestamp with time zone not null,
    "updated_at" timestamp with time zone not null
);

alter table "linked_linear_tickets"
    add constraint "unique_linked_linear_ticket" unique
	("linear_organization_id", "linear_ticket_id", "joined_channel_id",
	"message_ts", "thread_ts");

alter table "linked_linear_tickets"
    add constraint "linked_linear_tickets_linear_organization_id_fkey" foreign key
	("linear_organization_id") references "linear_organizations" ("id") on delete restrict on
	update restrict;

alter table "linked_linear_tickets"
    add constraint "linked_linear_tickets_joined_channel_id_fkey" foreign key
	("joined_channel_id") references "joined_channels" ("id") on delete
	restrict on update restrict;

alter table "linked_linear_tickets"
    add constraint "linked_linear_tickets_known_user_id_fkey" foreign key
	("known_user_id") references "known_users" ("id") on delete restrict
	on update restrict;

create trigger linked_linear_tickets_insert
  before insert
  on linked_linear_tickets
  for each row
execute procedure create_timestamps();

create trigger linked_linear_tickets_update
  before update
  on linked_linear_tickets
  for each row
execute procedure update_timestamps();
