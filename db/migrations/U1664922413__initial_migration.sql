create table "workspaces" (
    "id" uuid primary key unique default uuid_generate_v1mc (),
    "slack_subdomain" varchar not null,
    "slack_team_id" varchar not null,
    "slack_oauth_token" varchar not null
);

alter table "workspaces"
    add constraint "workspaces_slack_id_key" unique ("slack_team_id");

create table "joined_channels" (
    "id" uuid primary key unique default uuid_generate_v1mc (),
    "workspace_id" uuid not null,
    "channel_id" varchar not null
);

alter table "joined_channels"
    add constraint "joined_channels_workspace_channel_key" unique
    ("workspace_id", "channel_id");

alter table "joined_channels"
    add constraint "joined_channels_workspace_id_fkey" foreign key
    ("workspace_id") references "workspaces" ("id") on delete restrict on
    update restrict;

create table "nonces" (
    "id" uuid primary key unique default uuid_generate_v1mc (),
    "nonce_value" bytea not null,
    "expires_at" timestamp with time zone not null
);

alter table "nonces"
    add constraint "nonces_nonce_value_key" unique ("nonce_value");

create type "slacklinker_setting_tag" as enum ('AllowRegistration', 'RequireMutualTLS');

create table "settings" (
    "id" uuid primary key unique default uuid_generate_v1mc (),
    "tag" slacklinker_setting_tag not null,
    "content" jsonb not null
);

alter table "settings"
    add constraint "settings_tag_key" unique ("tag");

