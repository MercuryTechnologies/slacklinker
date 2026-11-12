alter table "joined_channels"
    add column "track_github_prs" boolean not null default false;

create type "github_pr_status" as enum ('PrDraft', 'PrOpened', 'PrQueued', 'PrMerged', 'PrClosed');

create type "github_pr_review" as enum ('ReviewNone', 'ReviewApproved', 'ReviewChangesRequested');

create table "github_repos" (
    "id" uuid primary key unique default uuid_generate_v1mc (),
    "owner" varchar not null,
    "name" varchar not null,
    "created_at" timestamp with time zone not null,
    "updated_at" timestamp with time zone not null
);

alter table "github_repos"
    add constraint "unique_github_repo" unique ("owner", "name");

create trigger github_repos_insert
  before insert
  on github_repos
  for each row
execute procedure create_timestamps();

create trigger github_repos_update
  before update
  on github_repos
  for each row
execute procedure update_timestamps();


create table "github_pull_requests" (
    "id" uuid primary key unique default uuid_generate_v1mc (),
    "repo_id" uuid not null,
    "number" int8 not null,
    "created_at" timestamp with time zone not null,
    "updated_at" timestamp with time zone not null
);

alter table "github_pull_requests"
    add constraint "unique_github_pull_request" unique ("repo_id", "number");

alter table "github_pull_requests"
    add constraint "github_pull_requests_repo_id_fkey" foreign key
	("repo_id") references "github_repos" ("id") on delete restrict on
	update restrict;

create trigger github_pull_requests_insert
  before insert
  on github_pull_requests
  for each row
execute procedure create_timestamps();

create trigger github_pull_requests_update
  before update
  on github_pull_requests
  for each row
execute procedure update_timestamps();


create table "linked_github_pull_requests" (
    "id" uuid primary key unique default uuid_generate_v1mc (),
    "pull_request_id" uuid not null,
    "joined_channel_id" uuid not null,
    "message_ts" varchar not null,
    "applied_status" github_pr_status null,
    "applied_review" github_pr_review not null,
    "created_at" timestamp with time zone not null,
    "updated_at" timestamp with time zone not null
);

alter table "linked_github_pull_requests"
    add constraint "unique_linked_github_pull_request" unique
	("pull_request_id", "joined_channel_id", "message_ts");

alter table "linked_github_pull_requests"
    add constraint "linked_github_pull_requests_pull_request_id_fkey" foreign key
	("pull_request_id") references "github_pull_requests" ("id") on delete
	restrict on update restrict;

-- cascade: slacklinker leaving a channel deletes its joined_channels row
alter table "linked_github_pull_requests"
    add constraint "linked_github_pull_requests_joined_channel_id_fkey" foreign key
	("joined_channel_id") references "joined_channels" ("id") on delete
	cascade on update restrict;

create trigger linked_github_pull_requests_insert
  before insert
  on linked_github_pull_requests
  for each row
execute procedure create_timestamps();

create trigger linked_github_pull_requests_update
  before update
  on linked_github_pull_requests
  for each row
execute procedure update_timestamps();
