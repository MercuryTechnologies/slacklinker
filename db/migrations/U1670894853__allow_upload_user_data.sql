alter table "known_users"
    add column "email" VARCHAR null;

alter type "slacklinker_setting_tag" add value 'AllowUploadUserData';

create index known_users_email_idx on known_users (email);

alter table "known_users"
    add column "github_username" VARCHAR null;
