create or replace function create_timestamps()
  returns trigger as
$$
begin
  new.created_at = now();
  new.updated_at = now();
  return new;
end
$$ language 'plpgsql';

create or replace function update_timestamps()
  returns trigger as
$$
begin
  new.updated_at = now();
  return new;
end
$$ language 'plpgsql';

alter table "joined_channels"
  add column "created_at" TIMESTAMP with time zone not null default now();

alter table "joined_channels"
  add column "updated_at" TIMESTAMP with time zone not null default now();

alter table "settings"
  add column "created_at" TIMESTAMP with time zone not null default now();

alter table "settings"
  add column "updated_at" TIMESTAMP with time zone not null default now();

alter table "workspaces"
  add column "created_at" TIMESTAMP with time zone not null default now();

alter table "workspaces"
  add column "updated_at" TIMESTAMP with time zone not null default now();

create trigger joined_channels_insert
  before insert
  on joined_channels
  for each row
execute procedure create_timestamps();

create trigger joined_channels_update
  before update
  on joined_channels
  for each row
execute procedure update_timestamps();

create trigger workspaces_insert
  before insert
  on workspaces
  for each row
execute procedure create_timestamps();

create trigger workspaces_update
  before update
  on workspaces
  for each row
execute procedure update_timestamps();

create trigger settings_insert
  before insert
  on settings
  for each row
execute procedure create_timestamps();

create trigger settings_update
  before update
  on settings
  for each row
execute procedure update_timestamps();

alter table settings alter column updated_at drop default;
alter table settings alter column created_at drop default;

alter table joined_channels alter column updated_at drop default;
alter table joined_channels alter column created_at drop default;

alter table workspaces alter column updated_at drop default;
alter table workspaces alter column created_at drop default;
