# Slacklinker: a Slack backlink bot

![screenshot of slacklinker replying to a message "I just pushed a PR that
definitely works, for sure" with a link to a message linking to it saying "Oh
no, prod is on fire"](doc/slacklinker-demo.png)

This is a Slack bot that creates backlinks when you link messages. It's written
using the Servant web framework for Haskell, and the [slack-web] Slack library.

[slack-web]: https://github.com/MercuryTechnologies/slack-web

## Configuration

### Slack setup

Here is the app manifest you can use to set up the Slack app on the [Slack API
site]:

[Slack API site]: https://api.slack.com/apps

```yaml
display_information:
  name: Slacklinker
features:
  bot_user:
    display_name: Slacklinker
    always_online: true
oauth_config:
  redirect_urls:
    - https://YOUR_SERVICE/oauth_redirect
  scopes:
    bot:
      - channels:history
      - channels:join
      - channels:read
      - chat:write
      - files:read
      - im:history
      - users:read
      - users:read.email
settings:
  event_subscriptions:
    request_url: https://YOUR_SERVICE/webhook
    bot_events:
      - channel_created
      - channel_left
      - message.channels
      - message.im
  org_deploy_enabled: false
  socket_mode_enabled: false
  token_rotation_enabled: false
```

Additionally, on the "App Home" tab, you have to enable "Messages tab" and
"Allow users to send Slash commands and messages from the messages tab".

### App setup

This app is developed and deployed using Nix. You can get a development shell
with haskell-language-server and all the tools you need with `nix develop`.

To deploy it, you can equivalently use the `packages.${system}.default`
attribute from `flake.nix` or the `build` attribute of `release.nix` (which are
identical). Before starting the application (`result/bin/slacklinker`), your
service manager should run `result/db/migrate.sh` to run the database
migrations.

Some basic configuration is done via environment variables:

Corresponding to the values in the "Basic Information" pane of the [Slack API site]:
- `SLACK_CLIENT_SECRET`
- `SLACK_SIGNING_SECRET`
- `SLACK_CLIENT_ID`

Corresponding to your database:
- `POSTGRES_CONNECTION_STRING` is a Postgres connection string like
  `postgresql://slacklinker:yourpassword@localhost:5432/slacklinker`

Corresponding to your OpenTelemetry tracing service (this is the recommended
way to debug and monitor Slacklinker):
- `OTEL_SERVICE_NAME=slacklinker`
- `OTEL_EXPORTER_OTLP_ENDPOINT` (for Honeycomb, `https://api.honeycomb.io`)
- `OTEL_EXPORTER_OTLP_HEADERS` (for Honeycomb, `x-honeycomb-team=YOUR-API-KEY`)

You can set some runtime configuration settings in the database using
`one-off-task set-setting`. A full list of these is in
[`src/Slacklinker/Settings/Types.hs`](src/Slacklinker/Settings/Types.hs).

Make Slacklinker not backlink to posts by these apps. This is helpful in making
sure that Slacklinker won't backlink to itself. It is recommended to bput 
Slacklinker's own app id here.
- `BLOCKED_APP_IDS=appid1,appid2

### Usage

Once you have the application running at some public URL, you need to authorize
it with OAuth2.

To do this, go to `https://YOUR_SERVICE/authorize`. You will be redirected to
Slack to authorize the application.

After authorizing Slacklinker on your workspace, unless you intend to run a
public instance, you should immediately disable `AllowRegistration`:

```
bin/one-off-task set-setting --settingName AllowRegistration --value false
```

(note that one-off-task needs to have the same environment variables as the
service)

Once Slacklinker is authorized, you can send the bot a private message
`join_all`, which will have the bot join all existing public non-shared Slack
channels.

The bot will automatically join any newly created public channels, so no
further action is needed.

## Development notes

Since [slack-web] does not (currently) support Slack's [Socket Mode], you
*need* a public request URL to run Slacklinker. This is most easily achieved
with something like [ngrok] in development and your preferred infrastructure in
production.

[Socket Mode]: https://api.slack.com/apis/connections/socket
[ngrok]: https://ngrok.com/

We recommend using [`direnv`][direnv] and [`nix-direnv`][nix-direnv] to get a
working environment for working on slacklinker. You can see a sample for
`.envrc` at [./.envrc.sample](./.envrc.sample).

[nix-direnv]: https://github.com/nix-community/nix-direnv
[direnv]: https://direnv.net/

### Schema changes & Migrations

If you do a database schema change, you will need to generate a migration.
These use the [Refinery] CLI, which basically just runs SQL. You can get
Persistent to generate the outline of the migration like so:

```
$ cabal run one-off-task -- suggest-migrations --migrationName your_migration_name
```

[Refinery]: https://github.com/rust-db/refinery

### Golden tests

Use `scripts/update-golden.sh SOME_GOLDEN_DIR` to update golden snapshot test
files.

### Logging

Set log level with `LOG_LEVEL=debug` and SQL log level with `LOG_SQL=debug`
environment variables.

### Updating dependencies

Some Haskell dependencies (such as tmp-postgress and slack-web) are overriden from source in [./nix/deps](https://github.com/MercuryTechnologies/slacklinker/tree/main/nix/deps). To update, use `cabal2nix` (available in your dev shell via nix develop).

```
cabal2nix "${GITHUB_URI}" > "./nix/deps/${PACKAGE_NAME}.nix"
```

That command will grab the latest git head from the repo. To get a specific version you can use `--revision`.