# Slacklinker: a slack backlink bot

Configuration:

Use environment variables:

- SLACK_CLIENT_SECRET
- SLACK_SIGNING_SECRET
- SLACK_CLIENT_ID
- POSTGRES_CONNECTION_STRING

You can set some runtime configuration settings using `one-off-task set-setting`.
A full list of these is in
[`src/Slacklinker/Settings/Types.hs`](src/Slacklinker/Settings/Types.hs).

## Development notes

Use `scripts/update-golden.sh SOME_GOLDEN_DIR` to update golden files.
