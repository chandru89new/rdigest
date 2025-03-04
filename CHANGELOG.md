# rdigest - Changelog

## v0.3.2

- Fix an unhandled error condition when sending a telegram notification fails.

## v0.3.0

- Different notification strategy to work around msging limits on Telegram.

## v0.2.1

- Use JSON instead of plain text during HTTP calls to TG notification.

## v0.2.0

Some big, spicy change here.

- Add the ability to "notify": this sends digest links to a channel on Telegram. Needs TG_TOKEN and TG_CHANNEL_ID env vars to be set for this to work.
- Change the default value for 'state' column to be 'unsent' instead of 'unread'.

## v0.1.7

- update template to be mobile-friendly.
- make html template `<details>` be open by default (revert).

## v0.1.6

- make the html template `<details>` be closed by default.

## v0.1.5

- forgot to add changelog here. no idea what got changed. but it's something really `minor` (obviously!).

## v0.1.4

- use `Paths_*` to get version dynamically from the .cabal file.
