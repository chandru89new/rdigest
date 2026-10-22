# rdigest - Changelog

## v0.5.2

- Fix markAsSeen DB call to set pragmas.

## v0.5.1

- Cleanup Model and Msgs and use Http.task for sequencing latest digest.
- Add 'Hide Seen' toggle.
- Remove styles.css and gitignore generated files for UI.

## v0.5.0

- Add 'seen_status' column to feed_items table.
- Add 'mark as seen' feature so feed items / links can be marked as seen/read.

## v0.4.7

- UI improvements.

## v0.4.6

- Add count to digests list.

## v0.4.5

- Fix ambiguity in `show` import.

## v0.4.4

- Minor UI change to make header-nav sticky.

## v0.4.3

- Erroneaous inputs exit with non-zero exit code.

## v0.4.2

- Update user-agent to fix feed extraction from Reddit.

## v0.4.1

- Import/export feeds via CLI.

## v0.4.0

- Local server to view digests, import OPML, manage feeds.
- Add a small migration support module for future updates to DB.
- Lotsa cleanup.

## v0.3.3

- Check for duplicates before trying to add a feed.

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
