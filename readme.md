# rdigest

Create "daily digests" of your (RSS) feeds.

Started as a project to build something in Haskell ("learn by building"), has grown into a tool I use to manage my daily catch-up of internet links.

## What does this do?

## How to run this?

- Grab the executable from the [releases](https://github.com/chandru89new/rdigest/releases/latest). At the time of writing this, binaries are built for MacOS (latest, M\* architecture) and Linux machines (x86-64) only but this should be sufficient unless you're running Windows.

- Make the executable "executable":

```sh
> tar -xzf rdigest_darwin_arm64_v0.3.1.tar.gz # you have to change this to point to the correct file that you downloaded from the release
> chmod +x rdigest
```

- Set env var specifying which folder `rdigest` should use:

```sh
> export RDIGEST_FOLDER=./path/to/folder
```

- Run `rdigest init` to get started:

```sh
> rdigest init
```

## rdigest commands

`rdigest` has a few commands.

Make sure you set up an `RDIGEST_FOLDER` environment variable before you use `rdigest`. This tool will create a SQLite file, and when you want the daily digests, it will create them too and it needs a path/folder to put these in.

```sh
> export RDIGEST_FOLDER=<path-to-where-rdigest-data-should-be-stored>
```

```bash
# prints help
> rdigest help

# adds an RSS feed, and will immediately get posts from that feed and add to the database
> rdigest add <feed_url>

# removes the feed and deletes all posts that belong to that feed
> rdigest remove <feed_url>

# show all feeds
> rdigest list feeds

# refresh all feeds, get all their posts/content and update the database. No duplicate content will be added though.
> rdigest refresh

# create today's digest.
> rdigest digest

# create a digest between the date range
> rdigest digest --from YYYY-MM-DD --to YYYY-MM-DD

# just update one feed
> rdigest refresh <feed_url>

# send notifications to Telegram channel
> rdigest notify
# check the Telegram Notification section for more info

# delete everything, remove the database. This won't delete the digests.
> rdigest purge
```

## Telegram Notification

`rdigest` can, if needed, send the daily digest links as messages to a Telegram chat/channel.

To do this:

- create a Telegram bot and grab the bot's token.
- create a Telegram channel and get the channel's ID (usually a negative integer)
- invite the bot you created into the channel you created with Admin-like privileges (ie, the bot should be able to post messages to the channel)
- in your terminal where you run `rdigest`, set these env vars:

```sh
> export TG_TOKEN=<bot token>
> export TG_CHANNEL_ID=<the channel id>
```

- get rdigest to send the notifications:

```sh
> rdigest notify
```

Links that are sent as notifications will be marked, so they won't be sent again.

Note: Due to Telegram's rate limits, `rdigest` will space-out batches of notifications, with ~1 minute wait time between sending out each batch of 20 or so notifications.

## Roadmap

- [x] create pre-built binaries for Linux for release
- [x] create pre-built binaries for Mac for release
- [x] add TG bot notification support
- [ ] add GUI
