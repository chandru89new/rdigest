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
> rdigest feeds add <feed_url>

# removes the feed and deletes all posts that belong to that feed
> rdigest feeds remove <feed_url>

# show all feeds
> rdigest feeds list

# refresh all feeds, get all their posts/content and update the database. No duplicate content will be added though.
> rdigest feeds update

# refresh just one particular feed
> rdigest feeds update <feed_url>

# create latest full digest. (latest full digest means the digest for the previous day)
> rdigest digest 2
# the number 2 is an offset. 1 = today. 2 = yesterday. 3 = day before yesterday and so on...

# view the upcoming digest
> rdigest digest

```

## Roadmap

- [x] create pre-built binaries for Linux for release
- [x] create pre-built binaries for Mac for release
- [x] add GUI
