# rdigest

A tool I am building to learn Haskell and manage my online reading/content consumption activity better.

## What does this do?

It's an RSS reader — but it just creates "daily digests" like these from the feeds I want:

![](https://images2.imgbox.com/1d/ca/t7iIGCrp_o.png)

This makes it easy for me to skim through stuff and pick the ones I want to read on a day (or between date ranges).

## How to run this?

- This is a Haskell project, so you'd need Haskell stuff to run this. (pre-built binaries are not yet a thing in this project but they're part of the [roadmap](./#roadmap))
- Install `ghcup` — [some helpful instructions here](https://www.haskell.org/ghcup/install/)
- Install the latest recommended version of `ghc` and `cabal` through `ghcup`. Running `ghcup help` would point you in the right direction
- Clone this project
- In terminal, at the root of this project, run `cabal build && cabal install`. This will build an executable called `rdigest`. You can simply call `rdigest` after that to run this tool.

## rdigest commands

`rdigest` has a few commands.

Make sure you set up an `RDIGEST_FOLDER` environment variable before you use `rdigest`. This tool will create a SQLite file, and when you want the daily digests, it will create them too and it needs a path/folder to put these in.

```sh
> export RDIGEST_FOLDER=<path-to-where-rdigest-data-should-be-stored>
```

```
> rdigest help -- prints help
> rdigest add <feed_url> -- adds an RSS feed, and will immediately get posts from that feed and add to the database
> rdigest remove <feed_url> -- removes the feed and deletes all posts that belong to that feed
> rdigest list feeds -- show all feeds
> rdigest refresh -- refresh all feeds, get all their posts/content and update the database. No duplicate content will be added though.
> rdigest digest -- create today's digest.
> rdigest digest --from YYYY-MM-DD --to YYYY-MM-DD -- create a digest between the date range
> rdigest refresh <feed_url> -- just update one feed
> rdigest purge -- delete everything, remove the database. This won't delete the digests.
```

## Roadmap

- [ ] spin up a server, for a UI-driven rdigest
- [ ] create pre-built binaries for Mac and Linux for release
