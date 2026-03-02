-- Save data
CREATE TABLE feeds_backup AS
SELECT *
FROM feeds;
CREATE TABLE feed_items_backup AS
SELECT *
FROM feed_items;
-- Drop originals
DROP TABLE feed_items;
DROP TABLE feeds;
-- Create with final names directly (no renaming)
CREATE TABLE IF NOT EXISTS digests (
  id INTEGER PRIMARY KEY,
  date DATETIME DEFAULT CURRENT_TIMESTAMP
);
CREATE TABLE feeds (
  id INTEGER PRIMARY KEY,
  title TEXT,
  url TEXT NOT NULL,
  created DATETIME DEFAULT CURRENT_TIMESTAMP
);
CREATE TABLE feed_items (
  link TEXT NOT NULL PRIMARY KEY,
  title TEXT NOT NULL,
  updated DATETIME DEFAULT CURRENT_TIMESTAMP,
  feed_id INTEGER NOT NULL,
  digest_id INTEGER,
  FOREIGN KEY (feed_id) REFERENCES feeds(id) ON DELETE CASCADE,
  FOREIGN KEY (digest_id) REFERENCES digests(id)
);
-- Restore data
INSERT INTO feeds (id, title, url, created)
SELECT id,
  title,
  url,
  created_at
FROM feeds_backup;
INSERT INTO feed_items (link, title, updated, feed_id, digest_id)
SELECT link,
  title,
  updated,
  feed_id,
  NULL
FROM feed_items_backup;
-- Clean up
DROP TABLE feeds_backup;
DROP TABLE feed_items_backup;
-- no null created
UPDATE feeds
SET created = CURRENT_TIMESTAMP
WHERE created IS NULL;