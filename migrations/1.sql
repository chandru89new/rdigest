alter table feeds
add column title TEXT;
alter table feeds
add column created_at DATETIME;
update feeds
set created_at = CURRENT_TIMESTAMP
where created_at is null;