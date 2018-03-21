------------------------------------------------------------------------- Posts

DROP TABLE IF EXISTS post;
CREATE TABLE post (
    id              INTEGER     PRIMARY KEY AUTOINCREMENT,
    title           TEXT,
    is_favourite    INTEGER     NOT NULL,
    created         INTEGER     NOT NULL,
    modified        INTEGER     NOT NULL,
    is_deleted      INTEGER     NOT NULL
);

DROP TABLE IF EXISTS image;
CREATE TABLE image (
    id          INTEGER     PRIMARY KEY AUTOINCREMENT,
    post_id     INTEGER     NOT NULL,
    hash        TEXT        NOT NULL,
    file_size   INTEGER     NOT NULL,
    extension   TEXT        NOT NULL,
    width       INTEGER     NOT NULL,
    height      INTEGER     NOT NULL,
    is_animated INTEGER     NOT NULL,

    FOREIGN KEY (post_id) REFERENCES post (id)
        ON UPDATE CASCADE
        ON DELETE CASCADE
);

CREATE INDEX width_height_index ON image(width, height);

DROP TABLE IF EXISTS album;
CREATE TABLE album (
    id          INTEGER     PRIMARY KEY AUTOINCREMENT,
    post_id     INTEGER     NOT NULL,
    file_size   INTEGER     NOT NULL,

    FOREIGN KEY (post_id) REFERENCES post (id)
        ON UPDATE CASCADE
        ON DELETE CASCADE
);

DROP TABLE IF EXISTS page;
CREATE TABLE page (
    id          INTEGER     PRIMARY KEY AUTOINCREMENT,
    album_id    INTEGER     NOT NULL,
    number      INTEGER     NOT NULL,
    title       TEXT        NOT NULL,
    extension   TEXT        NOT NULL,

    FOREIGN KEY (album_id) REFERENCES album (id)
        ON UPDATE CASCADE
        ON DELETE CASCADE
);

CREATE UNIQUE INDEX page_position_index ON page (album_id, number);

-------------------------------------------------------------------------- Tags

DROP TABLE IF EXISTS category;
CREATE TABLE category (
    id          INTEGER     PRIMARY KEY AUTOINCREMENT,
    name        TEXT        UNIQUE NOT NULL
);

DROP TABLE IF EXISTS tag;
CREATE TABLE tag (
    id      INTEGER     PRIMARY KEY AUTOINCREMENT,
    name    TEXT        UNIQUE NOT NULL,
    created INTEGER     NOT NULL
);

CREATE INDEX tag_created_index ON tag(created);

DROP TABLE IF EXISTS post_tag;
CREATE TABLE post_tag (
    post_id INTEGER     NOT NULL,
    tag_id  INTEGER     NOT NULL,

    PRIMARY KEY (post_id, tag_id),

    FOREIGN KEY (post_id) REFERENCES post(id)
        ON UPDATE CASCADE
        ON DELETE CASCADE,

    FOREIGN KEY (tag_id) REFERENCES tag(id)
        ON UPDATE CASCADE
        ON DELETE CASCADE
);

DROP TABLE IF EXISTS tag_category;
CREATE TABLE tag_category (
    tag_id      INTEGER     NOT NULL,
    category_id INTEGER     NOT NULL,

    PRIMARY KEY(tag_id, category_id),

    FOREIGN KEY(tag_id) REFERENCES tag(id)
        ON UPDATE CASCADE
        ON DELETE CASCADE,

    FOREIGN KEY(category_id) REFERENCES category(id)
        ON UPDATE CASCADE
        ON DELETE CASCADE
);

----------------------------------------------------------------------- Sources

DROP TABLE IF EXISTS post_source;
CREATE TABLE post_source (
    id      INTEGER PRIMARY KEY AUTOINCREMENT,
    post_id INTEGER NOT NULL,
    url     TEXT    NOT NULL,
    
    FOREIGN KEY(post_id) REFERENCES post(id)
    	ON UPDATE CASCADE
    	ON DELETE CASCADE
);

------------------------------------------------------------------------ Scopes

DROP TABLE IF EXISTS scope;
CREATE TABLE scope (
    id          INTEGER     PRIMARY KEY AUTOINCREMENT,
    name        TEXT        UNIQUE NOT NULL,
    expression  TEXT        NOT NULL
);
