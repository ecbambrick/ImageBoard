DROP TABLE IF EXISTS post;
CREATE TABLE post (
    id              INTEGER     PRIMARY KEY AUTOINCREMENT,
    title           TEXT,
    is_favourite    INTEGER     NOT NULL,
    created         TIMESTAMP   NOT NULL DEFAULT CURRENT_TIMESTAMP,
    modified        TIMESTAMP   NOT NULL DEFAULT CURRENT_TIMESTAMP
);

DROP TABLE IF EXISTS image;
CREATE TABLE image (
    id          INTEGER     PRIMARY KEY AUTOINCREMENT,
    post_id     INTEGER     NOT NULL,
    hash        TEXT        NOT NULL,
    width       INTEGER     NOT NULL,
    height      INTEGER     NOT NULL,
    file_size   INTEGER     NOT NULL,
    extension   TEXT        NOT NULL,
    
    FOREIGN KEY (post_id) REFERENCES post (id) 
        ON UPDATE CASCADE 
        ON DELETE CASCADE
);

DROP TABLE IF EXISTS gallery;
CREATE TABLE gallery (
    id      INTEGER     PRIMARY KEY AUTOINCREMENT,
    post_id INTEGER     NOT NULL,
    
    FOREIGN KEY (post_id) REFERENCES post (id)
        ON UPDATE CASCADE 
        ON DELETE CASCADE
);

DROP TABLE IF EXISTS category;
CREATE TABLE category (
    id          INTEGER     PRIMARY KEY AUTOINCREMENT,
    name        TEXT        UNIQUE NOT NULL
);

DROP TABLE IF EXISTS tag;
CREATE TABLE tag (
    id      INTEGER     PRIMARY KEY AUTOINCREMENT,
    name    TEXT        UNIQUE NOT NULL
);

DROP TABLE IF EXISTS gallery_image;
CREATE TABLE gallery_image (
    gallery_id  INTEGER     NOT NULL,
    image_id    INTEGER     NOT NULL,
    page_number INTEGER     NOT NULL,
    page_title  TEXT        NOT NULL,
    
    PRIMARY KEY (gallery_id, image_id),
    
    FOREIGN KEY (gallery_id) REFERENCES gallery(id) 
        ON UPDATE CASCADE 
        ON DELETE CASCADE,
        
    FOREIGN KEY (image_id) REFERENCES image(id) 
        ON UPDATE CASCADE 
        ON DELETE CASCADE
);

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

CREATE INDEX width_height_index ON image(width, height);
