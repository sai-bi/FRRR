create database renren;

drop   user 'frrr'@'localhost';
create user 'frrr'@'localhost';
grant select, insert, delete on renren.* to 'frrr'@'localhost';
flush privileges;

use renren;

create table users (
  user_id bigint unsigned not null,
  name    varchar(255)    not null,
  network varchar(255)    not null,

  primary key (user_id)

) engine=InnoDB charset=utf8;

create table albums (
  user_id  bigint unsigned not null,
  album_id bigint unsigned not null,
  title    text            not null,
  private  bool            not null,

  foreign key (user_id) references users (user_id),
  primary key (user_id, album_id)

) engine=InnoDB charset=utf8;

create table photos (
  user_id      bigint unsigned not null,
  album_id     bigint unsigned not null,
  photo_id     bigint unsigned not null,
  caption      text            not null,
  url          text            not null,
  downloaded   bool            not null,
  cached_faces text            not null,

  foreign key (user_id) references users (user_id),
  primary key (user_id, photo_id)

) engine=InnoDB charset=utf8;

create table faces (
  face_id  bigint   unsigned not null,
  -- user ID of the owner of the photo, not that of the face
  user_id  bigint   unsigned not null,
  photo_id bigint   unsigned not null,
  x        smallint unsigned not null,
  y        smallint unsigned not null,
  width    smallint unsigned not null,
  height   smallint unsigned not null,

  foreign key (user_id) references users (user_id),
  primary key (face_id)

) engine=InnoDB charset=utf8;
