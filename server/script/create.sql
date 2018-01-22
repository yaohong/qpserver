create database if not exists qp;
use qp;
create table if not exists `account` (
	`user_id` bigint(20) NOT NULL AUTO_INCREMENT,
  #`wx_id` varchar(64) NOT NULL,
	`acc` varchar(64) NOT NULL,
	`pwd` varchar(32) DEFAULT '',
	`time` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
	PRIMARY KEY (`user_id`),
  #UNIQUE KEY `UK_wx_id` (`wx_id`),
  UNIQUE KEY `UK_acc` (`acc`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8, AUTO_INCREMENT=100000;



create table if not exists `game_data` (
  `user_id` bigint(20) NOT NULL,
  `room_card` bigint(20) NOT NULL,
  `nickname` varchar(32) NOT NULL DEFAULT '',
  `avatar_url` varchar(128) NOT NULL DEFAULT  '',
  PRIMARY KEY (`user_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;