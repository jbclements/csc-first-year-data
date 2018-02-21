CREATE TABLE "raw_grade" (
  "id" varchar(20) NOT NULL,
  "qtr" integer NOT NULL,
  "subject" varchar(20) NOT NULL,
  "num" varchar(20) NOT NULL,
  "grade" varchar(10) NOT NULL,
  "version" varchar(30) NOT NULL
);

CREATE TABLE "123_instructor" (
  "id" varchar(20) NOT NULL,
  "qtr" integer NOT NULL,
  "instructor" varchar(30) NOT NULL,
  "version" varchar(30) NOT NULL,
  PRIMARY KEY (id,qtr,version)
);
