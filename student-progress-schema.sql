-- the 'id' here is obtained via a secure hash from the emplid, using a cryptographically secure key
-- the 'course' here is a course id as produced by csse-scheduling/canonicalize.rkt

CREATE TABLE "class_grade" (
  "id" varchar(20) NOT NULL,
  "qtr" integer,
  "course" varchar(20),
  "grade" varchar(10)
  PRIMARY KEY ("id","qtr","course")
);
