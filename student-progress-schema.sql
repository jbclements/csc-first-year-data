-- the 'id' here is obtained via a secure hash from the emplid, using a cryptographically secure key
-- the 'course' here is a course id as produced by csse-scheduling/canonicalize.rkt
-- bizarrely, there's no primary key; you can take the same class twice in the same quarter for different grades.
-- e.g., 2148 CSC S490

CREATE TABLE "course_grade" (
  "id" varchar(20) NOT NULL,
  "qtr" integer,
  "course" varchar(20),
  "grade" varchar(10)
);
