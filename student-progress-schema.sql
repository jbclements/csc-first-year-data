-- the 'id' here is obtained via a secure hash from the emplid, using a cryptographically secure key
-- the 'course' here is a course id as produced by csse-scheduling/canonicalize.rkt
-- bizarrely, there's no primary key; you can take the same class twice in the same quarter for different grades.
-- e.g., 2148 CSC S490

CREATE TABLE "course_grade" (
  "id" varchar(20) NOT NULL,
  "qtr" integer NOT NULL,
  "course" varchar(20) NOT NULL,
  "grade" varchar(10) NOT NULL
);

CREATE TABLE "majors" (
  "id" varchar(20) NOT NULL,
  "qtr" integer NOT NULL,
  "major" varchar(20) NOT NULL,
  PRIMARY KEY ("id","qtr")
);

CREATE VIEW "entry_qtrs"
  AS SELECT id,MIN(qtr) FROM course_grade GROUP BY id;

-- this will be broken when we include failing grades...
CREATE TEMPORARY VIEW "passed_308"
  AS SELECT id FROM course_grade WHERE course='csc308' GROUP BY id;

SELECT entry_qtrs.id,min FROM (entry_qtrs INNER JOIN majors ON entry_qtrs.id = majors.id)
  WHERE entry_qtrs.id NOT IN (SELECT id FROM passed_308)
   AND major = 'SE'
   AND min < 2158;


GROUP BY min
   ORDER BY min;


AND min <= 2158;
  
  
