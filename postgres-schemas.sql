CREATE TABLE "curriculum" (
  "instructor" varchar(30) NOT NULL,
  "curriculum" varchar(20) NOT NULL,
  PRIMARY KEY ("instructor")
);

CREATE TABLE "passing_grade" (
  "grade" varchar(10) NOT NULL
);

CREATE TABLE "raw_grade" (
  "id" varchar(20) NOT NULL,
  "qtr" integer NOT NULL,
  "subject" varchar(20) NOT NULL,
  "num" varchar(20) NOT NULL,
  "grade" varchar(10) NOT NULL,
  "version" varchar(30) NOT NULL,
  PRIMARY KEY ("id","qtr","subject","num","version")
);

CREATE TABLE "123_instructor" (
  "id" varchar(20) NOT NULL,
  "qtr" integer NOT NULL,
  "instructor" varchar(30) NOT NULL,
  "version" varchar(30) NOT NULL,
  PRIMARY KEY (id,qtr,version)
);


-- the 'id' here is obtained via a secure hash from the emplid, using a cryptographically secure key

CREATE TABLE "ap_score" (
  "id" varchar(20) NOT NULL,
  "test" varchar(20) NOT NULL,
  "score" integer NOT NULL,
  "version" varchar(20) NOT NULL,
  PRIMARY KEY("id","version")
);

-- only interested in first taking of any class

CREATE VIEW first_time_qtr AS
  (SELECT id,min(qtr) qtr,subject,num FROM
  raw_grade GROUP BY id,subject,num
);

CREATE VIEW first_time_grade AS
  (SELECT g.id,g.qtr,g.subject,g.num,grade FROM
    (first_time_qtr ft LEFT JOIN raw_grade g
      ON ft.id=g.id AND ft.qtr=g.qtr AND ft.subject=g.subject
      AND ft.num=g.num));

CREATE TABLE grades_after_123 AS
  (SELECT f2.id,f2.qtr,f2.subject,f2.num,f2.grade FROM
    (first_time_grade f1 INNER JOIN first_time_grade f2
      ON f1.id=f2.id) WHERE f1.num='123' AND f1.qtr < f2.qtr
      AND f2.num != '123');


-- views:

CREATE VIEW first_101_qtr AS (SELECT id,MIN(qtr) qtr FROM raw_grade WHERE num='101' GROUP BY id);
CREATE VIEW first_101_grade AS (SELECT g.id,g.qtr,grade FROM (first_101_qtr fq LEFT JOIN raw_grade g ON fq.id=g.id AND fq.qtr=g.qtr) WHERE num='101');
-- same for 102 & 103

CREATE VIEW took_class_gt_102 AS (SELECT id FROM grades_after_123 WHERE num ='357' OR num='103' OR num='348' OR num='349' OR num='225' GROUP BY id);
CREATE VIEW took_class_gt_101 AS (SELECT id FROM grades_after_123 WHERE num ='357' OR num='103' OR num='348' OR num='349' OR num='225' OR num='102' GROUP BY id);
CREATE VIEW took_class_gt_123 AS (SELECT id FROM grades_after_123 WHERE num ='357' OR num='103' OR num='348' OR num='349' OR num='225' OR num='102' OR num='101' GROUP BY id);


CREATE VIEW skipped_101 AS (SELECT id FROM took_class_gt_101
  WHERE id NOT IN (SELECT id FROM grades_after_123 WHERE num='101' GROUP BY id)
  GROUP BY id);

CREATE VIEW dropped_before_101 AS (SELECT id FROM "123_instructor"
  WHERE id NOT IN (SELECT id FROM took_class_gt_123)
    AND id IN (SELECT id FROM raw_grade));
CREATE VIEW dropped_before_102 AS (SELECT id FROM "123_instructor"
  WHERE id NOT IN (SELECT id FROM took_class_gt_101)
    AND id IN (SELECT id FROM raw_grade));
  
CREATE VIEW skipped_102 AS (SELECT id FROM took_class_gt_102
  WHERE id NOT IN (SELECT id FROM grades_after_123 WHERE num='102' GROUP BY id)
  GROUP BY id);

CREATE VIEW first_cs_class AS (SELECT id,MIN(qtr) qtr FROM
  raw_grade GROUP BY id);
CREATE VIEW passed_103 AS (SELECT id,MIN(qtr) qtr FROM
  raw_grade WHERE num='103' AND grade IN (SELECT grade FROM passing_grade)
  GROUP BY id);


