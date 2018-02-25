-- the 'id' here is obtained via a secure hash from the emplid, using a cryptographically secure key

CREATE TABLE "ap_score" (
  "id" varchar(20) NOT NULL,
  "test" varchar(20) NOT NULL,
  "score" integer NOT NULL,
  PRIMARY KEY("id")
);

CREATE TABLE "class_grade" (
  "id" varchar(20) NOT NULL,
  "qtr" integer,
  "subject" varchar(10),
  "num" varchar(10),
  "grade" varchar(10)
  PRIMARY KEY ("id","qtr","subject","num")
);

