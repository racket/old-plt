-- For PostgreSQL.
-- Also needs contrib/pgcrypto.

DROP TABLE assignment_grades;
DROP TABLE assignments;
DROP TABLE course_people;
DROP TABLE partners;
DROP TABLE courses;
DROP TABLE people;

-- Members (people)
CREATE TABLE people (
    id     SERIAL PRIMARY KEY,
    -- Their name
    name   TEXT,
    -- Their Northeastern ID
    neu_id INTEGER,
    -- Log in name
    username TEXT UNIQUE,
    -- Secret phrase
    password TEXT
    );

-- Courses
CREATE TABLE courses (
    id     SERIAL  PRIMARY KEY,
    -- The name of the course
    name   TEXT,       
    -- The Northeastern number for the course
    number VARCHAR (6),
    -- Where submissions go
    directory TEXT NOT NULL,
    -- The size of a normal acceptable partnership
    default_partnership_size INTEGER
    );

-- Partnerships
CREATE TABLE partners (
    id         SERIAL    PRIMARY KEY,
    -- A student in a partnership
    student_id INTEGER   REFERENCES people (id)
                         ON DELETE CASCADE
                         ON UPDATE CASCADE,
    -- An ID, unique to a partnership
    partner_id INTEGER,
    -- The course this is for
    course_id  INTEGER   REFERENCES courses (id)
                         ON DELETE CASCADE
                         ON UPDATE CASCADE,
    -- When the partnership was formed
    created    TIMESTAMP,
    -- When the partnership died
    ended      TIMESTAMP
    );

-- Many-to-many: courses <=> people
CREATE TABLE course_people (
    id        SERIAL  PRIMARY KEY,
    -- The person
    person_id INTEGER REFERENCES people (id)
                      ON DELETE CASCADE
                      ON UPDATE CASCADE,
    -- The course
    course_id INTEGER REFERENCES courses (id)
                      ON DELETE CASCADE
                      ON UPDATE CASCADE,
    -- Their position in the course
    position  TEXT    CHECK (position LIKE 'student' OR
      position LIKE 'grader' OR
      position LIKE 'instructor')
    );


-- Assignments
CREATE TABLE assignments (
    id              SERIAL  PRIMARY KEY,
    -- When it needs to be in
    due             TIMESTAMP,
    -- The name of the assignment
    name            TEXT,
    -- The description of the assignment
    description     TEXT,
    -- The URL of the full assignment
    description_url TEXT,
    -- Which course it's for
    course_id       INTEGER REFERENCES courses (id)
                            ON DELETE CASCADE
                            ON UPDATE CASCADE,
    -- The grade type
    grade_type      TEXT    CHECK (grade_type LIKE 'letter' OR
                                   grade_type LIKE 'percentage' OR
                                   grade_type LIKE 'pass/fail' OR
                                   grade_type LIKE 'number'),
    -- Anything extra about the grade e.g. "out of 80 points"
    grade_misc      TEXT
    );

-- Passed-in assignments, graded or ungraded
CREATE TABLE assignment_grades (
    id              SERIAL    PRIMARY KEY,
    -- Which assignment
    assignment_id   INTEGER   REFERENCES assignments (id)
                              ON DELETE CASCADE
                              ON UPDATE CASCADE,
    -- Which group passed it in. REFERENCES don't work on non-UNIQUE fields
    partner_id      INTEGER,
    -- The grade, based on assignments/grade_type
    grade           TEXT,
    -- When it was received
    submission_date TIMESTAMP DEFAULT current_timestamp,
    -- The name of the file or directory submitted
    submission      TEXT,
    -- Anything the grader wants
    comment         TEXT
    );
