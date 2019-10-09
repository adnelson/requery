DROP TABLE IF EXISTS authors CASCADE;
DROP TABLE IF EXISTS books CASCADE;
DROP TABLE IF EXISTS book_authors CASCADE;

CREATE TABLE authors (
  id SERIAL PRIMARY KEY,
  first_name TEXT NOT NULL,
  last_name TEXT NOT NULL,
  birth_year INT NOT NULL,
  death_year INT
);

CREATE TABLE books (
  id SERIAL PRIMARY KEY,
  title TEXT NOT NULL,
  year INT NOT NULL,
  genre TEXT NOT NULL
);

CREATE TABLE book_authors (
  author_id INT NOT NULL,
  book_id INT NOT NULL,
  PRIMARY KEY (author_id, book_id),
  FOREIGN KEY (author_id) REFERENCES authors(id) ON DELETE CASCADE,
  FOREIGN KEY (book_id) REFERENCES books(id) ON DELETE CASCADE
);

/*
INSERT INTO authors (first_name, last_name, birth_year, death_year)
VALUES
  ('Michael', 'Crichton', 1942, 2008),
  ('Maya', 'Angelou', 1928, 2014),
  ('Anne', 'Rice', 1941, NULL),
  ('Neil', 'Gaiman', 1960, NULL),
  ('Terry', 'Pratchett', 1948, 2015);

INSERT INTO books (title, year, genre)
VALUES
  ('Jurassic Park', 1990, 'Science Fiction');
--  ('
*/
