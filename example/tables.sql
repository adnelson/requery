DROP TABLE IF EXISTS customer CASCADE;
DROP TABLE IF EXISTS address CASCADE;
DROP TABLE IF EXISTS customer_address CASCADE;

CREATE TABLE customer (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  email TEXT NOT NULL,
  phone_number TEXT
);

CREATE TABLE address (
  id SERIAL PRIMARY KEY,
  contents TEXT NOT NULL
);

CREATE TABLE customer_address (
  customer_id INT NOT NULL,
  address_id INT NOT NULL,
  PRIMARY KEY (customer_id, address_id)
);
