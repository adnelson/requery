let setup = client =>
  client
  |> connect
  |> then_(_ =>
       query(
         "CREATE TABLE IF NOT EXISTS fruits (id serial PRIMARY KEY NOT NULL, name varchar(20) UNIQUE NOT NULL)",
         client,
       )
     )
  |> then_(_ =>
       query(
         "CREATE TABLE IF NOT EXISTS vendors (vendor_name varchar(20) NOT NULL, fruits_id integer NOT NULL, "
         ++ "PRIMARY KEY (vendor_name, fruits_id), FOREIGN KEY (fruits_id) REFERENCES fruits (id))",
         client,
       )
     )
  |> then_(_ =>
       query("INSERT INTO fruits VALUES (default, 'avocado')", client)
     )
  |> then_(_ =>
       query("INSERT INTO fruits VALUES (default, 'banana')", client)
     )
  |> then_(_ =>
       query("INSERT INTO fruits VALUES (default, 'blueberry')", client)
     )
  |> then_(_ =>
       query("INSERT INTO fruits VALUES (default, 'raspberry')", client)
     )
  |> then_(_ => query("INSERT INTO vendors VALUES ('bill',  1)", client))
  |> then_(_ => query("INSERT INTO vendors VALUES ('frank', 2)", client))
  |> then_(_ => query("INSERT INTO vendors VALUES ('frank', 3)", client))
  |> then_(_ => query("INSERT INTO vendors VALUES ('jane',  4)", client));

let teardown = client =>
  client
  |> query("DROP TABLE IF EXISTS vendors")
  |> then_(_ => query("DROP TABLE IF EXISTS fruits", client))
  |> then_(_ => end_(client));

let test_query = client =>
  client
  |> query("SELECT * FROM fruits WHERE id > $1", ~values=[|1|])
  |> then_((result: Result.t(fruit)) => {
       expect(result##rows)
       |> to_deep_equal([|
            {"id": 2, "name": "banana"},
            {"id": 3, "name": "blueberry"},
            {"id": 4, "name": "raspberry"},
          |]);
       resolve(client);
     });

let test_query_object = client =>
  client
  |> query'(
       Query.make(
         ~text=
           "SELECT vendor_name AS vendor, name AS fruit FROM vendors JOIN fruits ON id = fruits_id WHERE vendor_name = $1",
         ~values=[|"frank"|],
         (),
       ),
     )
  |> then_((result: Result.t(vendor)) => {
       expect(result##rows)
       |> to_deep_equal([|
            {"fruit": "banana", "vendor": "frank"},
            {"fruit": "blueberry", "vendor": "frank"},
          |]);
       resolve(client);
     });
