open Jest;
open Expect;
module A = Utils.Array;
module O = Utils.Option;
module D = Utils.Dict;
module RD = RowDecode;

// Test decoding dictionaries
module Dict = {
  let jsonRows: array(Js.Json.t) = [%bs.raw {|require('./test_data/user_rows.json')|}];
  let rows = RD.toRows(jsonRows);

  describe("dictOf", () => {
    describe("one-dimensional dictionary", () => {
      let byFirstName: D.t(array(string)) =
        rows |> RD.(dictOf(~keyField="first_name", decodeEach(field("last_name", string))));
      test("Bob", () =>
        expect(D.get(byFirstName, "Bob")) |> toEqual(Some([|"Blooperman"|]))
      );
    });

    describe("nested dictionary", () => {
      let firstLastId: D.t(D.t(int)) =
        rows
        |> RD.(
             dictOf(
               ~keyField="last_name",
               dictOf(~keyField="first_name", decodeOne(field("id", int))),
             )
           );
      test("Blooperman", () =>
        expect(D.get(firstLastId, "Blooperman"))
        |> toEqual(Some(D.fromArray([|("Bob", 1), ("Billy", 5)|])))
      );
    });
  });

  describe("dictOfWithOrder", () =>
    describe("one-dimensional dictionary", () => {
      let (byFirstName: D.t(array(string)), firstNames) =
        rows
        |> RD.(dictOfWithOrder(~keyField="first_name", decodeEach(field("last_name", string))));
      test("Bob", () =>
        expect(D.get(byFirstName, "Bob")) |> toEqual(Some([|"Blooperman"|]))
      );
      test("names", () =>
        expect(firstNames) |> toEqual([|"Bob", "Biff", "Barnabus", "Bertrand", "Billy"|])
      );
    })
  );

  describe("flat dict", () => {
    describe("unordered", () => {
      describe("ID => first name", () => {
        let dict =
          rows
          |> RD.dict(
               ~keyField="id",
               ~keyDecode=j => j |> RD.int |> string_of_int,
               ~valueField="first_name",
               ~valueDecode=RD.string,
             );
        test("ID 1 is Bob", () =>
          expect(D.get(dict, "1")) |> toEqual(Some("Bob"))
        );
        test("ID 2 is Biff", () =>
          expect(D.get(dict, "2")) |> toEqual(Some("Biff"))
        );
      });

      describe("last name => first name => ID", () => {
        let dict =
          rows
          |> RD.dict2d(
               ~outerKeyField="last_name",
               ~innerKeyField="first_name",
               ~valueField="id",
               ~valueDecode=RD.int,
             );
        test("Bob's ID", () =>
          expect(D.get(D.getExn(dict, "Blooperman"), "Bob")) |> toEqual(Some(1))
        );
        test("Biff's ID", () =>
          expect(D.get(D.getExn(dict, "Bofferton"), "Biff")) |> toEqual(Some(2))
        );
        describe("the Boffertons", () => {
          let boffertons = D.getExn(dict, "Bofferton");
          A.forEach([|"Biff", "Barnabus", "Bertrand"|], name =>
            test(name, () =>
              expect(O.isSome(D.get(boffertons, name))) |> toBe(true)
            )
          );
        });
      });
    });

    describe("ordered", () =>
      describe("ID => first name", () => {
        let (dict, ordering) =
          rows
          |> RD.dictWithOrder(
               ~keyField="id",
               ~keyDecode=j => j |> RD.int |> string_of_int,
               ~valueField="first_name",
               ~valueDecode=RD.string,
             );
        test("ID 1 is Bob", () =>
          expect(D.get(dict, "1")) |> toEqual(Some("Bob"))
        );
        test("ID 2 is Biff", () =>
          expect(D.get(dict, "2")) |> toEqual(Some("Biff"))
        );
        test("ordering contains all IDs in order", () =>
          expect(ordering) |> toEqual(A.map([|1, 2, 3, 4, 5|], string_of_int))
        );
      })
    );
  });
};
