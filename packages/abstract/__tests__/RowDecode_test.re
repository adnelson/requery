open Jest;
open Expect;
module A = Utils.Array;
module D = Utils.Dict;
module RD = RowDecode;

// Test decoding dictionaries
module Dict = {
  describe("flat dict", () => {
    let jsonRows: array(Js.Json.t) = [%bs.raw {|require('./test_data/user_rows.json')|}];
    let rows = RD.toRows(jsonRows);
    describe("unordered", () =>
      describe("ID to first name", () => {
        let dict =
          rows
          |> RD.dict(
               ~keyField="id",
               ~keyDecode=j => j |> RD.int |> string_of_int,
               ~valueField="first_name",
               ~valueDecode=RD.string,
             );
        test("Bob", () =>
          expect(D.get(dict, "1")) |> toEqual(Some("Bob"))
        );
        test("Biff", () =>
          expect(D.get(dict, "2")) |> toEqual(Some("Biff"))
        );
      })
    );

    describe("ordered", () =>
      describe("ID to first name", () => {
        let (dict, order) =
          rows
          |> RD.dictWithOrder(
               ~keyField="id",
               ~keyDecode=j => j |> RD.int |> string_of_int,
               ~valueField="first_name",
               ~valueDecode=RD.string,
             );
        test("Bob", () =>
          expect(D.get(dict, "1")) |> toEqual(Some("Bob"))
        );
        test("Biff", () =>
          expect(D.get(dict, "2")) |> toEqual(Some("Biff"))
        );
        test("ordering", () =>
          expect(order) |> toEqual(A.map([|1, 2, 3, 4, 5|], string_of_int))
        );
      })
    );
  });
};
