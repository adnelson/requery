open Jest;
open Expect;

module String = {
  open Utils.String;
  describe("dedupe", () => {
    let input = [|"a", "b", "b", "a", "c"|];
    test("array", () =>
      expect(dedupeArray(input)) |> toEqual([|"a", "b", "c"|])
    );
    test("list", () =>
      expect(dedupeList(Belt.List.fromArray(input)))
      |> toEqual(["a", "b", "c"])
    );
  });
};
