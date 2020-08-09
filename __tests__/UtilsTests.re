open Jest;
open Expect;

module String = {
  open StringUtils;
  describe("dedupe", () => {
    let input = [|"a", "b", "b", "a", "c"|];
    test("array", () =>
      expect(dedupeArray(input))->toEqual([|"a", "b", "c"|])
    );
    test("list", () =>
      expect(dedupeList(Belt.List.fromArray(input)))->toEqual(["a", "b", "c"])
    );
  });
};

module Map = {
  open MapUtils;
  describe("maps", () => {
    test("string", () => {
      let map = fromArray([|("foo", "bar")|]);
      expect(map->get("foo"))->toEqual(Some("bar"));
    });
    test("int", () => {
      let map = fromArray([|(123, "bar")|]);
      expect(map->get(123))->toEqual(Some("bar"));
    });
    test("string-like", () => {
      module MyString =
        Opaque.String.Make(
          Opaque.String.Validation.NoValidation,
          {},
        );
      let map = fromArray([|(MyString.fromString("foo"), "bar")|]);
      expect(map->get(MyString.fromString("foo")))->toEqual(Some("bar"));
    });

    test("group by", () => {
      let strings = [|"apple", "cat", "bag", "hello", "world"|];
      let grouped: t(int, array(string)) = strings->groupBy(Js.String.length);
      expect(grouped->get(5))->toEqual(Some([|"apple", "hello", "world"|]));
      expect(grouped->get(3))->toEqual(Some([|"cat", "bag"|]));
      expect(grouped->get(8))->toEqual(None);
    });
  });
};
