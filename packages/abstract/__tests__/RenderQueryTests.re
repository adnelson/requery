open Jest;
open Expect;
module QB = QueryBuilder;
module R = RenderQuery.Default;

module SnapshotTests = {
  describe("createTable", () => {
    test("author", () =>
      expect(
        BooksExample.Author.createTable(QB.Types.int)->R.CreateTable.render,
      )
      ->toMatchSnapshot()
    );
    test("book", () =>
      expect(
        BooksExample.Book.createTable(QB.Types.int)->R.CreateTable.render,
      )
      ->toMatchSnapshot()
    );
  });
  describe("select", () => {
    test("author books", () =>
      expect(BooksExample.authorBooksSelect->R.Select.render)
      ->toMatchSnapshot()
    );
    test("author books CTE", () =>
      expect(BooksExample.getAuthorIdsCTE->R.Select.render)->toMatchSnapshot()
    );
  });
  describe("insert", () => {
    test("authors", () => {
      expect(
        R.Insert.render(
          ~onConflict=string_of_int,
          BooksExample.insertAuthors,
        ),
      )
      ->toMatchSnapshot()
    });
    test("authors with fake on conflict", () => {
      let insert = BooksExample.insertAuthors;
      let withOnConflict = insert |> QB.onConflict(1234);
      let renderOnConflict = n => "ON CONFLICT " ++ string_of_int(n);
      let rendered =
        R.Insert.render(~onConflict=renderOnConflict, withOnConflict);
      expect(rendered)->toMatchSnapshot();
      expect(rendered)->toEqual(stringContaining("ON CONFLICT 1234"));
    });
  });
};
