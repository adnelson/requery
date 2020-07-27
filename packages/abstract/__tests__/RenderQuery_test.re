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
    test("authors", () =>
      expect(BooksExample.insertAuthors->R.Insert.render)->toMatchSnapshot()
    )
  });
};
