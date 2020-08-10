open Tools;
open Jest;
open Expect;
open BooksExample;
open Sql;
open Utils.Abbreviations;
module QB = QueryBuilder;

module TableToolsTests = {
  describe("table tools", () => {
    let authors = Author.createTable(QueryBuilder.Types.int);
    let books = Book.createTable(QueryBuilder.Types.int);
    test("name", () =>
      expect(authors->TableTools.name)->toEqual(Author.tableName)
    );
    test("columns", () =>
      expect(authors->TableTools.columnNames)
      ->toEqual([|"id", "first", "last"|]->A.map(QB.cname))
    );
    test("columnDefMap", () => {
      let defMap = authors->TableTools.columnDefMap;
      expect(defMap->M.get("first"->QB.cname)->O.map(CreateTable.defType))
      ->toBe(Some(QB.Types.text));
      expect(defMap->M.has("blabla"->QB.cname))->toBe(false);
    });
    test("getCol", () => {
      expect(authors->TableTools.getCol("id"->QB.cname))->toEqual("id"->QB.cname);
      expect(() =>
        authors->TableTools.getCol("not a real column"->QB.cname)
      )->toThrowSomething;
    });
    test("primaryKeyColumn", () =>
      expect(authors->TableTools.primaryKeyColumnDef->O.map(({name}) => name))
      ->toEqual(Some("id"->QB.cname))
    );
    test("foreignKeyColumns", () => {
      let fkColumns = books->TableTools.foreignKeyColumns;
      expect(fkColumns->M.keysArray)->toEqual([|"author id"->QB.cname|]);
      expect(fkColumns->M.get("author id"->QB.cname))
      ->toEqual(Some(("author"->QB.tname, "id"->QB.cname)));
    });
  });
};
