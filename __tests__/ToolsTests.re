open Tools;
open Jest;
open Expect;
open BooksExample;

module TableToolsTests = {
  describe("table tools", () => {
    let table = Author.createTable(QueryBuilder.Types.int);
    test("name", () =>
      expect(table->TableTools.name)->toEqual(Author.tableName)
    );
    test("columns", () =>
      expect(table->TableTools.columns)->toEqual([|"id", "first", "last"|]->A.map(QB.cname))
    );
    test("primaryKeyColumn", () =>
      expect(table->TableTools.primaryKeyColumn)->toEqual(Some("id"->QB.cname))
    );
  });
};
