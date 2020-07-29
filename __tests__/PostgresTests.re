open Jest;
open Expect;
module QB = Requery.QueryBuilder;
module RE = Requery.RowEncode;

open PostgresCustomSyntax;

let onConflictConstraint =
  Sql.OnConflict.{
    target:
      Some({
        index: None,
        onConstraint: Some("scary_constraint"->QB.constraintName),
        where: None,
      }),
    action: DoNothing,
  };

module CustomSyntaxTests = {
  test("rendering an on conflict  clause", () =>
    expect(onConflictConstraint->Render.OnConflict.render)->toMatchSnapshot()
  );
  describe("rendering an insert", () => {
    test("no on conflict", () =>
      expect(Sql.Insert(BooksExample.insertAuthors) |> render)->toMatchSnapshot()
    );
    describe("with on conflict", () =>
      test("on a constraint", () => {
        let insert =
          Sql.Insert(BooksExample.insertAuthors |> QB.onConflict(onConflictConstraint));
        let rendered = insert |> render;
        expect(rendered)->toMatchSnapshot();
        expect(rendered)->toEqual(stringContaining("ON CONFLICT ON CONSTRAINT"));
        expect(rendered)->toEqual(stringContaining("scary_constraint"));
      })
    );
  });
};
