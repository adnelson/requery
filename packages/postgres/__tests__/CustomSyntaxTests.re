open Jest;
open Expect;
module QB = Requery.QueryBuilder;
module RE = Requery.RowEncode;
module Example = Requery.BooksExample;

let onConflictConstraint =
  Custom.Sql.OnConflict.{
    target:
      Some({
        index: None,
        onConstraint: Some("scary_constraint"->QB.constraintName),
        where: None,
      }),
    action: DoNothing,
  };

module CustomSyntaxTests = {
  test("rendering an on conflict clause", () =>
    expect(onConflictConstraint->Custom.Render.OnConflict.render)->toMatchSnapshot()
  );
  describe("rendering an insert", () => {
    test("no on conflict", () =>
      expect(Requery.Sql.Insert(Example.insertAuthors) |> Custom.render)->toMatchSnapshot()
    );
    describe("with on conflict", () =>
      test("on a constraint", () => {
        let insert =
          Requery.Sql.Insert(Example.insertAuthors |> QB.onConflict(onConflictConstraint));
        let rendered = insert |> Custom.render;
        expect(rendered)->toMatchSnapshot();
        expect(rendered)->toEqual(stringContaining("ON CONFLICT ON CONSTRAINT"));
        expect(rendered)->toEqual(stringContaining("scary_constraint"));
      })
    );
  });
};
