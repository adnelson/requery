open Jest;
open Expect;
module QB = Requery.QueryBuilder;
module RE = Requery.RowEncode;

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

let insertAuthors =
  QB.(
    [("Stephen", "King"), ("Jane", "Austen")]
    |> insertMany(RE.columns2("first", string, "last", string))
    |> into(tname("author"))
  );
//    |> onConflict(onConflictConstraint)

module CustomSyntaxTests = {
  test("on conflict rendering", () =>
    expect(onConflictConstraint->Custom.Render.OnConflict.render)
    ->toMatchSnapshot()
  );
  describe("custom syntax", () => {
    test("no on conflict", () =>
      expect(insertAuthors |> Postgres.Render.insert)->toMatchSnapshot()
    );
    describe("with on conflict", () =>
      test("on a constraint", () => {
        let insert =
          Requery.Sql.Insert(
            insertAuthors |> QB.onConflict(onConflictConstraint),
          );
        let rendered = insert |> Custom.render;
        expect(rendered)->toMatchSnapshot();
        expect(rendered)
        ->toEqual(stringContaining("ON CONFLICT ON CONSTRAINT"));
        expect(rendered)->toEqual(stringContaining("scary_constraint"));
      })
    );
  });
};
