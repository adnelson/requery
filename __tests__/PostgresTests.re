open Jest;
open Expect;
module RE = Requery.RowEncode;

module QB = QueryBuilder;
module PQB = PostgresQueryBuilder;

module CustomSyntaxTests = {
  open PostgresSyntax;

  let scaryConstraint =
    PQB.(
      pgMakeOnConflict(~target=pgOnConstraint("scary_constraint"->QB.constraintName), DoNothing)
    );

  test("rendering an on conflict  clause", () =>
    expect(scaryConstraint->PostgresRender.OnConflict.render)->toMatchSnapshot()
  );
  describe("rendering an insert", () => {
    test("no on conflict", () =>
      expect(Sql.Insert(BooksExample.insertAuthors) |> PostgresRender.pgRender)
      ->toMatchSnapshot()
    );
    describe("with on conflict", () => {
      test("on a constraint", () => {
        let insert = Sql.Insert(BooksExample.insertAuthors |> QB.onConflict(scaryConstraint));
        let rendered = insert |> PostgresRender.pgRender;
        expect(rendered)->toMatchSnapshot();
        expect(rendered)->toEqual(stringContaining("ON CONFLICT ON CONSTRAINT"));
        expect(rendered)->toEqual(stringContaining("scary_constraint"));
      });
      test("do nothing", () => {
        let insert = Sql.Insert(BooksExample.insertAuthors |> PQB.pgOnConflictNothing);
        let rendered = insert |> PostgresRender.pgRender;
        expect(rendered)->toMatchSnapshot();
        expect(rendered)->toEqual(stringContaining("ON CONFLICT"));
        expect(rendered)->toEqual(stringContaining("DO NOTHING"));
      });
    });
  });

  describe("create type", () => {
    open QueryBuilder;
    open PostgresQueryBuilder;
    test("enum type", () => {
      let ct = pgCreateEnumType(typeName("color"), ["red", "green", "blue"]->pgEnumValues);
      let rendered = CreateCustom(ct)->PostgresRender.pgRender;
      expect(rendered)->toMatchSnapshot();
      expect(rendered)->toEqual(stringContaining("AS ENUM"));
    });
    test("enum type with invalid characters in value", () => {
      expect(() =>
        pgCreateEnumType(typeName("color"), ["it's bad"->pgEnumValue])
      )
      ->toThrowSomething
    });

    test("enum type with no values defined", () => {
      expect(() =>
        pgCreateEnumType(typeName("color"), [])
      )->toThrowSomething
    });
  });
};
