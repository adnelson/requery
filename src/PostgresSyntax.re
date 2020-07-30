module A = Utils.Array;
module O = Utils.Option;
module S = Utils.String;
module QB = QueryBuilder;

module Sql = {
  // Postgres-specific syntax
  include Sql;

  // Expresses the `RETURNING` clause for inserts
  module Returning = {
    type t =
      | Columns(array(Column.t));
  };

  // What do do if there's a conflict while inserting rows
  // Subset of the syntax described here:
  // https://www.postgresql.org/docs/12/sql-insert.html
  module OnConflict = {
    type conflictIndex =
      | IndexColumn(QB.columnName)
      | IndexExpression(QB.expr);

    type conflictTarget = {
      index: option(conflictIndex),
      where: option(QB.expr),
      onConstraint: option(QB.constraintName),
    };

    let makeTarget = (~index=?, ~where=?, ~onConstraint=?, ()) => {index, where, onConstraint};

    // TODO `DO UPDATE` syntax
    type conflictAction =
      | DoNothing;

    type t = {
      target: option(conflictTarget),
      action: conflictAction,
    };

    let make = (~target=?, action) => {target, action};
  };

  module CreateType = {
    module EnumValue =
      Opaque.String.Make(
        (
          Opaque.String.Validation.MatchRegex({
            let regex = [%re {|/^\w+$/|}];
          })
        ),
        {},
      );

    // Raised in the query builder function. Might be worth
    // moving this logic here
    exception NoValuesInEnum(TypeName.t);

    type typeVariant =
      | Enum(array(EnumValue.t));

    type t = {
      name: TypeName.t,
      variant: typeVariant,
    };

    let makeEnum = (name, values) => {
      switch (values) {
      | [||] => raise(NoValuesInEnum(name))
      | _ => {name, variant: Enum(values)}
      };
    };
  };
};

module Render = {
  open RenderQuery;
  module Rules = Requery.RenderQuery.DefaultRules;
  module Render = Requery.RenderQuery.WithRenderingRules(Rules);
  include Render;
  module Returning = {
    let render: Sql.Returning.t => string =
      fun
      | Columns(columns) =>
        " RETURNING " ++ A.mapJoinCommasParens(columns, Render.Column.render);
  };

  module OnConflict = {
    open Sql.OnConflict;
    let renderIndex =
      fun
      | IndexColumn(cn) => cn->Render.ColumnName.render
      | IndexExpression(e) => "(" ++ e->Render.Expression.render ++ ")";
    let renderTarget = ({index, where, onConstraint}) =>
      S.joinSpaces([|
        onConstraint->O.mapString(cn => "ON CONSTRAINT " ++ cn->Render.ConstraintName.render),
        index->O.mapString(renderIndex),
        where->O.mapString(e => "WHERE " ++ e->Render.Expression.render),
      |]);
    let renderAction =
      fun
      | DoNothing => "DO NOTHING";
    let render: t => string =
      ({target, action}) =>
        S.joinSpaces([|"ON CONFLICT", target->O.mapString(renderTarget), action->renderAction|]);
  };

  module CreateType = {
    open Sql.CreateType;
    module EnumValue = {
      include EnumValue;
      // This should be safe because the enum value regex prevents quotes,
      // but if that changes this will need to be revisited.
      let render = ev => "'" ++ ev->EnumValue.toString ++ "'";
    };

    let renderVariant =
      fun
      | Enum(values) => "AS ENUM " ++ values->map(EnumValue.render)->commas->parens;

    let render = ({name, variant}) =>
      [|"CREATE TYPE", name->TypeName.render, variant->renderVariant|]->spaces;
  };
};

module QueryBuilder = {
  module QB = Requery.QueryBuilder;
  include QB;
  open Sql.Returning;

  let returning = columns => QB.returning(Columns(columns));
  let returning1 = col => QB.returning(Columns([|col|]));
  let enumValue = Sql.CreateType.EnumValue.fromString;
  let enumValues = vs => vs->Belt.Array.map(enumValue);
  let createEnumType = Sql.CreateType.makeEnum;
};

type returning = Sql.Returning.t;
type onConflict = Sql.OnConflict.t;
type createCustom = Sql.CreateType.t;
type insert = Sql.Insert.t(returning, onConflict);
type query('t) = Sql.query(returning, onConflict, createCustom, 't);

// lol so many renders
let render: query(QB.tableName) => string =
  Render.Render.render(
    ~returning=Render.Returning.render,
    ~onConflict=Render.OnConflict.render,
    ~createCustom=Render.CreateType.render,
    ~tableRef=Render.TableName.render,
  );

// Render using a custom table reference type.
let renderWith: (~tableRef: 'tr => string, query('tableRef)) => string =
  (~tableRef) =>
    Render.Render.render(
      ~returning=Render.Returning.render,
      ~onConflict=Render.OnConflict.render,
      ~createCustom=Render.CreateType.render,
      ~tableRef,
    );
