// Postgres-specific syntax
module A = Utils.Array;
module O = Utils.Option;
module S = Utils.String;
module QB = QueryBuilder;

open Sql;

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

/* module Render = { */
/*   open RenderQuery; */
/*   module Rules = Requery.RenderQuery.DefaultRules; */
/*   module Render = Requery.RenderQuery.WithRenderingRules(Rules); */
/*   include Render; */
/*   module Returning = { */
/*     let render: Sql.Returning.t => string = */
/*       fun */
/*       | Columns(columns) => */
/*         " RETURNING " ++ A.mapJoinCommasParens(columns, Render.Column.render); */
/*   }; */

/*   module OnConflict = { */
/*     open Sql.OnConflict; */
/*     let renderIndex = */
/*       fun */
/*       | IndexColumn(cn) => cn->Render.ColumnName.render */
/*       | IndexExpression(e) => "(" ++ e->Render.Expression.render ++ ")"; */
/*     let renderTarget = ({index, where, onConstraint}) => */
/*       S.joinSpaces([| */
/*         onConstraint->O.mapString(cn => "ON CONSTRAINT " ++ cn->Render.ConstraintName.render), */
/*         index->O.mapString(renderIndex), */
/*         where->O.mapString(e => "WHERE " ++ e->Render.Expression.render), */
/*       |]); */
/*     let renderAction = */
/*       fun */
/*       | DoNothing => "DO NOTHING"; */
/*     let render: t => string = */
/*       ({target, action}) => */
/*         S.joinSpaces([|"ON CONFLICT", target->O.mapString(renderTarget), action->renderAction|]); */
/*   }; */

/*   module CreateType = { */
/*     open Sql.CreateType; */
/*     module EnumValue = { */
/*       include EnumValue; */
/*       // This should be safe because the enum value regex prevents quotes, */
/*       // but if that changes this will need to be revisited. */
/*       let render = ev => "'" ++ ev->EnumValue.toString ++ "'"; */
/*     }; */

/*     let renderVariant = */
/*       fun */
/*       | Enum(values) => "AS ENUM " ++ values->map(EnumValue.render)->commas->parens; */

/*     let render = ({name, variant}) => */
/*       [|"CREATE TYPE", name->TypeName.render, variant->renderVariant|]->spaces; */
/*   }; */
/* }; */

// Type aliases for postgres-specific queries
type pgEnumValue = CreateType.EnumValue.t;
type pgCreateType = CreateType.t;
type pgReturning = Returning.t;
type pgOnConflict = OnConflict.t;
type pgCreateCustom = CreateType.t;
type pgInsert = Sql.Insert.t(pgReturning, pgOnConflict);
type pgQueryOf('t) = Sql.query(pgReturning, pgOnConflict, pgCreateCustom, 't);
type pgQuery = Sql.query(pgReturning, pgOnConflict, pgCreateCustom, Sql.TableName.t);

/* module QueryBuilder = { */
/*   module QB = Requery.QueryBuilder; */
/*   include QB; */
/*   open Sql; */
/*   type enumValue = CreateType.EnumValue.t; */
/*   type createType = CreateType.t; */
/* type returning = Sql.Returning.t; */
/* type onConflict = Sql.OnConflict.t; */
/* type createCustom = Sql.CreateType.t; */

/* // Insert made specific to postgres */
/* type pgInsert = Sql.Insert.t(returning, onConflict); */

/* type query('t) = Sql.query(returning, onConflict, createCustom, 't); */

/*   let returning: (array(column), insert(Returning.t, 'a)) => insert(Returning.t, 'a) = */
/*     columns => QB.returning(Returning.Columns(columns)); */
/*   let returning1: (column, insert(Returning.t, 'a)) => insert(Returning.t, 'a) = */
/*     col => QB.returning(Returning.Columns([|col|])); */

/*   let enumValue: string => enumValue = Sql.CreateType.EnumValue.fromString; */
/*   let enumValues: list(string) => list(enumValue) = vs => vs->Belt.List.map(enumValue); */
/*   let enumValuesArray: array(string) => array(enumValue) = vs => vs->Belt.Array.map(enumValue); */
/*   let createEnumType: (typeName, list(enumValue)) => createType = */
/*     (name, values) => CreateType.makeEnum(name, values->Belt.List.toArray); */

/*   // Attaches `ON CONFLICT DO NOTHING` to an insert */
/*   let onConflictNothing: 'r. insert('r, OnConflict.t) => insert('r, OnConflict.t) = */
/*     ins => ins |> onConflict(OnConflict.make(DoNothing)); */
/* }; */
