module A = Requery.Utils.Array;
module O = Requery.Utils.Option;
module S = Requery.Utils.String;
module QB = Requery.QueryBuilder;

module Sql = {
  // Postgres-specific syntax
  include Requery.Sql;

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

    // TODO `DO UPDATE` syntax
    type conflictAction =
      | DoNothing;

    type t = {
      target: option(conflictTarget),
      action: conflictAction,
    };
  };
};

module Render = {
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
        onConstraint->O.mapString(cn =>
          "ON CONSTRAINT " ++ cn->Render.ConstraintName.render
        ),
        index->O.mapString(renderIndex),
        where->O.mapString(e => "WHERE " ++ e->Render.Expression.render),
      |]);
    let renderAction =
      fun
      | DoNothing => "DO NOTHING";
    let render: t => string =
      ({target, action}) =>
        S.joinSpaces([|
          "ON CONFLICT",
          target->O.mapString(renderTarget),
          action->renderAction,
        |]);
  };
};

module QueryBuilder = {
  module QB = Requery.QueryBuilder;
  include QB;
  open Sql.Returning;

  let returning = columns => QB.returning(Columns(columns));
  let returning1 = col => QB.returning(Columns([|col|]));
};

type query = Sql.query(Sql.Returning.t, Sql.OnConflict.t);

// lol so many renders
let render: query => string =
  Render.Render.render(Render.Returning.render, Render.OnConflict.render);
