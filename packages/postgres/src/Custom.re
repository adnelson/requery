module A = RequeryAbstract.Utils.Array;

module Sql = {
  // Postgres-specific syntax
  include RequeryAbstract.Sql;

  // Expresses the `RETURNING` clause for inserts
  module Returning = {
    type t =
      | Columns(array(Column.t));
  };
};

module Render = {
  module Rules = RequeryAbstract.RenderQuery.DefaultRules;
  module Render = RequeryAbstract.RenderQuery.WithRenderingRules(Rules);
  include Render;
  module Returning = {
    let render: Sql.Returning.t => string =
      fun
      | Columns(columns) =>
        " RETURNING " ++ A.mapJoinCommasParens(columns, Render.Column.render);
  };
};

module QueryBuilder = {
  module QB = RequeryAbstract.QueryBuilder;
  include QB;
  open Sql.Returning;

  let returning = columns => QB.returning(Columns(columns));
  let returning1 = col => QB.returning(Columns([|col|]));
};

type query = Sql.query(Sql.Returning.t);

// lol so many renders
let render: query => string = Render.Render.render(Render.Returning.render);
