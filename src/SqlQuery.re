module A = Belt.Array;
module O = Belt.Option;

type tableName = string;

module Column = {
  // e.g. 'foo' or 'mytable.foo'
  type t = {
    table: option(tableName),
    name: string,
  };

  let make = (~t=?, name: string): t => {table: t, name};

  let render: t => string =
    ({table, name}) =>
      switch (table) {
      | Some(t) => t ++ "." ++ name
      | None => name
      };
};

module Aliased = {
  type t('a) = ('a, option(string));
  let bare: 'a => t('a) = x => (x, None);
  let make = (~a=?, x) => (x, a);
  let as_: ('a, string) => t('a) = (x, alias) => (x, Some(alias));
  let render: ('a => string, t('a)) => string =
    renderInner =>
      fun
      | (x, None) => renderInner(x)
      | (x, Some(alias)) => renderInner(x) ++ " AS " ++ alias;
};

module Expression = {
  type t =
    | Column(Column.t)
    | Eq(t, t)
    | Neq(t, t)
    | And(t, t)
    | Or(t, t);

  let col = (~t=?, col) => Column(Column.make(~t?, col));

  let rec render: t => string =
    fun
    | Column(col) => Column.render(col)
    | Eq(ex1, ex2) => render(ex1) ++ " = " ++ render(ex2)
    | Neq(ex1, ex2) => render(ex1) ++ " <> " ++ render(ex2)
    | And(ex1, ex2) => render(ex1) ++ " AND " ++ render(ex2)
    | Or(ex1, ex2) => render(ex1) ++ " OR " ++ render(ex2);
};

module Join = {
  type condition = Expression.t;

  type type_ =
    | Inner(condition)
    | Left(condition)
    | Right(condition)
    | Cross;

  let renderType: type_ => (string, option(string)) =
    fun
    | Inner(on) => ("INNER JOIN", Some(Expression.render(on)))
    | Left(on) => ("LEFT OUTER JOIN", Some(Expression.render(on)))
    | Right(on) => ("RIGHT OUTER JOIN", Some(Expression.render(on)))
    | Cross => ("CROSS JOIN", None);
};

// TODO distinct, other functions, etc
module Selection = {
  type t =
    | All(option(tableName)) // e.g. '*' or 'mytable.*'
    | Expression(Expression.t)
    | Count(t);

  let all: t = All(None);
  let allFrom: tableName => t = name => All(Some(name));
  let expr: Expression.t => t = e => Expression(e);
  //  let col: string => t = colName => Expression(Expression.Column(Column.make(colName)));
  let col = (~t=?, col) => Expression(Expression.Column(Column.make(~t?, col)));

  let rec render: t => string =
    fun
    | All(None) => "*"
    | All(Some(table)) => table ++ ".*"
    | Expression(e) => Expression.render(e)
    // TODO this feels more like an expression, but I'm not sure how that works with the '*'
    | Count(sel) => "COUNT(" ++ render(sel) ++ ")";
};

module Query = {
  // What comes after the FROM of a query.
  type target =
    | TableName(Aliased.t(tableName))
    | SubQuery(t, string)
    | Join(Join.type_, target, target)

  // Renders into a SELECT query.
  and t = {
    selections: array(Aliased.t(Selection.t)),
    from: option(target),
    groupBy: array(Column.t),
    limit: option(int),
  };

  let make = (~groupBy=?, ~limit=?, ~from=?, selections) => {
    selections,
    from,
    limit,
    groupBy: O.getWithDefault(groupBy, [||]),
  };

  let select = (~a=?, sel): (Selection.t, option(string)) => Aliased.make(~a?, sel);
  let table = (~a=?, tableName) => TableName(Aliased.make(tableName, ~a?));
  let innerJoin = (t1, t2, cond) => Join(Join.Inner(cond), t1, t2);
  let leftJoin = (t1, t2, cond) => Join(Join.Left(cond), t1, t2);

  let rec renderTarget: target => string =
    fun
    | TableName(tname) => Aliased.render(s => s, tname)
    | SubQuery(q, alias) => "(" ++ renderQuery(q) ++ ") AS " ++ alias
    | Join(join, t1, t2) =>
      switch (Join.renderType(join)) {
      | (keyword, None) => renderTarget(t1) ++ " " ++ keyword ++ " " ++ renderTarget(t2)
      | (keyword, Some(on)) =>
        renderTarget(t1) ++ " " ++ keyword ++ " " ++ renderTarget(t2) ++ " ON " ++ on
      }

  and renderQuery: t => string =
    ({selections, from, groupBy, limit}) => {
      let groupByString =
        switch (groupBy) {
        | [||] => ""
        | columns => " GROUP BY " ++ Js.Array.joinWith(", ", A.map(columns, Column.render))
        };
      let limitString = O.mapWithDefault(limit, "", n => " LIMIT " ++ string_of_int(n));
      let selectionsString =
        Js.Array.joinWith(", ", A.map(selections, Aliased.render(Selection.render)));
      let fromString = O.mapWithDefault(from, "", t => " FROM " ++ renderTarget(t));
      "SELECT " ++ selectionsString ++ fromString ++ groupByString ++ limitString;
    };
};

let renderQuery = Query.renderQuery;

module C = Column;
module E = Expression;
module S = Selection;
module J = Join;
module Q = Query;

/*
 SELECT q.id AS question_id, ro.label, count FROM question AS q
 INNER JOIN response_option AS ro ON q.option_set_id = ro.option_set_id
 LEFT OUTER JOIN (
   SELECT count(*), question_id, response_option_id
   FROM single_choice_response
   GROUP BY question_id, response_option_id
 ) AS counts ON counts.question_id = q.question_id and
    counts.response_option_id = ro.id
 WHERE q.id = 1;
 */
let countsQuery: Query.t =
  Query.(
    make(
      A.map([|S.Count(S.all), S.col("question_id"), S.col("response_option_id")|], select),
      ~from=table("single_choice_response"),
      ~groupBy=A.map([|"question_id", "response_option_id"|], Column.make),
    )
  );

let outerQuery: Query.t =
  Query.(
    make(
      A.map([|S.col("id", ~t="q"), S.col("label", ~t="ro"), S.col("count")|], select),
      ~from=
        leftJoin(
          innerJoin(
            table("question", ~a="q"),
            table("response_option", ~a="ro"),
            E.(Eq(col(~t="q", "option_set_id"), col(~t="ro", "option_set_id"))),
          ),
          SubQuery(countsQuery, "counts"),
          E.(
            And(
              Eq(col(~t="counts", "question_id"), col(~t="q", "id")),
              Eq(col(~t="counts", "response_option_id"), col(~t="ro", "id")),
            )
          ),
        ),
    )
  );
