module A = Utils.Array;
module L = Utils.List;
module O = Utils.Option;
module ISet = Belt.Set.Int;
module J = Utils.Json;

module type SqlRenderingRules = {
  let _TRUE: string;
  let _FALSE: string;
  // What character to use to wrap a column name on the left/right
  let _NAME_WRAP_LEFT: string;
  let _NAME_WRAP_RIGHT: string;
  // TODO: text escaping function for column names
};

module WithRenderingRules = (S: SqlRenderingRules) => {
  let wrap = s => S._NAME_WRAP_LEFT ++ s ++ S._NAME_WRAP_RIGHT;

  module Table = {
    open Sql.Table;
    let render = toString;
  };

  module Column = {
    open Sql.Column;
    let render = c =>
      switch (toTuple(c)) {
      | (None, c) => wrap(c)
      | (Some(t), c) => wrap(Sql.Table.toString(t)) ++ "." ++ wrap(c)
      };
  };

  module Aliased = {
    open Sql.Aliased;
    let render: ('a => string, t('a)) => string =
      (renderInner, aliased) =>
        switch (toTuple(aliased)) {
        | (x, None) => renderInner(x)
        | (x, Some(alias)) => renderInner(x) ++ " AS " ++ alias
        };
  };

  module Expression = {
    open Sql.Expression;
    // Escape single quotes by replacing them with single quote pairs.
    let escape = Js.String.replaceByRe(Js.Re.fromStringWithFlags("'", ~flags="g"), "''");
    let renderAtom: atom => string =
      fun
      | Null => "NULL"
      | Column(col) => Column.render(col)
      | Int(i) => string_of_int(i)
      | Float(f) => Js.Float.toString(f)
      | String(s) => "'" ++ escape(s) ++ "'"
      | Bool(b) => b ? S._TRUE : S._FALSE;

    // TODO handle precedence here, wrap with parentheses where needed
    let rec render: t => string =
      fun
      | Atom(atom) => renderAtom(atom)
      | Typed(e, t) => render(e) ++ "::" ++ t
      | Eq(ex1, ex2) => render(ex1) ++ " = " ++ render(ex2)
      | Neq(ex1, ex2) => render(ex1) ++ " <> " ++ render(ex2)
      | Lt(ex1, ex2) => render(ex1) ++ " < " ++ render(ex2)
      | Leq(ex1, ex2) => render(ex1) ++ " <= " ++ render(ex2)
      | Gt(ex1, ex2) => render(ex1) ++ " > " ++ render(ex2)
      | Geq(ex1, ex2) => render(ex1) ++ " >= " ++ render(ex2)
      | Like(ex1, ex2) => render(ex1) ++ " LIKE " ++ render(ex2)
      | And(ex1, ex2) => render(ex1) ++ " AND " ++ render(ex2)
      | Or(ex1, ex2) => render(ex1) ++ " OR " ++ render(ex2)
      | IsNull(e) => render(e) ++ " IS NULL"
      | IsNotNull(e) => render(e) ++ " IS NOT NULL"
      | Call(fnName, args) => fnName ++ A.mapJoinCommasParens(args, render)
      | Tuple(exprs) => A.mapJoinCommasParens(exprs, render);
  };

  module Select = {
    open Sql.Select;
    let renderJoinType: joinType => (string, option(string)) =
      fun
      | Inner(on) => ("INNER JOIN", Some(Expression.render(on)))
      | Left(on) => ("LEFT OUTER JOIN", Some(Expression.render(on)))
      | Right(on) => ("RIGHT OUTER JOIN", Some(Expression.render(on)))
      | Full(on) => ("FULL JOIN", Some(Expression.render(on)))
      | Cross => ("CROSS JOIN", None);

    let renderDirection: direction => string =
      fun
      | ASC => "ASC"
      | DESC => "DESC";

    let rec renderTarget: target => string =
      fun
      | Table(tname) => Aliased.render(Sql.Table.toString, tname)
      | SubSelect(q, alias) => "(" ++ render(q) ++ ") AS " ++ alias
      | Join(join, t1, t2) =>
        switch (renderJoinType(join)) {
        | (keyword, None) => renderTarget(t1) ++ " " ++ keyword ++ " " ++ renderTarget(t2)
        | (keyword, Some(on)) =>
          renderTarget(t1) ++ " " ++ keyword ++ " " ++ renderTarget(t2) ++ " ON " ++ on
        }

    and render: t => string =
      ({selections, from, orderBy, groupBy, limit, where}) => {
        let selections = A.mapJoinCommas(selections, Aliased.render(Expression.render));
        let from = O.mapString(from, t => " FROM " ++ renderTarget(t));
        let orderBy =
          A.mapJoinIfNonEmpty(orderBy, ~prefix=" ORDER BY ", ", ", ((c, optDir)) =>
            Column.render(c) ++ O.mapString(optDir, dir => " " ++ renderDirection(dir))
          );
        let groupBy = A.mapJoinIfNonEmpty(groupBy, ~prefix=" GROUP BY ", ", ", Column.render);
        let limit = O.mapString(limit, n => " LIMIT " ++ string_of_int(n));
        let where = O.mapString(where, e => " WHERE " ++ Expression.render(e));
        "SELECT " ++ selections ++ from ++ groupBy ++ where ++ orderBy ++ limit;
      };
  };

  module Insert = {
    open Sql.Insert;
    exception UnequalNumberOfExpressions(list(int));

    let render: t => string =
      ({data, into, returning}) => {
        "INSERT INTO "
        ++ Table.render(into)
        ++ " "
        ++ (
          switch (data) {
          | Values(values) =>
            let cols = A.mapJoinCommasParens(values, v => Column.render(fst(v)));
            let numsOfExprs = ISet.fromArray(A.map(values, ((_, exprs)) => A.length(exprs)));
            switch (ISet.toList(numsOfExprs)) {
            // They must all have the same number of expressions.
            | [count] =>
              // Convert expressions to comma-separated tuples
              let tuples = A.makeBy(count, n => A.map(values, ((_, exprs)) => exprs[n]));
              let valuesStr =
                A.mapJoinCommas(tuples, exprs => A.mapJoinCommasParens(exprs, Expression.render));
              cols ++ " VALUES " ++ valuesStr;
            | counts => raise(UnequalNumberOfExpressions(counts))
            };
          | Select(sel) => Select.render(sel)
          }
        )
        ++ O.mapString(
             returning,
             fun
             | Columns(columns) =>
               " RETURNING " ++ A.mapJoinCommasParens(L.toArray(columns), Column.render),
           );
      };
  };

  let render: Sql.query => string =
    fun
    | Select(s) => Select.render(s)
    | Insert(i) => Insert.render(i);
};
