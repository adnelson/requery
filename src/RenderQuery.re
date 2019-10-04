module A = Utils.Array;
module L = Utils.List;
module O = Utils.Option;
module ISet = Belt.Set.Int;

module Column = {
  open SqlQuery.Column;
  let render = toString;
};

module TableName = {
  open SqlQuery.TableName;
  let render = toString;
};

module Aliased = {
  open SqlQuery.Aliased;
  let render: ('a => string, t('a)) => string =
    (renderInner, aliased) =>
      switch (toTuple(aliased)) {
      | (x, None) => renderInner(x)
      | (x, Some(alias)) => renderInner(x) ++ " AS " ++ alias
      };
};

module Expression = {
  open SqlQuery.Expression;
  // Escape single quotes by replacing them with single quote pairs.
  let escape = Js.String.replaceByRe(Js.Re.fromStringWithFlags("'", ~flags="g"), "''");
  let renderAtom: atom => string =
    fun
    | Column(col) => SqlQuery.Column.toString(col)
    | Int(i) => string_of_int(i)
    | Float(f) => Js.Float.toString(f)
    | String(s) => "'" ++ escape(s) ++ "'"
    | Bool(b) => b ? "TRUE" : "FALSE";

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
    | And(ex1, ex2) => render(ex1) ++ " AND " ++ render(ex2)
    | Or(ex1, ex2) => render(ex1) ++ " OR " ++ render(ex2)
    | Call(fnName, args) =>
      fnName ++ "(" ++ Js.Array.joinWith(", ", A.map(args, render)) ++ ")";
};

module Select = {
  open SqlQuery.Select;
  let renderJoinType: joinType => (string, option(string)) =
    fun
    | Inner(on) => ("INNER JOIN", Some(Expression.render(on)))
    | Left(on) => ("LEFT OUTER JOIN", Some(Expression.render(on)))
    | Right(on) => ("RIGHT OUTER JOIN", Some(Expression.render(on)))
    | Cross => ("CROSS JOIN", None);

  let renderDirection: direction => string =
    fun
    | ASC => "ASC"
    | DESC => "DESC";

  let rec renderTarget: target => string =
    fun
    | TableName(tname) => Aliased.render(SqlQuery.TableName.toString, tname)
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
  open SqlQuery.Insert;
  let render: t => string =
    ({data, into}) => {
      "INSERT INTO "
      ++ TableName.render(into)
      ++ " "
      ++ (
        switch (data) {
        | Values(values) =>
          let cols =
            A.mapJoinCommas(values, ~prefix="(", ~suffix=")", ((c, _)) => Column.render(c));
          let numsOfExprs = ISet.fromArray(A.map(values, ((_, exprs)) => A.length(exprs)));
          switch (ISet.toList(numsOfExprs)) {
          // They must all have the same number of expressions.
          | [count] =>
            // Convert expressions to comma-separated tuples
            let tuples = A.makeBy(count, n => A.map(values, ((_, exprs)) => exprs[n]));
            let values =
              A.mapJoinCommas(tuples, exprs =>
                A.mapJoinCommas(exprs, ~prefix="(", ~suffix=")", Expression.render)
              );
            cols ++ " VALUES " ++ values;
          | counts =>
            let counts = A.mapJoinCommas(L.toArray(counts), string_of_int);
            Utils.throw(
              "Not all expression arrays were the same length. Saw lengths: " ++ counts,
            );
          };
        | Select(sel) => Select.render(sel)
        }
      );
    };
};

let render: SqlQuery.query => string =
  fun
  | Select(s) => Select.render(s)
  | Insert(i) => Insert.render(i);
