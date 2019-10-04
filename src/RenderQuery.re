module A = Utils.Array;
module O = Utils.Option;

module Column = {
  open SqlQuery.Column;
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
  let renderAtom: atom => string =
    fun
    | Column(col) => SqlQuery.Column.toString(col)
    | Int(i) => string_of_int(i)
    | Float(f) => Js.Float.toString(f)
    | String(s) => "'" ++ s ++ "'" // TODO escape quotes
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

let render: SqlQuery.query => string =
  fun
  | Select(s) => Select.render(s)
  | Insert(_) => Utils.throw("Can't render inserts yet");
