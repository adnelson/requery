open SqlQuery;
module E = Expression;
module L = Utils.List;

type column = SqlQuery.Column.t;
type tableName = SqlQuery.TableName.t;
type target = SqlQuery.Select.target;
type expr = SqlQuery.Expression.t;
type aliasedExpr = SqlQuery.Aliased.t(expr);
type direction = SqlQuery.Select.direction;
type select = SqlQuery.Select.t;
type insert = SqlQuery.Insert.t;

let typed = (e, t) => E.Typed(e, t);

let int = i => E.Atom(E.Int(i));
let bool = b => E.Atom(E.Bool(b));
let float = f => E.Atom(E.Float(f));
let string = s => E.Atom(E.String(s));
let bigint = i => typed(int(i), "BigInt");

let col_ = c => E.Atom(E.Column(c));
let col = c => E.Atom(E.Column(Column.fromString(c)));
let all = col("*");
let allFrom = t => col(t ++ ".*");

let eq = (e1, e2) => E.Eq(e1, e2);
let (==) = eq;
let neq = (e1, e2) => E.Neq(e1, e2);
let (!=) = neq;
let lt = (e1, e2) => E.Lt(e1, e2);
let (<) = lt;
let leq = (e1, e2) => E.Leq(e1, e2);
let (<=) = leq;
let gt = (e1, e2) => E.Gt(e1, e2);
let (>) = gt;
let geq = (e1, e2) => E.Geq(e1, e2);
let (>=) = geq;
let and_ = (e1, e2) => E.And(e1, e2);
let (&&) = and_;
let or_ = (e1, e2) => E.Or(e1, e2);
let (||) = or_;
let ands =
  fun
  | [] => bool(true)
  | [expr, ...exprs] => L.reduce(exprs, expr, and_);
let ors =
  fun
  | [] => bool(false)
  | [expr, ...exprs] => L.reduce(exprs, expr, or_);

let count = e => E.Call("COUNT", [|e|]);
let distinct = e => E.Call("DISTINCT", [|e|]);
let max = e => E.Call("MAX", [|e|]);
let min = e => E.Call("MIN", [|e|]);
let avg = e => E.Call("AVG", [|e|]);
let sum = e => E.Call("SUM", [|e|]);
let coalesce = (e1, e2) => E.Call("COALESCE", [|e1, e2|]);
let call = (name, args) => E.Call(name, L.toArray(args));

let e = (~a=?, expr): aliasedExpr => Aliased.make(expr, ~a?);

let table = (~a=?, name) => Select.TableName(Aliased.make(name, ~a?));
let innerJoin = (t1, on, t2) => Select.(Join(Inner(on), t2, t1));
let leftJoin = (t1, on, t2) => Select.(Join(Left(on), t2, t1));
let rightJoin = (t1, on, t2) => Select.(Join(Right(on), t2, t1));
let crossJoin = (t1, t2) => Select.(Join(Cross, t2, t1));
// TODO this can inspect the type of the select to collapse unnecessary aliases
let sub = (alias, select) => Select.SubSelect(select, alias);

let column = SqlQuery.Column.fromString;
let columns = SqlQuery.Column.fromStringList;
let (asc, desc) = Select.(ASC, DESC);

let select = (~from=?, ~groupBy=[], ~orderBy=[], ~limit=?, ~where=?, selections) =>
  Select.{
    selections: L.toArray(selections),
    from,
    limit,
    groupBy: L.toArray(groupBy),
    orderBy: L.toArray(orderBy),
    where,
  };

let as_ = alias =>
  Select.(
    fun
    | TableName(tname) => TableName(Aliased.as_(tname, alias))
    | SubSelect(q, _) => SubSelect(q, alias)
    | target => SubSelect(select([e(all)], ~from=target), alias)
  );

let selecting = (sels, s) => Select.{...s, selections: L.toArray(sels)};
let from = (t, s) => Select.{...s, from: Some(t)};
let limit = (n, s) => Select.{...s, limit: Some(n)};
let where = (cond, s) => Select.{...s, where: Some(cond)};
let orderBy = (cols, s) =>
  Select.{...s, orderBy: L.amap(cols, ((c, dir)) => (c, Some(dir)))};
let orderBy_ = (cols, s) => Select.{...s, orderBy: L.amap(cols, c => (c, None))};
let orderBy1 = (col, dir, s) => Select.{...s, orderBy: [|(col, Some(dir))|]};
let orderBy1_ = (col, s) => Select.{...s, orderBy: [|(col, None)|]};
let orderBy2 = (col1, dir1, col2, dir2, s) =>
  Select.{...s, orderBy: [|(col1, Some(dir1)), (col2, Some(dir2))|]};
let orderBy2_ = (col1, col2, s) => Select.{...s, orderBy: [|(col1, None), (col2, None)|]};
let groupBy = (cols, s) => Select.{...s, groupBy: L.toArray(cols)};

let insertValues = (values, into) =>
  Insert.{
    into,
    data: Values(L.toArray(L.map(values, ((c, exprs)) => (c, L.toArray(exprs))))),
  };

let insertSelect = (select, into) => {Insert.into, data: Select(select)};
