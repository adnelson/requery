open SqlQuery;
module E = Expression;
module L = Belt.List;

type target = SqlQuery.Select.target;
type expr = SqlQuery.Expression.t;
type aliasedExpr = SqlQuery.Aliased.t(expr);
type select = SqlQuery.Select.t;

let typed = (e, t) => E.Typed(e, t);

let int = i => E.Atom(E.Int(i));
let bool = b => E.Atom(E.Bool(b));
let float = f => E.Atom(E.Float(f));
let string = s => E.Atom(E.String(s));
let bigint = i => typed(int(i), "BigInt");

let col = c => E.Atom(E.Column(Column.fromString(c)));
let all = col("*");
let allFrom = t => col(t ++ ".*");

let eq = (e1, e2) => E.Eq(e1, e2);
let neq = (e1, e2) => E.Neq(e1, e2);
let and_ = (e1, e2) => E.And(e1, e2);
let or_ = (e1, e2) => E.Or(e1, e2);
let lt = (e1, e2) => E.Lt(e1, e2);
let leq = (e1, e2) => E.Leq(e1, e2);
let gt = (e1, e2) => E.Gt(e1, e2);
let geq = (e1, e2) => E.Geq(e1, e2);

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
let sub = (select, alias) => Select.SubSelect(select, alias);

let select = (~from=?, ~groupBy=[], ~orderBy=[], ~limit=?, ~where=?, selections) =>
  Select.{
    selections: L.toArray(selections),
    from,
    limit,
    groupBy: L.toArray(L.map(groupBy, Column.fromString)),
    orderBy: L.toArray(L.map(orderBy, Column.fromString)),
    where,
  };
