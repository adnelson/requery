// Retrieving objects, via select
module QB = QueryBuilder;

type t('args, 'result) = {
  toSelect: QB.toSelect('args),
  decode: RowDecode.decodeRows('result),
};

let make = (toSelect, decode) => {toSelect, decode};
let toSelect: t('a, 'r) => QB.toSelect('a) = ({toSelect}, args) => toSelect(args);

// Alter the result of a retrieval with a function.
let transformOutput: (t('a, 'b), 'b => 'c) => t('a, 'c) =
  (r, f) => {...r, decode: rows => f(r.decode(rows))};
