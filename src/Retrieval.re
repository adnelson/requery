// Retrieving objects, via select
module QB = QueryBuilder;

type t('args, 'result) = {
  toSelect: QB.toSelect('args),
  decode: RowDecode.decodeRows('result),
};

let make = (toSelect, decode) => {toSelect, decode};
let toSelect: t('a, 'r) => QB.toSelect('a) = ({toSelect}, args) => toSelect(args);
