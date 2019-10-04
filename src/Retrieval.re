type t('args, 'result) = {
  // TODO this could be a promise
  toSelect: 'args => QueryBuilder.select,
  decode: RowDecode.decoder('result),
};

let make = (toSelect, decode) => {toSelect, decode};
let toSelect: (t('a, 'r), 'a) => QueryBuilder.select = ({toSelect}, args) => toSelect(args);
