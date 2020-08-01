// Tools to combine an abstract type which can be rendered into a
// query with a decoder of the result. This can be thought of as an alternate
// API to running the individual query functions in `Client`: instead construct
// query/decoder pairs and use the functions in this module to operate on them.
module P = PromiseUtils;
module M = MapUtils;

module SelectDecode = {
  type t('a, 'result) = {
    toSelect: QueryBuilder.toSelect('a),
    fromRows: RowDecode.fromRows('result),
  };

  let make = (~toSelect, ~fromRows) => {toSelect, fromRows};
  let toSelect = ({toSelect}) => toSelect;
  let fromRows = ({fromRows}) => fromRows;

  // Run a select object on a given client, returning
  let run: (Client.t('h, 'r, 'q), t('a, 'r), 'a) => P.t(Client.QueryResult.t('r)) =
    (cli, {toSelect, fromRows}, sel) => cli->Client.select(fromRows, sel->toSelect);
};

// TODO Adds caching capabilities to a select.
/* module SelectDecodeCache = { */
/*   type t('a, 'result) = { */
/*     // Mutable map */
/*     cache: M.t(string, P.t('result)), */
/*     toCacheKey: 'a => string, */
/*     selectDecode: SelectDecode.t('a, 'result), */
/*   }; */

/*   // Run a select object on a given client, returning */
/*   let run: (Client.t('h, 'r, 'q), t('a, 'r), 'a) => P.t(Client.QueryResult.t('r)) = */
/*     (cli, {cache, toCacheKey, selectDecode}, sel) => { */
/*     let key = sel->toCacheKey; */
/*     switch (cache->M.get(key)) { */
/*       | None => { */
/*           let prom = cli->SelectDecode.run(selectDecode, sel); */
/*           cache->M.set(key, prom); */
/*           prom */
/*         } */
/*       | Some(resultProm) => resultProm */
/*     } */
/*   }; */
/* }; */
