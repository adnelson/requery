// Tools to combine an abstract type which can be rendered into a
// query with a decoder of the result. This can be thought of as an alternate
// API to running the individual query functions in `Client`: instead construct
// query/decoder pairs and use the functions in this module to operate on them.
module P = PromiseUtils;
module M = MapUtils;

module SelectDecode = {
  type t('sel, 'output) = {
    toSelect: QueryBuilder.toSelect('sel),
    fromRows: RowDecode.fromRows('output),
  };

  let make = (~toSelect, ~fromRows) => {toSelect, fromRows};
  let toSelect = ({toSelect}) => toSelect;
  let fromRows = ({fromRows}) => fromRows;

  // Run a select object on a given client, returning
  let run: (Client.t('h, 'r, 'q), t('s, 'o), 's) => P.t(Client.QueryResult.t('o)) =
    (cli, {toSelect, fromRows}, sel) => cli->Client.select(fromRows, sel->toSelect);
};

// Adds caching capabilities to a select. Very basic for now. Might be better
// to make into an independent composable abstraction to avoid code duplication...
module CachedSelectDecode = {
  type t('a, 'result) = {
    // Mutable map
    cache: M.t(string, P.t(Client.QueryResult.t('result))),
    toCacheKey: 'a => string,
    removeErrorsFromCache: bool,
    selectDecode: SelectDecode.t('a, 'result),
  };

  // Run a query, caching the selection to avoid repeated queries.
  let run: (Client.t('h, 'r, 'q), t('a, 'r), 'a) => P.t(Client.QueryResult.t('r)) =
    (cli, {cache, toCacheKey, removeErrorsFromCache, selectDecode}, sel) => {
      let key = sel->toCacheKey;
      switch (cache->M.get(key)) {
      | None =>
        let prom_ = cli->SelectDecode.run(selectDecode, sel);
        let prom =
          !removeErrorsFromCache
            ? prom_
            : prom_
              |> P.catch(err => {
                   cache->M.delete(key);
                   P.rejectError(err);
                 });
        cache->M.set(key, prom);
        prom;
      | Some(resultProm) => resultProm
      };
    };
};
