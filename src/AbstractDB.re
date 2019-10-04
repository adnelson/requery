module O = Belt.Option;
let (resolve, then_, reject) = Js.Promise.(resolve, then_, reject);

type config = {
  host: string,
  database: string,
  port: int,
};

type error =
  | RowDecodeError(RowDecode.error);

let encodeError: Utils.encoder(error) =
  Utils.Json.Encode.(
    fun
    | RowDecodeError(e) => e |> object1("RowDecodeError", RowDecode.encodeError)
  );

exception Error(error);

module QueryResult = {
  type t('a) =
    | Error(error)
    | Success('a);

  module Enc = Utils.Json.Encode;

  let encode: Utils.encoder('a) => Utils.encoder(t('a)) =
    Enc.(
      encodeSuccess =>
        fun
        | Error(e) => e |> object1("Error", encodeError)
        | Success(x) => x |> object1("Success", encodeSuccess)
    );

  let unwrap: t('a) => 'a =
    fun
    | Success(x) => x
    | Error(err) => raise(Error(err));

  let unwrapPromise: ('a => Js.Promise.t('b), t('a)) => Js.Promise.t('b) =
    next =>
      fun
      | Success(x) => next(x)
      | Error(err) => reject(Error(err));
};

// This could be a functor, parameterized on an error type. Also, need
// to think about error handling.
module type DBType = {
  type result;
  type pool;
  type client;

  let resultToRows: result => array(Js.Json.t);
  let makePool: config => pool;
  let makeClient: pool => Js.Promise.t(client);
  let releaseClient: client => Js.Promise.t(unit);
  let releasePool: pool => Js.Promise.t(unit);
  let query: (client, SqlQuery.query) => Js.Promise.t(result);
};

module Query = (DB: DBType) => {
  let select =
      (
        ~logQuery=?,
        ~logResult=?,
        client: DB.client,
        query: QueryBuilder.select,
        decode: RowDecode.decoder('t),
      )
      : Js.Promise.t(QueryResult.t('t)) => {
    let _ = O.map(logQuery, f => f(query));
    DB.query(client, SqlQuery.Select(query))
    |> then_((result: DB.result) => {
         let _ = O.map(logResult, f => f(result));
         resolve(
           try (QueryResult.Success(decode(DB.resultToRows(result)))) {
           | RowDecode.Error(e) => QueryResult.Error(RowDecodeError(e))
           },
         );
       });
  };
};

/*
 module type MyType {
   type input;
   let action: input => unit;
 };

 module A: MyType = {
   type input = string;
   let action = i => Js.log(i);
 }

 module B: MyType = {
   type input = {
     x: string,
     y: string
   };

   let action = ({x, y}) => Js.log(x ++ ", " ++ y);
 }

 let _ = {
   A.action("hello");
   B.action({x: "hello", y: "world"});
 };

 */
