// The result of decoding a database response. Uses Belt.Result under the hood.
open Belt.Result;

let (resolve, then_, reject) = Js.Promise.(resolve, then_, reject);
type error =
  | RowDecodeError(RowDecode.error);

let encodeError: Utils.Json.encoder(error) =
  Utils.Json.Encode.(
    fun
    | RowDecodeError(e) => e |> object1("RowDecodeError", RowDecode.encodeError)
  );

exception Error(error);

type t('a) = Belt.Result.t(error, 'a);
let ok = x => Ok(x);
let error = e => Error(e);

module Enc = Utils.Json.Encode;

let encode: Utils.Json.encoder('a) => Utils.Json.encoder(t('a)) =
  Enc.(
    encodeOk =>
      fun
      | Error(e) => e |> object1("Error", encodeError)
      | Ok(x) => x |> object1("Ok", encodeOk)
  );

let unwrap: t('a) => 'a =
  fun
  | Ok(x) => x
  | Error(err) => raise(Error(err));

let unwrapPromise: t('a) => Js.Promise.t('a) =
  fun
  | Ok(x) => resolve(x)
  | Error(err) => reject(Error(err));

let unwrapPromise2: ((t('a), t('b))) => Js.Promise.t(('a, 'b)) =
  fun
  | (Ok(x), Ok(y)) => resolve((x, y))
  | (Error(err), _) => reject(Error(err))
  | (_, Error(err)) => reject(Error(err));
