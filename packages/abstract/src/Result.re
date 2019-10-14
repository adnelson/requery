let (resolve, then_, reject) = Js.Promise.(resolve, then_, reject);
type error =
  | RowDecodeError(RowDecode.error);

let encodeError: Utils.Json.encoder(error) =
  Utils.Json.Encode.(
    fun
    | RowDecodeError(e) => e |> object1("RowDecodeError", RowDecode.encodeError)
  );

exception Error(error);

type t('a) =
  | Error(error)
  | Success('a);

module Enc = Utils.Json.Encode;

let encode: Utils.Json.encoder('a) => Utils.Json.encoder(t('a)) =
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

let unwrapPromise: t('a) => Js.Promise.t('a) =
  fun
  | Success(x) => resolve(x)
  | Error(err) => reject(Error(err));

let unwrapPromise2: ((t('a), t('b))) => Js.Promise.t(('a, 'b)) =
  fun
  | (Success(x), Success(y)) => resolve((x, y))
  | (Error(err), _) => reject(Error(err))
  | (_, Error(err)) => reject(Error(err));
