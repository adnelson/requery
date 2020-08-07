type callback('error, 'result) = (. Js.Nullable.t('error), Js.Undefined.t('result)) => unit;

type t('error, 'result) = callback('error, 'result);

exception CannotDecodeCallbackArguments(string);

// Raises an exception if the error is not null.
let raiseOnErrorCallback: 'e 'r. ('r => unit) => t('e, 'r) =
  onResult =>
    (. nullableError, possiblyUndefinedData) =>
      switch (Js.Nullable.toOption(nullableError), Js.Undefined.toOption(possiblyUndefinedData)) {
      | (Some(err), _) => raise(Obj.magic(err))
      | (None, Some(data)) => onResult(data)
      // TODO should this return unit instead?
      | _ => raise(CannotDecodeCallbackArguments("callback was called with no arguments"))
      };

// Provides a `result` interface to callback handling.
let resultCallback: (result('r, 'e) => unit) => t('e, 'r) =
  onResult =>
    (. nullableError, possiblyUndefinedData) =>
      switch (Js.Nullable.toOption(nullableError), Js.Undefined.toOption(possiblyUndefinedData)) {
      | (Some(err), _) => onResult(Error(err))
      | (None, Some(data)) => onResult(Ok(data))
      // TODO should this return unit instead?
      | _ => raise(CannotDecodeCallbackArguments("callback was called with no arguments"))
      };
