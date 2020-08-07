include Node.Fs;
module P = PromiseUtils;

type callback('error, 'result) = (. Js.Nullable.t('error), Js.Undefined.t('result)) => unit;

let resultCallback: (result('r, 'e) => unit) => callback('e, 'r) =
  onResult =>
    (. nullableError, possiblyUndefinedData) =>
      switch (Js.Nullable.toOption(nullableError), Js.Undefined.toOption(possiblyUndefinedData)) {
      | (Some(err), _) => onResult(Error(err))
      | (None, Some(data)) => onResult(Ok(data))
      // TODO should this return unit instead?
      | _ => Js.Exn.raiseError("`readFile` returned no error and no data")
      };

module Error = {
  type t;

  [@bs.get] external message: t => string = "message";
  [@bs.get] external path: t => string = "path";
  [@bs.get] external code: t => string = "code";
  [@bs.get] external syscall: t => string = "syscall";
  [@bs.get] external errno: t => int = "errno";
};

type stringEncoding = [ | `utf8 | `ascii];

[@bs.module "fs"]
external readFile:
  (
    ~path: string,
    ~encoding: [@bs.string] [ | `utf8 | [@bs.as "ascii"] `ascii],
    ~cb: callback(Error.t, string)
  ) =>
  unit =
  "readFile";

// Read a file, return the result as a promise.
let readFileAsync: (~encoding: stringEncoding=?, string) => Js.Promise.t(string) =
  (~encoding=`utf8, path) =>
    P.makeWithError((~resolve, ~reject) =>
      readFile(
        ~path,
        ~encoding,
        ~cb=
          resultCallback(res =>
            switch (res) {
            | Ok(data) => resolve(. data)
            | Error(error) => reject(. error)
            }
          ),
      )
    );
