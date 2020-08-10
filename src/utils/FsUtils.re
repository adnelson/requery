include Node.Fs;
module P = PromiseUtils;

module Error = {
  type t;

  [@bs.get] external message: t => string = "message";
  [@bs.get] external path: t => string = "path";
  [@bs.get] external code: t => string = "code";
  [@bs.get] external syscall: t => string = "syscall";
  [@bs.get] external errno: t => int = "errno";
};

type stringEncoding = [ | `utf8 | `ascii];

// Read a file expecting a string back.
[@bs.module "fs"]
external readStringFile:
  (
    ~path: string,
    ~encoding: [@bs.string] [ | `utf8 | [@bs.as "ascii"] `ascii],
    ~cb: CallbackUtils.t(Error.t, string)
  ) =>
  unit =
  "readFile";

// Read a file, return the result as a promise.
let readFileAsync: (~encoding: stringEncoding=?, string) => Js.Promise.t(string) =
  (~encoding=`utf8, path) =>
    P.makeWithError((~resolve, ~reject) =>
      readStringFile(
        ~path,
        ~encoding,
        ~cb=
          CallbackUtils.resultCallback(res =>
            switch (res) {
            | Ok(data) => resolve(. data)
            | Error(error) => reject(. error)
            }
          ),
      )
    );
