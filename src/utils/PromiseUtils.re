include Js.Promise;

[@bs.send] external flatMap: (t('a), 'a => t('b)) => t('b) = "then";
[@bs.send] external map: (t('a), 'a => 'b) => t('b) = "then";

let transform: ('a => 'b, t('a)) => t('b) = (f, prom) => prom |> then_(x => resolve(f(x)));

// Annoyingly Js.Promise requires that you reject with an exn
[@bs.val] [@bs.scope "Promise"] external rejectError: 'a => t('b) = "reject";

let then2: (('a, 'b) => t('c), t(('a, 'b))) => t('c) =
  (f, prom) => prom |> then_(((a, b)) => f(a, b));
let then3: (('a, 'b, 'c) => t('d), t(('a, 'b, 'c))) => t('d) =
  (f, prom) => prom |> then_(((a, b, c)) => f(a, b, c));
let rLog: 'a => Js.Promise.t(unit) = x => Js.Promise.resolve(Js.log(x));
let rLog2: ('a, 'b) => Js.Promise.t(unit) = (a, b) => Js.Promise.resolve(Js.log2(a, b));
let rLogReturn: ('a => 'b, 'a) => Js.Promise.t('a) =
  (toLog, x) => {
    Js.log(toLog(x));
    Js.Promise.resolve(x);
  };
exception Error(error);

[@bs.send] external catchMap: (t('a), error => 'a) => t('a) = "catch";

[@bs.send] external finally: (t('a), unit => unit) => t('a) = "finally";
