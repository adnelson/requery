include Js.Promise;

[@bs.send] external flatMap: (t('a), 'a => t('b)) => t('b) = "then";

[@bs.send] external map: (t('a), 'a => 'b) => t('b) = "then";

// Fire off a promise and ignore its result
let fireOff: 'a. (unit => t(unit)) => unit = makePromise => makePromise()->ignore;

// Treat a promise as an applicative functor.
let apply: 'a 'b. (t('a => 'b), t('a)) => t('b) =
  (funcPromise, nextPromise) =>
    funcPromise->flatMap(f => nextPromise->flatMap(x => x->f->resolve));

// Like apply, but computes the promises in parallel.
let applyParallel: 'a 'b. (t('a => 'b), t('a)) => t('b) =
  (funcPromise, nextPromise) => all2((funcPromise, nextPromise))->map(((f, x)) => f(x));

// Annoyingly Js.Promise requires that you reject with an exn
[@bs.val] [@bs.scope "Promise"] external rejectError: 'a => t('b) = "reject";

let tap: 'a. (t('a), 'a => unit) => t('a) =
  (prom, f) =>
    prom->map(x => {
      f(x);
      x;
    });

let then2: (('a, 'b) => t('c), t(('a, 'b))) => t('c) =
  (f, prom) => prom |> then_(((a, b)) => f(a, b));
let then3: (('a, 'b, 'c) => t('d), t(('a, 'b, 'c))) => t('d) =
  (f, prom) => prom |> then_(((a, b, c)) => f(a, b, c));
let rLog: 'a => t(unit) = x => resolve(Js.log(x));
let rLog2: ('a, 'b) => t(unit) = (a, b) => resolve(Js.log2(a, b));
let rLog3: ('a, 'b, 'c) => t(unit) = (a, b, c) => resolve(Js.log3(a, b, c));
let rLog4: ('a, 'b, 'c, 'd) => t(unit) = (a, b, c, d) => resolve(Js.log4(a, b, c, d));
let rLogReturn: ('a => 'b, 'a) => t('a) =
  (toLog, x) => {
    Js.log(toLog(x));
    resolve(x);
  };
exception Error(error);

[@bs.send] external catchMap: (t('a), error => 'a) => t('a) = "catch";

[@bs.send] external finally: (t('a), unit => unit) => t('a) = "finally";
