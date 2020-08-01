include Js.Promise;

[@bs.send] external flatMap: (t('a), 'a => t('b)) => t('b) = "then";

let transform: ('a => 'b, t('a)) => t('b) = (f, prom) => prom |> then_(x => resolve(f(x)));

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

let catchMap = (prom, f) => prom |> catch(err => err->f->resolve);

let finally: (unit => t(unit), t('a)) => t('a) =
  (action, prom) =>
    prom
    |> then_(result => {
         ignore(action());
         resolve(result);
       })
    |> catch(err => {
         ignore(action());
         Js.log(err);
         reject(Error(err));
       });
