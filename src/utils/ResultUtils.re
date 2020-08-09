include Belt.Result;

// Map a function over a result, if it's a success.
let map: ('a => 'b, t('a, 'err)) => t('b, 'err) =
  f =>
    fun
    | Ok(x) => Ok(f(x))
    | Error(e) => Error(e);

let unwrap: t('a, exn) => 'a =
  fun
  | Ok(x) => x
  | Error(err) => raise(err);

let unwrapPromise: t('a, exn) => Js.Promise.t('a) =
  fun
  | Ok(x) => Js.Promise.resolve(x)
  | Error(err) => Js.Promise.reject(err);

let unwrapPromise2: ((t('a, exn), t('b, exn))) => Js.Promise.t(('a, 'b)) =
  fun
  | (Ok(x), Ok(y)) => Js.Promise.resolve((x, y))
  | (Error(err), _) => Js.Promise.reject(err)
  | (_, Error(err)) => Js.Promise.reject(err);

let mapError = f =>
  fun
  | Ok(_) as result => result
  | Error(e) => Error(f(e));

// Apply a function, returning Ok(result). If an exception is
// thrown by the function, return Error(exception). This can be
// used to build an error boundary around code which might fail.
//
// exception BadNumber(int)
// let badFunction = n => n == 3 ? raise(BadNumber(n)) : n + 1
// expect(12->catchExn(badFunction)).toEqual(Ok(13))
// expect(3->catchExn(badFunction)).toEqual(Error(BadNumber(3)))
//
let catchExn: ('a, 'a => 'b) => t('b, exn) =
  (x, f) =>
    try(Ok(x->f)) {
    | e => Error(e)
    };
