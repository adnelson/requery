// Things to `open` in every `*Utils` module
module IMap = Belt.Map.Int;
module ISet = Belt.Set.Int;
module SMap = Belt.Map.String;
module SSet = Belt.Set.String;

// Throw an exception as a native javascript error. Acts like failwith but
// will have a stack trace if triggered.
let throw: string => 'a = Js.Exn.raiseError;

external id: 'a => 'a = "%identity";

let uncurry: (('a, 'b) => 'c, ('a, 'b)) => 'c = (f, (a, b)) => f(a, b);
let uncurry3: (('a, 'b, 'c) => 'd, ('a, 'b, 'c)) => 'd = (f, (a, b, c)) => f(a, b, c);
let uncurry4: (('a, 'b, 'c, 'd) => 'e, ('a, 'b, 'c, 'd)) => 'e =
  (f, (a, b, c, d)) => f(a, b, c, d);

// Given a function which expects a tuple, turn it into a function which expects two arguments.
let curry: ((('a, 'b)) => 'c, 'a, 'b) => 'c = (f, a, b) => f((a, b));
let curry3: ((('a, 'b, 'c)) => 'd, 'a, 'b, 'c) => 'd = (f, a, b, c) => f((a, b, c));
let curry4: ((('a, 'b, 'c, 'd)) => 'd, 'a, 'b, 'c, 'd) => 'd =
  (f, a, b, c, d) => f((a, b, c, d));

// Map two functions over a 2-tuple.
let mapTup2: 'a1 'a2 'b1 'b2. ('a1 => 'a2, 'b1 => 'b2, ('a1, 'b1)) => ('a2, 'b2) =
  (f, g, (a, b)) => (f(a), g(b));

// Map a function over the first element in a tuple.
let mapFst: 'a 'b 'c. ('a => 'c, ('a, 'b)) => ('c, 'b) = (f, (a, b)) => (f(a), b);

// Map a function over the second element in a tuple.
let mapSnd: 'a 'b 'c. ('b => 'c, ('a, 'b)) => ('a, 'c) = (f, (a, b)) => (a, f(b));
