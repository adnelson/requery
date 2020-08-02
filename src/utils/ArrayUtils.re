// Extra functions on arrays
open UtilsPrelude;
module O = Belt.Option;

include Belt.Array;

let toList = Belt.List.fromArray;
let mapToList = (arr, f) => arr->map(f)->toList;

[@bs.val] [@bs.variadic] external maxFloat: array(float) => float = "Math.max";

[@bs.val] [@bs.variadic] external maxInt: array(int) => int = "Math.max";

let max: array(float) => float = arr => reduce(arr, neg_infinity, max);
let min: array(float) => float = arr => reduce(arr, infinity, min);

let contains: (array('a), 'a) => bool = (arr, elem) => some(arr, e => e == elem);
let joinWith: (array(string), string) => string = (arr, sep) => Js.Array.joinWith(sep, arr);
let joinSpaces: array(string) => string = arr => joinWith(arr, " ");
let mapJoin: (array('a), ~prefix: string=?, ~suffix: string=?, string, 'a => string) => string =
  (arr, ~prefix="", ~suffix="", sep, f) => prefix ++ joinWith(map(arr, f), sep) ++ suffix;
let mapJoinWith: (array('a), string, 'a => string) => string =
  (arr, sep, f) => joinWith(map(arr, f), sep);
let mapJoinCommas = (arr, ~prefix=?, ~suffix=?, f) => mapJoin(arr, ~prefix?, ~suffix?, ", ", f);
let mapJoinSpaces = (arr, ~prefix=?, ~suffix=?, f) => mapJoin(arr, ~prefix?, ~suffix?, " ", f);
let mapJoinCommasParens = (arr, f) => mapJoin(arr, ~prefix="(", ~suffix=")", ", ", f);
let mapJoinIfNonEmpty:
  (array('a), ~onEmpty: string=?, ~prefix: string=?, ~suffix: string=?, string, 'a => string) =>
  string =
  (arr, ~onEmpty="", ~prefix="", ~suffix="", sep, f) =>
    switch (arr) {
    | [||] => onEmpty
    | _ => mapJoin(arr, ~prefix, ~suffix, sep, f)
    };

// like map, but argument order flipped
let flipMap: ('a => 'b, array('a)) => array('b) = (f, a) => map(a, f);

// like forEach, but reverse argument order
let flipForEach: ('a => 'b, array('a)) => unit = (f, a) => forEach(a, f);

// Find the first item in the array which matches a predicate, or return None.
let find: (array('a), 'a => bool) => option('a) =
  (arr, test) =>
    switch (keep(arr, test)) {
    | [||] => None
    | matches => Some(matches[0])
    };

// Find the first item in the array which matches a predicate, or raise an error.
let findExn: (array('a), 'a => bool) => 'a =
  (arr, test) =>
    switch (find(arr, test)) {
    | None => throw("No matching element in array")
    | Some(m) => m
    };

let pushMut = (arr: array('a), elem: 'a): unit => Js.Array.push(elem, arr) |> ignore;

// Mutates arr, adding each element of arr' to it.
let extend = (arr: array('a), arr': array('a)): unit =>
  forEach(arr', elem => Js.Array.push(elem, arr) |> ignore);

// Flatten an array of arrays.
let flat = (arr: array(array('a))): array('a) => {
  let res: array('a) = [||];
  forEach(arr, innerArr => extend(res, innerArr));
  res;
};

let head = (arr: array('a)): option('a) => get(arr, 0);
let nestedHead = (arr: array(array('a))): option('a) => O.flatMap(head(arr), a => get(a, 0));

// map and then flatten
let flatMap = (arr: array('a), f: 'a => array('b)): array('b) => flat(map(arr, f));

let sumInts = (arr: array(int)): int => reduce(arr, 0, (+));
let sumFloats = (arr: array(float)): float => reduce(arr, 0.0, (+.));

// Cross-product two arrays, applying a function to each pair.
let cross = (arr1: array('a), arr2: array('b), f: ('a, 'b) => 'c): array('c) => {
  flatMap(arr1, a => map(arr2, b => f(a, b)));
};

// Same as cross but operating on three arrays.
let cross3 =
    (arr1: array('a), arr2: array('b), arr3: array('c), f: ('a, 'b, 'c) => 'd): array('d) => {
  flatMap(arr1, a => flatMap(arr2, b => map(arr3, c => f(a, b, c))));
};

// Get the values of all of the `Some()` variants in an array of options.
let keepSome = (arr: array(option('a))): array('a) => keepMap(arr, x => x);

// Create a singleton array
let singleton = x => [|x|];
// Same as `map`, but with the arguments order reversed.
//  let map' = (f: 'a => 'b, arr: array('a)): array('b) => map(arr, f);

// Return a new array with the given index set to the given value.
let setPure = (arr, i, x) => {
  let arr' = copy(arr);
  let _ = set(arr', i, x);
  arr';
};

// Convenient alias, get first elements from a tuple array
let firsts: array(('a, 'b)) => array('a) = arr => map(arr, fst);

// Convenient alias, get second elements from a tuple array
let seconds: array(('a, 'b)) => array('b) = arr => map(arr, snd);