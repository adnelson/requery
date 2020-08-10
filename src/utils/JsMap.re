// Interface to the primitive Map type in JS. In practice, 'k
// must be a type that JavaScript is able to handle, so use carefully.
type t('k, 'v);

[@bs.new] external empty: unit => t('k, 'v) = "Map";
[@bs.new] external fromArray: array(('k, 'v)) => t('k, 'v) = "Map";
[@bs.val] external toArray: t('k, 'v) => array(('k, 'v)) = "Array.from";
[@bs.send] external has: (t('k, 'v), 'k) => bool = "has";
[@bs.send] [@bs.return nullable] external get: (t('k, 'v), 'k) => option('v) = "get";
[@bs.send] external setMut: (t('k, 'v), 'k, 'v) => t('k, 'v) = "set";
[@bs.send] external deleteMut: (t('k, 'v), 'k) => bool = "delete";

// Key/value pairs of a map.
type entries('k, 'a) = Js.Array.array_like(('k, 'a));

[@bs.new] external fromEntries: entries('k, 'a) => t('k, 'a) = "Map";
[@bs.send] external entries: t('k, 'a) => entries('k, 'a) = "entries";

[@bs.send] external keys: t('k, 'a) => Js.Array.array_like('k) = "keys";
[@bs.send] external values: t('k, 'a) => Js.Array.array_like('a) = "values";
