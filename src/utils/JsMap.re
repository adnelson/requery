// Interface to the primitive Map type in JS. In practice, 'k
// must be a type that JavaScript is able to handle, so use carefully.
type t('k, 'v);

[@bs.new] external empty: unit => t('k, 'v) = "Map";
[@bs.new] external fromArray: array(('k, 'v)) => t('k, 'v) = "Map";
[@bs.val] external toArray: t('k, 'v) => array(('k, 'v)) = "Array.from";
[@bs.send] external has: (t('k, 'v), 'k) => bool = "has";
[@bs.send] external setMut: (t('k, 'v), 'k, 'v) => t('k, 'v) = "set";
[@bs.send] external deleteMut: (t('k, 'v), 'k) => bool = "delete";
