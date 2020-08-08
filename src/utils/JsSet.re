// Interface to the primitive Set type in JS. In practice, 'a
// must be a type that JavaScript is able to handle, so use carefully.
type t('a);

[@bs.new] external empty: unit => t('a) = "Set";
[@bs.new] external fromArray: array('a) => t('a) = "Set";
[@bs.val] external toArray: t('a) => array('a) = "Array.from";
[@bs.send] external has: (t('a), 'a) => bool = "has";
[@bs.send] external addMut: (t('a), 'a) => t('a) = "add";
[@bs.send] external deleteMut: (t('a), 'a) => bool = "delete";
