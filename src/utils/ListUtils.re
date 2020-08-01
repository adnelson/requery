include Belt.List;

let amap: (list('a), 'a => 'b) => array('b) = (l, f) => toArray(map(l, f));
let mapa: (array('a), 'a => 'b) => list('b) = (a, f) => a->Belt.Array.map(f)->fromArray;
