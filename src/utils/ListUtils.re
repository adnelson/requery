include Belt.List;

let mapToArray: (list('a), 'a => 'b) => array('b) = (l, f) => toArray(map(l, f));
//let amap = mapToArray;
//let mapa: (array('a), 'a => 'b) => list('b) = (a, f) => a->Belt.Array.map(f)->fromArray;
//let mapa = fromArrayMap;
