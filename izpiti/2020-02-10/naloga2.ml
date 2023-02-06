type ('a, 'b) tree = 
  		| Empty
  		| ANode of ('a, 'b) tree * 'a * ('a, 'b) tree
  		| BNode of ('a, 'b) tree * 'b * ('a, 'b) tree


let aleaf x = ANode(Empty, x, Empty)

let bleaf y = BNode(Empty, y, Empty)

let test = ANode(bleaf true, 12, ANode(aleaf 0, 5, bleaf false))

let test1 = ANode(bleaf true, 12, ANode(bleaf false, 5, bleaf false))

let rec adepth tree = match tree with 
  | Empty -> 0
  | ANode(l, x, d) when l = Empty && d = Empty -> 1
  | BNode(l, y, d) when l = Empty && d = Empty-> 0
  | ANode(l, x, d) -> 1 + (if adepth l < adepth d then adepth d else adepth l)
  | BNode(l, y, d) -> 1 + (if adepth l < adepth d then adepth d else adepth l)

let rec bdepth tree = match tree with 
  | Empty -> 0
  | ANode(l, x, d) when l = Empty && d = Empty -> 0
  | BNode(l, y, d) when l = Empty && d = Empty-> 1
  | ANode(l, x, d) -> 1 + (if adepth l < adepth d then adepth d else adepth l)
  | BNode(l, y, d) -> 1 + (if adepth l < adepth d then adepth d else adepth l)


type result = {aNodes : int; bNodes : int}

let rec count_a = function
| Empty -> 0
| ANode(l, x, d) -> 1 + count_a l + count_a d
| BNode(l, x, d) -> count_a l + count_a d

let rec count_b = function
| Empty -> 0
| ANode(l, x, d) -> count_a l + count_a d
| BNode(l, x, d) -> 1 + count_a l + count_a d


let count tree = {aNodes = count_a tree; bNodes = count_b tree}

let is_typemirror