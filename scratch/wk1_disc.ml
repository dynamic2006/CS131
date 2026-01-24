type 'a tree =
  | Empty
  | Node of 'a * 'a tree * 'a tree

let rec insert x tree =
  match tree with
  | Empty -> Node (x, Empty, Empty)
  | Node (value, left, right) ->
      if x < value then
        Node (value, insert x left, right)
      else if x > value then
        Node (value, left, insert x right)
      else
        tree

let fib n =
  let rec helper i a b =
    if i = n then a
    else helper (i + 1) b (a + b)
  in
  helper 0 0 1
