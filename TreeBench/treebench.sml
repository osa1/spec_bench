datatype tree =
    Leaf of int
  | Node of tree * tree

fun sumTree (t: tree): tree =
    case t of
       Leaf n        => Leaf (n + 1)
     | Node (t1, t2) => Node (sumTree t1, sumTree t2)

val _ = print "test\n"
