datatype tree =
    Leaf of int
  | Node of tree * tree

fun add1Tree (t: tree): tree =
   case t of
      Leaf n        => Leaf (n + 1)
    | Node (t1, t2) => Node (add1Tree t1, add1Tree t2)

fun buildTree (power: int): tree =
   let
      fun graftTree (root: int, power: int): tree =
         if power = 0
            then Leaf root
            else Node ( graftTree (root, power-1)
                      , graftTree (root + (Int.fromLarge (IntInf.pow (2, power-1))), power-1)
                      )
   in
      graftTree (1, power)
   end

fun showTreePrec (p: int, t: tree): string =
   let
      val openParen  = if (p > 10) then "(" else ""
      val closeParen = if (p > 10) then ")" else ""
   in
      case t of
         Leaf n =>
           openParen
             ^ "Leaf "
             ^ Int.toString n
             ^ closeParen
       | Node (t1, t2) =>
           openParen
             ^ "Node "
             ^ showTreePrec (11, t1)
             ^ " "
             ^ showTreePrec (11, t2)
             ^ closeParen
   end

fun showTree (t: tree): string = showTreePrec (0, t)

val _ = print (showTree (buildTree 5) ^ "\n")
