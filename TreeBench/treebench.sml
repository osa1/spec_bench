open Timer

fun putStrLn (str: string) = print (str ^ "\n")
fun printLargeInt (i: LargeInt.int) = putStrLn (LargeInt.toString i)

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
                      , graftTree (root + Int.fromLarge (IntInf.pow (2, power-1)), power-1)
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

fun benchmark (power: int): Time.time * Time.time * Time.time =
   let
      val t = buildTree power

      val cpuTimer = startCPUTimer ()
      val realTimer = startRealTimer ()

      val _ = add1Tree t

      val {usr = usr, sys = sys} = checkCPUTimer cpuTimer
      val realTime = checkRealTimer realTimer
   in
      (usr, sys, realTime)
   end

val power = case map Int.fromString (CommandLine.arguments ()) of
                    SOME i :: _ => i
                  | _           => raise Fail "Can't parse number of iterations"
val (usr, sys, realTime) = benchmark power
val _ = printLargeInt (Time.toNanoseconds usr)
val _ = printLargeInt (Time.toNanoseconds sys)
val _ = printLargeInt (Time.toNanoseconds realTime)
