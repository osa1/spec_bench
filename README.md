In this benchmark we measure costs of indirections(pointers). Imagine we have
the standard list type, but we specialize it for `Int`s, and make the `Int`
field strict:

```haskell
data IntList
  = ILNil
  | ILCons {-# UNPACK #-} !Int IntList
```

This representation doesn't have the indirection polymorphic list has: Elements
in cons cells are held unboxed.

Now using this type and `[Int]`, we benchmark two very simple operations:

1. We run `sum :: [Int] -> Int` which is simply a loop that adds up all the
   elements in the list.
   (NOTE: `sum` in `base` is more general than we need and it's slower for this
   reason, so we have our own `sum` for fairness)

2. We run `length :: [Int] -> Int` which measures length of the list.

Raw results and a plot can be seen below.

Slowness of generic, boxed representation was expected(24ms vs 39ms, keep
reading for some explanations). A more interesting part is even `length` is
slower on generic lists(23ms specialized list vs 34ms generic list).

Our explanation of this slowness is the garbage collection: Even if we never
traverse those pointers, garbage collector will. It'll also do extra copying
because we have more heap objects.

Note that the length of our lists are 10 ^ 7. This approximately makes 10Kb of
extra data in the case of generic representation which may fill a whole L1 cache
but it's probably not enough for filling a L2 cache. Our guess is that as the
lists get bigger this performance difference would increase because of wasted
cache and cache misses.

In the case of `sum`, the problem with generic representation is not only the
pointer traversals. Since functions that operate on generic lists can't assume
that the fields(head and tail of the cons cell) will be strict, they need to
enter the thunks. This means for `sum` code, we do a jump to a `Int` thunk entry
code in each iteration. This can be seen in the STG code for unboxed and boxed
variants of lists.

This is STG for generic list's sum function:

```haskell
Main.$wglSumAcc
  :: GHC.Prim.Int# -> Main.GL GHC.Types.Int -> GHC.Prim.Int# =
    \r srt:SRT:[] [ww_saei w_saej]
        case w_saej of _ {
          Main.GLNil -> ww_saei;
          Main.GLCons h_sael t_saem ->
              case h_sael of _ {
                GHC.Types.I# y_saeo ->
                    case +# [ww_saei y_saeo] of sat_saep {
                      __DEFAULT -> Main.$wglSumAcc sat_saep t_saem;
                    };
              };
        };
```

Note how we have a pattern matching on `h_sael`. This means entering the thunk
for head of the cons cell every time.

This is same code for our unboxed list:

```haskell
Main.$wspecListSumAcc
  :: GHC.Prim.Int# -> Main.IntList -> GHC.Prim.Int# =
    \r srt:SRT:[] [ww_safc w_safd]
        case w_safd of _ {
          Main.ILNil -> ww_safc;
          Main.ILCons dt_saff l_safg ->
              case +# [ww_safc dt_saff] of sat_safh {
                __DEFAULT -> Main.$wspecListSumAcc sat_safh l_safg;
              };
        };
```

The `case` expression for the head of cons cell has disappeared, no thunk
entries!

## TODO: Talk about why generic strict list is not any faster

## Plots

(Plots need to be updated for new data)

## Results

Length:

```
benchmarking Haskell list (size: 10^7)/length
time                 37.67 ms   (37.51 ms .. 37.87 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 37.72 ms   (37.64 ms .. 37.86 ms)
std dev              204.1 μs   (137.8 μs .. 266.8 μs)

benchmarking Generic list (size: 10^7)/length
time                 37.68 ms   (37.41 ms .. 37.91 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 37.73 ms   (37.63 ms .. 37.84 ms)
std dev              207.2 μs   (169.1 μs .. 261.2 μs)

benchmarking Generic strict list (size: 10^7)/length
time                 37.36 ms   (37.19 ms .. 37.52 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 37.36 ms   (37.27 ms .. 37.44 ms)
std dev              166.7 μs   (130.2 μs .. 208.7 μs)

benchmarking Unboxed Int list (size: 10^7)/length
time                 24.24 ms   (24.14 ms .. 24.32 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 24.06 ms   (23.99 ms .. 24.12 ms)
std dev              151.8 μs   (119.4 μs .. 196.1 μs)
```

Sum:

```
benchmarking Haskell list (size: 10^7)/sum
time                 1.375 s    (1.126 s .. 1.710 s)
                     0.993 R²   (0.979 R² .. 1.000 R²)
mean                 1.420 s    (1.376 s .. 1.457 s)
std dev              59.22 ms   (0.0 s .. 64.32 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking Generic list (size: 10^7)/sum
time                 39.57 ms   (39.43 ms .. 39.71 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 39.72 ms   (39.63 ms .. 39.82 ms)
std dev              189.3 μs   (136.1 μs .. 265.0 μs)

benchmarking Generic strict list (size: 10^7)/sum
time                 39.83 ms   (39.70 ms .. 39.97 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 39.74 ms   (39.70 ms .. 39.79 ms)
std dev              100.6 μs   (69.95 μs .. 145.3 μs)

benchmarking Unboxed Int list (size: 10^7)/sum
time                 24.91 ms   (24.76 ms .. 25.05 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 24.81 ms   (24.75 ms .. 24.88 ms)
std dev              143.6 μs   (115.4 μs .. 170.4 μs)
```

The reason `Haskell list` variant is super slow is because the type of `sum` is
too general:

```haskell
sum :: (Num a, Foldable t) => t a -> a
```

This is why we implemented our variant `Generic list` for fairness. (TODO: Maybe
talk a bit about code for very general `sum`)
