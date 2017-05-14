module Graph.RelativeOrdering exposing (RelativeOrdering(Before, After, Concurrent))

{-|
@docs RelativeOrdering
-}


{-| This type represents the relative ordering between two things in a graph.
-}
type RelativeOrdering
  = Before
  | After
  | Concurrent
