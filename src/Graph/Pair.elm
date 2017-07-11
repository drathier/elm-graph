module Graph.Pair
  exposing
    ( getEdgeData
    , insertEdge
    , insertEdgeData
    , removeEdge
    , removeEdgeData
    , mapEdge
    )

{-| Graph.Pair provides an alternative api for all functions that in some way take an edge as an argument. All functions in the normal api take edges as two arguments. This module provides the 2-tuple alternatives to those functions. It's basically just doing `uncurry` for you.

@docs getEdgeData, insertEdge, insertEdgeData, removeEdge, removeEdgeData, mapEdge

-}

import Graph as G exposing (Graph)


{-| Uncurried version of [`getEdgeData`](Graph#getEdgeData).
-}
getEdgeData : ( comparable, comparable ) -> Graph comparable data edgeData -> Maybe edgeData
getEdgeData =
  uncurry G.getEdgeData


{-| Uncurried version of [`insertEdge`](Graph#insertEdge).
-}
insertEdge : ( comparable, comparable ) -> Graph comparable data edgeData -> Graph comparable data edgeData
insertEdge =
  uncurry G.insertEdge


{-| Uncurried version of [`insertEdgeData`](Graph#insertEdgeData).
-}
insertEdgeData : ( comparable, comparable ) -> edgeData -> Graph comparable data edgeData -> Graph comparable data edgeData
insertEdgeData =
  uncurry G.insertEdgeData


{-| Uncurried version of [`removeEdge`](Graph#removeEdge).
-}
removeEdge : ( comparable, comparable ) -> Graph comparable data edgeData -> Graph comparable data edgeData
removeEdge =
  uncurry G.removeEdge


{-| Uncurried version of [`removeEdgeData`](Graph#removeEdgeData).
-}
removeEdgeData : ( comparable, comparable ) -> Graph comparable data edgeData -> Graph comparable data edgeData
removeEdgeData =
  uncurry G.removeEdgeData


{-| Uncurried version of [`updateEdge`](Graph#updateEdge).
-}
updateEdge : ( comparable, comparable ) -> (Maybe edgeData -> Maybe edgeData) -> Graph comparable data edgeData -> Graph comparable data edgeData
updateEdge =
  uncurry G.updateEdge


{-| Uncurried version of [`mapEdge`](Graph#mapEdge).
-}
mapEdge : (( comparable, comparable ) -> Maybe edgeData1 -> Maybe edgeData2) -> Graph comparable data edgeData1 -> Graph comparable data edgeData2
mapEdge fn graph =
  G.mapEdge (curry fn) graph
