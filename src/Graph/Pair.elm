module Graph.Pair
  exposing
    ( getEdgeData
    , insertEdge
    , insertEdgeData
    , removeEdge
    , removeEdgeData
    )

import Graph as G exposing (Graph)


getEdgeData : ( comparable, comparable ) -> Graph comparable data edgeData -> Maybe edgeData
getEdgeData =
  uncurry G.getEdgeData


insertEdge : ( comparable, comparable ) -> Graph comparable data edgeData -> Graph comparable data edgeData
insertEdge =
  uncurry G.insertEdge


insertEdgeData : ( comparable, comparable ) -> edgeData -> Graph comparable data edgeData -> Graph comparable data edgeData
insertEdgeData =
  uncurry G.insertEdgeData


removeEdge : ( comparable, comparable ) -> Graph comparable data edgeData -> Graph comparable data edgeData
removeEdge =
  uncurry G.removeEdge


removeEdgeData : ( comparable, comparable ) -> Graph comparable data edgeData -> Graph comparable data edgeData
removeEdgeData =
  uncurry G.removeEdgeData


mapEdge : (( comparable, comparable ) -> Maybe edgeData1 -> Maybe edgeData2) -> Graph comparable data edgeData1 -> Graph comparable data edgeData2
mapEdge fn graph =
  G.mapEdge (curry fn) graph
