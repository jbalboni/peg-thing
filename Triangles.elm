module Triangles where

import Lazy.List exposing (..)
import Lazy exposing (lazy)


isTriangular : Int -> Bool
isTriangular n =
  let
    lastTriangle = (List.reverse (toList (takeWhile (\x -> n >= x) tri)))
  in
    case lastTriangle of
      [] ->
        False
      x::_ ->
        x == n

tri : LazyList Int
tri =
  triangles 0 1

triangles : Int -> Int -> LazyList Int
triangles sum current = lazy <| \() ->
  let
    newSum = sum + current
  in
    Cons newSum (triangles newSum (current + 1))

rowTriangle : Int -> Int
rowTriangle n =
  case List.reverse (toList (take n tri)) of
    [] ->
      0
    first::_ ->
      first

rowNum : Int -> Int
rowNum pos =
  (List.length (toList (takeWhile (\x -> pos > x) tri))) + 1
