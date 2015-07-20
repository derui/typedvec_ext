module Std = Typedvec_ext.Std
module S = Std.Size
module V = Std.Algebra.Vec
module Q = Std.Qua

let%spec "Quaternion should make a quaternion having specified angle and axis" =
  let sub n v v' =
    let v = V.unsafe_get v n
    and v' = V.unsafe_get v' n in
    abs_float (v -. v') < 0.00001 in
  let axis = V.make S.three 0.0 in
  V.set ~index:0 ~v:1.0 axis;
  V.set ~index:1 ~v:2.0 axis;
  V.set ~index:2 ~v:3.0 axis;
  let v = Q.make ~angle:10.0 ~axis () in
  (abs_float (10.0 -. (Q.angle v)) < 0.00001) [@true];
  let axis = V.normalize axis in
  let axis' = Q.axis v in
  (sub 0 axis' axis) [@true];
  (sub 1 axis' axis) [@true];
  (sub 2 axis' axis) [@true]

let%spec "Quaternion can return identity" =
  let v = Q.identity () in
  (Q.axis v |> V.to_list) [@eq [0.0;0.0;0.0]];
  (Q.angle v) [@eq 0.0]
