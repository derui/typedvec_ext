open Typedvec.Std
module A = Algebra

(* This type of an axis of the Quaternion. Also known imaginaly number. *)
type axis = Size.three A.s A.vec
  
(* This type of an angle of the Quaternion. *)
type angle = float

type t = {
  quat_axis: axis;
  quat_angle: angle;
}

(* Constants. *)
let pi = 3.1415926535897232

let epsilon = 0.00001

let identity () = {quat_axis = A.Vec.make Size.three 0.0; quat_angle = 1.0}

let make ~angle ~axis () =
  let angle = angle /. 2.0 in
  let radian = pi /. 180.0 *. angle in
  let base_sin = sin radian  in
  let normal = A.Vec.normalize axis in
  {quat_angle = cos radian;
   quat_axis = A.Vec.scalar ~v:normal ~scale:base_sin
  }

let norm {quat_axis = axis;quat_angle = angle} =
  let module V = Algebra.Vec in
  let presquare = angle *. angle +. (V.norm_sqrt axis) in
  sqrt presquare

let normalize quat =
  let normed = norm quat in
  let module V = Algebra.Vec in
  (* norm should be over 0.0 *)
  if normed > 0.0 then
    let mangle = 1.0 /. normed in
    {quat_angle = quat.quat_angle *. mangle;
     quat_axis = V.scalar ~v:quat.quat_axis ~scale:mangle
    }
  else
    identity ()

let extract_vec v = 
  let module V = Algebra.Vec in
  match (V.get ~index:0 v, V.get ~index:1 v, V.get ~index:2 v) with
  | (Some(x), Some(y), Some(z)) -> (x,y,z)
  | _ -> failwith "Quaternion axis must have size three."

let multiply first second =
  let module V = Algebra.Vec in
  let w1 = first.quat_angle
  and (x1, y1, z1) = extract_vec first.quat_axis
  and w2 = second.quat_angle
  and (x2, y2, z2) = extract_vec second.quat_axis in
  { quat_angle = w1 *. w2 -. x1 *. x2 -. y1 *. y2 -. z1 *. z2;
    quat_axis = let v = V.copy first.quat_axis in
                V.set v ~index:0 ~v:(w1 *. x2 +. x1 *. w2 +. y1 *. z2 -. z1 *. y2);
                V.set v ~index:1 ~v:(w1 *. y2 +. y1 *. w2 +. z1 *. x2 -. x1 *. z2);
                V.set v ~index:2 ~v:(w1 *. z2 +. z1 *. w2 +. x1 *. y2 -. y1 *. x2);
                v
  }

module Open : sig
  val ( *> ) : t -> t -> t
  val ( *< ) : t -> t -> t
end = struct
    let ( *> ) f s = multiply f s
    let ( *< ) f s = multiply f s
end

let to_mat quat =
  let module V = Algebra.Vec in
  let module M = Algebra.Mat in
  let module S = Size in
  let w = quat.quat_angle
  and (x, y, z) = extract_vec quat.quat_axis in
  let mat = M.make ~col:Size.four ~row:Size.four ~init:0.0 in
  M.set mat ~row:0 ~col:0 ~v:(1.0 -. (2.0 *. y *. y) -. (2.0 *. z *. z));
  M.set mat ~row:0 ~col:1 ~v:((2.0 *. x *. y) +. (2.0 *. w *. z));
  M.set mat ~row:0 ~col:2 ~v:((2.0 *. x *. z) -. (2.0 *. w *. y));
  M.set mat ~row:0 ~col:3 ~v:0.0;
  M.set mat ~row:1 ~col:0 ~v:((2.0 *. x *. y) -. (2.0 *. w *. z));
  M.set mat ~row:1 ~col:1 ~v:(1.0 -. (2.0 *. x *. x) -. (2.0 *. z *. z));
  M.set mat ~row:1 ~col:2 ~v:((2.0 *. y *. z) +. (2.0 *. w *. x));
  M.set mat ~row:1 ~col:3 ~v:0.0;
  M.set mat ~row:2 ~col:0 ~v:((2.0 *. x *. z) +. (2.0 *. w *. y));
  M.set mat ~row:2 ~col:1 ~v:((2.0 *. y *. z) -. (2.0 *. w *. x));
  M.set mat ~row:2 ~col:2 ~v:(1.0 -. (2.0 *. x *. x) -. (2.0 *. y *. y));
  M.set mat ~row:2 ~col:3 ~v:0.0;
  M.set mat ~row:3 ~col:0 ~v:0.0;
  M.set mat ~row:3 ~col:1 ~v:0.0;
  M.set mat ~row:3 ~col:2 ~v:0.0;
  M.set mat ~row:3 ~col:3 ~v:1.0;
  mat

(* construct dot product from q1 and q2. *)
let dot q1 q2 =
  let w = q1.quat_angle *. q2.quat_angle
  and (x1, y1, z1) = extract_vec q1.quat_axis
  and (x2, y2, z2) = extract_vec q2.quat_axis in
  w +. (x1 *. x2) +. (y1 *. y2) +. (z1 *. z2)

(* return tuple quaternions between q1 and q2.
   if q1 and q2 given quaternion's dot is negate, return
   (-q1, q2). q2 is without change always.
*)
let minimum_angle q1 q2 =
  let module V = Algebra.Vec in
  let dotted = dot q1 q2 in
  if dotted < 0.0 then
    ({quat_angle = -.dotted; quat_axis = V.scalar ~v:q1.quat_axis ~scale:(-1.0)}, q2)
  else
    (q1, q2)

let slerp ~from_quat ~to_quat ~freq () =
  let (from_quat, to_quat) = minimum_angle from_quat to_quat in
  let dotted = dot from_quat to_quat in
  let correction_freq freq dotted =
    (* calculate correction ratio from from-to frequently.
       However, if from and to are almost to point equalivalence
       direction, return liner correction ratio.
    *)
    if dotted > (1.0 -. epsilon) then
      (1.0 -. freq, freq)
    else
      let sin_dot = sqrt (1.0 -. dotted *. dotted) in
      let theta = atan2 sin_dot dotted in
      let over = 1.0 /. sin_dot in
      ((sin ((1.0 -. freq) *. theta)) *. over,
       (sin (freq *. theta)) *. over
      )
  in
  let (k0, k1) = correction_freq freq dotted in
  let w0 = from_quat.quat_angle
  and (x0,y0,z0) = extract_vec from_quat.quat_axis
  and w1 = to_quat.quat_angle
  and (x1,y1,z1) = extract_vec to_quat.quat_axis in
  let module V = A.Vec in
  {quat_angle = w0 *. k0 +. w1 *. k1;
   quat_axis = let v = V.copy from_quat.quat_axis in
               V.set ~index:0 ~v:(x0 *. k0 +. x1 *. k1) v;
               V.set ~index:1 ~v:(y0 *. k0 +. y1 *. k1) v;
               V.set ~index:2 ~v:(z0 *. k0 +. z1 *. k1) v;
               v
  }

let to_string quat =
  let angle = quat.quat_angle
  and axis = quat.quat_axis in
  let (x, y, z) = extract_vec axis in
  Printf.sprintf "(%f|%f %f %f)" angle x y z

let angle {quat_angle;_} = 180.0 /. pi *. 2.0 *. (acos quat_angle)
let axis {quat_axis;quat_angle} =
  let sin_theta_sq = 1.0 -. quat_angle *. quat_angle in
  (* if calculated sin is lesser than zero, return zero vector *)
  if sin_theta_sq <= 0.0 then
    (identity ()).quat_axis
  else
    let over_sin = 1.0 /. sqrt sin_theta_sq in
    A.Vec.scalar ~v:quat_axis ~scale:over_sin
