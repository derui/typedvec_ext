(** This module provide type of Quaternion and quaternion related operations.

    Provided operations are as follows;
    - identity
    - normalize
    - multiply
    - convert to matrix
    Quaternion type provide from this module is depending on {!Vector} and {!Matrix4}.

    @author derui
    @version 0.1
*)

(** type of quaternion.
    user is only using by this module functions to be intermadiate.
    if need information of quaternion, using `axis' and `angle'
    function of this module.
*)
type t
open Typedvec.Std

(* This type of an axis of the Quaternion. Also known imaginaly number. *)
type axis = Size.three Algebra.s Algebra.vec
  
(* This type of an angle of the Quaternion. *)
type angle = float

(** construct identity quaternion.*)
val identity : unit -> t

(** construct a quaternion from angle of degree and rotation axis.
    if given axis is not unit vector, this function is using without
    change.
    To need unit axis, call `normalize' with target quaternion.
*)
val make : angle:angle -> axis:axis -> unit -> t

(** return a normalized quaternion.
    normalized quaternion has a unit axis.
*)
val normalize : t -> t

(** multiply first quaternion by second one.
    this function is precise math definition, so if some quaternion
    need to combine, multipling order is `right to left'.
*)
val multiply : t -> t -> t

(** This module is a helper to multiply quaternions.
    applying order of each function,
    ( *> ) is `first to second', but ( *< ) is `second to first',
    if equlivalence order of quaternions give.
*)
module Open : sig
  val ( *> ) : t -> t -> t
  val ( *< ) : t -> t -> t
end

(** extract informations from quaternion type variable. *)
val axis : t -> axis
val angle : t -> angle

(** Get the rotation matrix via four by four square matrix.*)
val to_mat : t -> (Size.four Algebra.s, Size.four Algebra.s) Algebra.mat

(** construct `obversing quaternion` between from and
    by obversing frequency.
    freq is only applied with between 0.0 and 1.0.
    if out of this range, raise exception.
*)
val slerp : from_quat:t -> to_quat:t -> freq:float -> unit -> t

(** Get stringified quaternion, example,
    (q | x y z)
*)
val to_string : t -> string
