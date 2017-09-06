open BN128Elements

val curve_order : Z.t

type 'a point = Finite of 'a * 'a
              | Infinite
module type G = sig
  type t
  val b : t
  val g : t point
  val is_on_curve : t point -> bool
  val double : t point -> t point
  val add : t point -> t point -> t point
  val mul : t point -> Z.t -> t point
end

module G1 : G with type t = FQ.t
module G2 : G with type t = FQP.t
module G12 : G with type t = FQP.t

val cast_g2 : G2.t point -> G12.t point
val cast_g1 : G1.t point -> G12.t point
