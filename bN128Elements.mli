val field_modulus : Z.t

module FQ : sig
  type t
  val create : Z.t -> t
  val add : t -> t -> t
  val mul : t -> t -> t
  val sub : t -> t -> t
  val div : t -> t -> t
  val pow : t -> Z.t -> t
  val neg : t -> t
  val one : t
  val zero: t

  val to_z : t -> Z.t
end

module FQP : sig
  type t
  val create : Z.t array -> Z.t array -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> FQ.t -> t
  val mulp : t -> t -> t
  val div : t -> FQ.t -> t
  val divp : t -> t -> t
  val pow : t -> Z.t -> t
  val neg : t -> t
  val inv : t -> t
  val one : Z.t array -> t
  val zero : Z.t array -> t

  val create_fq2 : Z.t array -> t
  val create_fq12 : Z.t array -> t

  val one_fq12 : t

  val coeffs : t -> FQ.t array
end
