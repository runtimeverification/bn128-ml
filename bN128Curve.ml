open BN128Elements

type 'a point = Finite of 'a * 'a
              | Infinite

let two = Z.of_int 2
let three = Z.of_int 3
let nine = Z.of_int 9

let curve_order = Z.of_string "21888242871839275222246405745257275088548364400416034343698204186575808495617"

let is_on_curve sub pow b pt = match pt with
| Infinite -> true
| Finite (x, y) -> (sub (pow y two) (pow x three)) = b

let double add div mul mulp pow sub neg p = match p with
| Infinite -> Infinite
| Finite (x, y) -> 
    let l = div (mul (pow x two) (FQ.create three)) (mul y (FQ.create two)) in
    let newx = sub (pow l two) (mul x (FQ.create two)) in
    let newy = sub (add (mulp (neg l) newx) (mulp l x)) y in
    Finite (newx, newy)

let add add div mul mulp pow sub neg p1 p2 = match p1, p2 with
| Infinite, Finite (x1,x2) -> Finite (x1,x2)
| Finite (x1,x2), Infinite -> Finite (x1,x2)
| Infinite, Infinite -> Infinite
| Finite (x1, y1), Finite (x2, y2) ->
    if x1 = x2 && y1 = y2 then double add div mul mulp pow sub neg p1 else
    if x1 = x2 then Infinite else
    let l = div (sub y2 y1) (sub x2 x1) in
    let newx = sub (sub (pow l two) x1) x2 in
    let newy = sub (add (mulp (neg l) newx) (mulp l x1)) y1 in
    Finite (newx, newy)

let rec mul add double pt n = 
  if n = Z.zero then Infinite else
  if n = Z.one then pt else
  if (Z.erem n two) = Z.zero then mul add double (double pt) (Z.div n two) else
  add (mul add double (double pt) (Z.div n two)) pt

module type G = sig
  type t
  val b : t
  val g : t point
  val is_on_curve : t point -> bool
  val double : t point -> t point
  val add : t point -> t point -> t point
  val mul : t point -> Z.t -> t point
end

module G1 : G with type t = FQ.t = struct
  type t = FQ.t
  let b = FQ.create three
  let g = Finite (FQ.one, FQ.create two)
  let is_on_curve = is_on_curve FQ.sub FQ.pow b
  let double = double FQ.add FQ.div FQ.mul FQ.mul FQ.pow FQ.sub FQ.neg
  let add = add FQ.add FQ.div FQ.mul FQ.mul FQ.pow FQ.sub FQ.neg
  let mul = mul add double
end

module G2 : G with type t = FQP.t = struct
  type t = FQP.t
  let b = FQP.divp (FQP.create_fq2 [|three; Z.zero|]) (FQP.create_fq2 [|nine; Z.one|])
  let g = Finite (FQP.create_fq2
    [|Z.of_string "10857046999023057135944570762232829481370756359578518086990519993285655852781";
      Z.of_string "11559732032986387107991004021392285783925812861821192530917403151452391805634"|],
  FQP.create_fq2
    [|Z.of_string "8495653923123431417604973247489272438418190587263600148770280649306958101930";
      Z.of_string "4082367875863433681332203403145435568316851327593401208105741076214120093531"|])
  let is_on_curve = is_on_curve FQP.sub FQP.pow b
  let double = double FQP.add FQP.divp FQP.mul FQP.mulp FQP.pow FQP.sub FQP.neg
  let add = add FQP.add FQP.divp FQP.mul FQP.mulp FQP.pow FQP.sub FQP.neg
  let mul = mul add double
end

let w = FQP.create_fq12 [|Z.zero; Z.one; Z.zero; Z.zero; Z.zero; Z.zero; Z.zero; Z.zero; Z.zero; Z.zero; Z.zero; Z.zero|]

let expand coeffs = match coeffs with
[|x1; x2|] -> FQP.create_fq12 [|FQ.to_z x1; Z.zero; Z.zero; Z.zero; Z.zero; Z.zero; FQ.to_z x2; Z.zero; Z.zero; Z.zero; Z.zero; Z.zero|]
| _ -> invalid_arg "input not in G2"

let cast_g2 p = match p with
| Infinite -> Infinite
| Finite (x, y) -> 
    let x, y = FQP.coeffs x, FQP.coeffs y in
    let xcoeffs = [|FQ.sub (Array.get x 0) (FQ.mul (Array.get x 1) (FQ.create nine)); Array.get x 1|] in
    let ycoeffs = [|FQ.sub (Array.get y 0) (FQ.mul (Array.get y 1) (FQ.create nine)); Array.get y 1|] in
    let nx = expand xcoeffs in
    let ny = expand ycoeffs in
    Finite (FQP.mulp nx (FQP.pow w two), FQP.mulp ny (FQP.pow w three))

let cast_g1 p = match p with
| Infinite -> Infinite
| Finite (x, y) ->
  Finite (FQP.create_fq12
    [|FQ.to_z x; Z.zero; Z.zero; Z.zero; Z.zero; Z.zero; Z.zero; Z.zero; Z.zero; Z.zero; Z.zero; Z.zero|], FQP.create_fq12
    [|FQ.to_z y; Z.zero; Z.zero; Z.zero; Z.zero; Z.zero; Z.zero; Z.zero; Z.zero; Z.zero; Z.zero; Z.zero|])

module G12 : G with type t = FQP.t = struct
  type t = FQP.t
  let b = FQP.create_fq12 [|three; Z.zero; Z.zero; Z.zero; Z.zero; Z.zero; Z.zero; Z.zero; Z.zero; Z.zero; Z.zero; Z.zero|]
  let g = cast_g2 G2.g
  let is_on_curve = is_on_curve FQP.sub FQP.pow b
  let double = double FQP.add FQP.divp FQP.mul FQP.mulp FQP.pow FQP.sub FQP.neg
  let add = add FQP.add FQP.divp FQP.mul FQP.mulp FQP.pow FQP.sub FQP.neg
  let mul = mul add double
end
