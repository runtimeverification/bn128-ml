(* See https://github.com/ethereum/py_pairing/blob/master/py_ecc/optimized_bn128/optimized_pairing.py *)
open BN128Elements
open BN128Curve

let log_ate_loop_count = 63

let two = Z.of_int 2
let three = FQ.create (Z.of_int 3)

let ate_loop_count = Z.of_string "29793968203157093288"

let pseudo_binary_encoding = [0; 0; 0;  1;  0; 1; 0; -1; 0;  0;  1; -1;  0; 0; 1; 0; 
                              0; 1; 1;  0; -1; 0; 0;  1; 0; -1;  0;  0;  0; 0; 1; 1;
                              1; 0; 0; -1;  0; 0; 1;  0; 0;  0;  0;  0; -1; 0; 0; 1;
                              1; 0; 0; -1;  0; 0; 0;  1; 1;  0; -1;  0;  0; 1; 0; 1]

let pseudo_binary_encoding = List.rev pseudo_binary_encoding

let line_func p1 p2 t = match p1, p2, t with
| Infinite, _, _ | _, Infinite, _ | _, _, Infinite -> invalid_arg "line func must be finite"
| Finite (x1, y1), Finite (x2, y2), Finite (xt, yt) ->
  let numerator = FQP.sub y2  y1 in
  let denominator = FQP.sub x2 x1 in
  if x1 <> x2 then
    FQP.sub (FQP.mulp numerator (FQP.sub xt x1)) (FQP.mulp denominator (FQP.sub yt y1)),
    denominator
  else if y1 = y2 then
    let numerator = (FQP.mul (FQP.pow x1 two) three) in
    let denominator = (FQP.mul y1 (FQ.create two)) in
    FQP.sub (FQP.mulp numerator (FQP.sub xt x1)) (FQP.mulp denominator (FQP.sub yt y1)),
    denominator
  else FQP.sub xt x1, FQP.one_fq12

let final_exponent = Z.div (Z.sub (Z.pow field_modulus 12) Z.one) curve_order

let miller_loop q p exponentiate = match q, p with
| Infinite, _ | _, Infinite -> FQP.one_fq12
| Finite _, Finite _ ->
  let r = ref q in
  let fn = ref FQP.one_fq12 in
  let fd = ref FQP.one_fq12 in
  List.iter (fun v ->
    let n, d = line_func !r !r p in
    fn := FQP.mulp (FQP.mulp !fn !fn) n;
    fd := FQP.mulp (FQP.mulp !fd !fd) d;
    r := G12.double !r;
    if v = 1 then (
      let n, d = line_func !r q p in
      fn := FQP.mulp !fn n;
      fd := FQP.mulp !fd d;
      r := G12.add !r q
    ) else if v = -1 then (
      let nq = G12.neg q in
      let n, d = line_func !r nq p in
      fn := FQP.mulp !fn n;
      fd := FQP.mulp !fd d;
      r := G12.add !r nq
    )
  ) pseudo_binary_encoding;
  let q1x, q1y = (match q with
  | Infinite -> invalid_arg "must be finite"
  | Finite (qx, qy) -> (FQP.pow qx field_modulus, FQP.pow qy field_modulus))
  in
  let nq2 = Finite (FQP.pow q1x field_modulus, FQP.pow (FQP.neg q1y) field_modulus) in
  let q1 = Finite (q1x, q1y) in
  let n1, d1 = line_func !r q1 p in
  let r = G12.add !r q1 in
  let n2, d2 = line_func r nq2 p in
  let f = FQP.divp (FQP.mulp !fn (FQP.mulp n1 n2)) (FQP.mulp !fd (FQP.mulp d1 d2)) in
  if exponentiate then
    FQP.pow f final_exponent
  else f

let pairing ?exponentiate:(e=true) (q,p) = 
  miller_loop (cast_g2 q) (cast_g1 p) e
