open BN128Elements
open BN128Curve

let log_ate_loop_count = 63

let two = Z.of_int 2
let three = FQ.create (Z.of_int 3)

let ate_loop_count = Z.of_string "29793968203157093288"

let line_func p1 p2 t = match p1, p2, t with
| Infinite, _, _ | _, Infinite, _ | _, _, Infinite -> invalid_arg "line func must be finite"
| Finite (x1, y1), Finite (x2, y2), Finite (xt, yt) ->
  if x1 <> x2 then 
    let m = FQP.divp (FQP.sub y2 y1) (FQP.sub x2 x1) in
    FQP.sub (FQP.mulp m (FQP.sub xt x1)) (FQP.sub yt y1)
  else if y1 = y2 then
    let m = FQP.divp (FQP.mul (FQP.pow x1 two) three) (FQP.mul y1 (FQ.create two)) in
    FQP.sub (FQP.mulp m (FQP.sub xt x1)) (FQP.sub yt y1)
  else FQP.sub xt x1

let miller_loop q p = match q, p with
| Infinite, _ | _, Infinite -> FQP.one_fq12
| Finite _, Finite _ ->
  let r = ref q in
  let f = ref FQP.one_fq12 in
  for i = log_ate_loop_count downto 0 do
    f := FQP.mulp (FQP.mulp !f !f) (line_func !r !r p);
    r := G12.double !r;
    if not (Z.equal (Z.logand ate_loop_count (Z.pow two i)) Z.zero) then (
      f := FQP.mulp !f (line_func !r q p);
      r := G12.add !r q
    )
  done;
  let q1x, q1y = (match q with
  | Infinite -> invalid_arg "must be finite"
  | Finite (qx, qy) -> (FQP.pow qx field_modulus, FQP.pow qy field_modulus))
  in
  let nq2 = Finite (FQP.pow q1x field_modulus, FQP.pow (FQP.neg q1y) field_modulus) in
  let q1 = Finite (q1x, q1y) in
  let f = FQP.mulp !f (line_func !r q1 p) in
  let r = G12.add !r q1 in
  let f = FQP.mulp f (line_func r nq2 p) in
  FQP.pow f (Z.div (Z.sub (Z.pow field_modulus 12) Z.one) curve_order)

let pairing (q, p) = 
  miller_loop (cast_g2 q) (cast_g1 p)
