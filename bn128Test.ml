open BN128Elements
open BN128Curve
open BN128Pairing

let z i = Z.of_int i
let fq i = FQ.create (z i)
let fq2 i j = FQP.create_fq2 [|z i;z j|]
let fq12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 = FQP.create_fq12 [|z x1; z x2; z x3; z x4; z x5; z x6; z x7; z x8; z x9; z x10; z x11; z x12|]

let () =
  assert (Z.powm (z 2) field_modulus field_modulus = z 2);
  assert (Z.powm (z 2) curve_order curve_order = z 2);
  assert (Z.erem (Z.sub (Z.pow field_modulus 12) (z 1)) curve_order = z 0);
  assert (G1.is_on_curve G1.g);
  assert (G2.is_on_curve G2.g);
  assert (G12.is_on_curve G12.g)

let () = 
  assert (FQ.mul (fq 2) (fq 2) = fq 4);
  assert (FQ.add (FQ.div (fq 2) (fq 7)) (FQ.div (fq 9) (fq 7)) = FQ.div (fq 11) (fq 7));
  assert (FQ.add (FQ.mul (fq 2) (fq 7)) (FQ.mul (fq 9) (fq 7)) = FQ.mul (fq 11) (fq 7));
  assert (FQ.pow (fq 9) field_modulus = fq 9)

let x = fq2 1 0
let f = fq2 1 2
let fpx = fq2 2 2
let one = FQP.one [|Z.one; Z.zero|]

let () =
  assert (FQP.add x f = fpx);
  assert (FQP.divp f f = one);
  assert (FQP.add (FQP.divp one f) (FQP.divp x f) = FQP.divp (FQP.add one x) f);
  assert (FQP.add (FQP.mulp one f) (FQP.mulp x f) = FQP.mulp (FQP.add one x) f);
  assert (FQP.pow x (Z.sub (Z.mul field_modulus field_modulus) Z.one) = one)

let x = fq12 1 0 0 0 0 0 0 0 0 0 0 0
let f = fq12 1 2 3 4 5 6 7 8 9 10 11 12
let fpx = fq12 2 2 3 4 5 6 7 8 9 10 11 12
let one = FQP.one_fq12

let () =
  assert (FQP.add x f = fpx);
  assert (FQP.divp f f = one); 
  assert (FQP.add (FQP.divp one f) (FQP.divp x f) = FQP.divp (FQP.add one x) f);
  assert (FQP.add (FQP.mulp one f) (FQP.mulp x f) = FQP.mulp (FQP.add one x) f)

let () =
  assert (G1.add (G1.add (G1.double G1.g) G1.g) G1.g = G1.double (G1.double G1.g));
  assert (G1.double G1.g <> G1.g);
  assert (G1.add (G1.mul G1.g (z 9)) (G1.mul G1.g (z 5)) = G1.add (G1.mul G1.g (z 12)) (G1.mul G1.g (z 2)));
  assert (G1.mul G1.g curve_order = Infinite)

let () =
  assert (G2.add (G2.add (G2.double G2.g) G2.g) G2.g = G2.double (G2.double G2.g));
  assert (G2.double G2.g <> G2.g);
  assert (G2.add (G2.mul G2.g (z 9)) (G2.mul G2.g (z 5)) = G2.add (G2.mul G2.g (z 12)) (G2.mul G2.g (z 2)));
  assert (G2.mul G2.g curve_order = Infinite);
  assert (G2.mul G2.g (Z.sub (Z.mul (z 2) field_modulus) curve_order) <> Infinite);
  assert (G2.is_on_curve (G2.mul G2.g (z 9)))

let () =
  assert (G12.add (G12.add (G12.double G12.g) G12.g) G12.g = G12.double (G12.double G12.g));
  assert (G12.double G12.g <> G12.g);
  assert (G12.add (G12.mul G12.g (z 9)) (G12.mul G12.g (z 5)) = G12.add (G12.mul G12.g (z 12)) (G12.mul G12.g (z 2)));
  assert (G12.is_on_curve (G12.mul G12.g (z 9)));
  assert (G12.mul G12.g curve_order = Infinite)

let p1 = pairing (G2.g, G1.g)
let pn1 = pairing (G2.g, G1.neg G1.g)
let np1 = pairing (G2.neg G2.g, G1.g)
let p2 = pairing (G2.g, G1.mul G1.g (z 2))
let po2 = pairing (G2.mul G2.g (z 2), G1.g)
let p3 = pairing (G2.mul G2.g (z 27), G1.mul G1.g (z 37))
let po3 = pairing (G2.g, G1.mul G1.g (z 999))

let () =
  assert (FQP.mulp p1 pn1 = one);
  assert (FQP.mulp p1 np1 = one);
  assert (pn1 = np1);
  assert (FQP.pow p1 curve_order = one);
  assert (FQP.mulp p1 p1 = p2);
  assert (p1 <> p2);
  assert (p1 <> np1);
  assert (p2 <> np1);
  assert (FQP.mulp p1 p1 = po2);
  assert (p3 = po3)
