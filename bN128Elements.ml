let field_modulus = Z.of_string "21888242871839275222246405745257275088696311157297823662689037894645226208583"

let inv a n =
  if a = Z.zero then a else
  let lm, hm = ref Z.one, ref Z.zero in
  let low, high = ref (Z.erem a n), ref n in
  while Z.gt !low Z.one do
    let r = Z.div !high !low in
    let nm, _new = Z.sub !hm (Z.mul !lm r), Z.sub !high (Z.mul !low r) in
    hm := !lm; high := !low; lm := nm; low := _new
  done;
  Z.erem !lm n

let two = Z.of_int 2

module FQ = struct
  type t = Z.t

  let create z = Z.erem z field_modulus

  let add z1 z2 = Z.erem (Z.add z1 z2) field_modulus

  let mul z1 z2 = Z.erem (Z.mul z1 z2) field_modulus

  let sub z1 z2 = Z.erem (Z.sub z1 z2) field_modulus

  let div z1 z2 = Z.erem (Z.mul z1 (inv z2 field_modulus)) field_modulus

  let one = Z.one

  let rec pow z1 z2 =
    if z2 = Z.zero then one else
    if z2 = Z.one then z1 else
    if (Z.erem z2 two) = Z.zero then pow (mul z1 z1) (Z.div z2 two) else
    mul (pow (mul z1 z1) (Z.div z2 two)) z1

  let neg z = Z.erem (Z.neg z) field_modulus

  let zero = Z.zero

  let to_z z = z

end

let deg p =
  let d = ref ((Array.length p) - 1) in
  while ((Array.get p !d) = Z.zero) && !d <> 0 do
   d := !d - 1
  done;
  !d

let poly_rounded_div a b =
  let dega = deg a in
  let degb = deg b in
  let temp = Array.copy a in
  let o = Array.make (Array.length a) Z.zero in
  for i = dega - degb downto 0 do
    Array.set o i (Z.add (Array.get o i) (Z.mul (Array.get temp (degb + i)) (inv (Array.get b degb) field_modulus)));
    for c = 0 to degb do
      Array.set temp (c + i) (Z.sub (Array.get temp (c + i)) (Array.get o c))
    done
  done;
  Array.init ((deg o) + 1) (fun i -> Z.erem (Array.get o i) field_modulus)

module FQP = struct
  type t = (FQ.t array * Z.t array * int)

  let coeffs p = match p with (coeffs, _, _) -> coeffs
  let mul_coeffs (p: t) = match p with (_, mul_coeffs, _) -> mul_coeffs
  let degree p = match p with (_, _, degree) -> degree

  let combine (p1: t) (p2: t) fq3 : t = match p1, p2 with
  (_, m1, d1), (_, m2, d2) when m1 = m2 && d1 = d2 -> (fq3, m1, d1)
  | _ -> failwith "unequal degree"

  let rec create coeffs mod_coeffs = 
    (Array.map FQ.create coeffs, mod_coeffs, Array.length mod_coeffs)

  let add p1 p2 = combine p1 p2 (Array.map2 FQ.add (coeffs p1) (coeffs p2))

  let sub p1 p2 = combine p1 p2 (Array.map2 FQ.sub (coeffs p1) (coeffs p2))

  let mul p fq = match p with
  (fqs, m, d) -> (Array.map (fun c -> FQ.mul c fq) fqs, m, d)

  let mulp' p1 p2 d mod_coeffs = 
    let b = Array.make (d * 2 - 1) Z.zero in
    for i = 0 to d - 1 do
      for j = 0 to d - 1 do
        Array.set b (i + j) (Z.add (Array.get b (i + j)) (Z.mul (Array.get p1 i) (Array.get p2 j)))
      done
    done;
    let b = ref b in
    while Array.length !b > d do
      let exp, top = Array.length !b - d - 1, Array.get !b ((Array.length !b) - 1) in
      b := Array.init ((Array.length !b) - 1) (Array.get !b);
      for i = 0 to d - 1 do
        Array.set !b (exp + i) (Z.sub (Array.get !b (exp + i)) (Z.mul top (Array.get mod_coeffs i)))
      done
    done;
    Array.map FQ.create !b

  let mulp p1 p2 = combine p1 p2 (mulp' (coeffs p1) (coeffs p2) (degree p1) (mul_coeffs p1))

  let div p fq = match p with
  (fqs, m, d) -> (Array.map (fun c -> FQ.div c fq) fqs, m, d)

  let zero mod_coeffs = (Array.make (Array.length mod_coeffs) FQ.zero, mod_coeffs, Array.length mod_coeffs)

  let one mod_coeffs : t = 
    let z = zero mod_coeffs in match z with (z, m, d) ->
    Array.set z 0 FQ.one;
    (z, m, d)

  let rec pow p z : t =
  match p with (_, mod_coeffs, _) ->
  if z = Z.zero then one mod_coeffs else
  if z = Z.one then p else
  if (Z.logand z Z.one) = Z.zero then pow (mulp p p) (Z.shift_right z 1) else
  mulp (pow (mulp p p) (Z.shift_right z 1)) p

  let neg p = match p with
  (fqs, m, d) -> (Array.map FQ.neg fqs, m, d)


  let create_fq2 coeffs = create coeffs [|Z.one; Z.zero|]

  let eighty_two = Z.of_int 82
  let neg_eighteen = Z.of_int (-18)

  let fq12_mod_coeffs = [|eighty_two; Z.zero; Z.zero; Z.zero; Z.zero; Z.zero; neg_eighteen; Z.zero; Z.zero; Z.zero; Z.zero; Z.zero|]

  let create_fq12 coeffs = create coeffs fq12_mod_coeffs

  let one_fq12 = one fq12_mod_coeffs

  let inv p = match p with
    (coeffs, mod_coeffs, d) ->
    let lm, hm = ref (Array.make (d + 1) Z.zero), ref (Array.make (d + 1) Z.zero) in
    Array.set !lm 0 Z.one;
    let low, high = ref (Array.make (d + 1) FQ.zero), ref (Array.make (d + 1) FQ.one) in
    Array.blit coeffs 0 !low 0 d;
    Array.blit mod_coeffs 0 !high 0 d;
    while (deg !low) > 0 do
      let r = poly_rounded_div !high !low in
      let newr = Array.make (d + 1) Z.zero in
      Array.blit r 0 newr 0 (Array.length r);
      let nm = Array.copy !hm in
      let _new = Array.copy !high in
      for i = 0 to d do
        for j = 0 to d - i do
          Array.set nm (i + j) (Z.sub (Array.get nm (i + j)) (Z.mul (Array.get !lm i) (Array.get newr j)));
          Array.set _new (i + j) (FQ.sub (Array.get _new (i + j)) (FQ.mul (Array.get !low i) (Array.get newr j)))
        done
      done;
      hm := !lm;
      high := !low;
      lm := nm;
      low := _new
    done;
    let output = Array.init d (Array.get !lm) in
    div (output,mod_coeffs, d) (Array.get !low 0)

  let divp p1 p2 = mulp p1 (inv p2)

end
