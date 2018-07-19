open BN128Curve

val pairing : ?exponentiate:bool -> G2.t point * G1.t point -> G12.t

val final_exponent : Z.t
