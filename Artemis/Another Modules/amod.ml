module type Field = sig
  type t
  val zero : t                  (* zero element of the field *)
  val one : t                   (* unit element of the field *)
  val compare : t -> t -> int   (* comparison *)
  val to_string : t -> string   (* field element to string *)
  val add : t -> t -> t         (* addition *)
  val mul : t -> t -> t         (* multiplication *)
  val sub : t -> t -> t         (* subtraction *)
  val div : t -> t -> t         (* division *)
  val add_inv : t -> t          (* additive inverse *) 
  val mul_inv : t -> t          (* multiplicative inverse *)
end

module type RationalField =
  sig
    include Field with type t = int * int
    type t = int * int          (* rationals are represented as pairs of int *)
    exception Bad_rational of string
    val standard_form : t -> t  (* standard from of a rational number *)
    val to_float : t -> float   (* decimal expansion *)
    val from_int : int -> t     (* integer to rational conversion *)          
  end

module type GaussianRationalField =
  sig
    include Field with type t = (int * int) * (int * int)
    (* Gaussian rationals are represented as pairs of rationals *)
    exception Division_by_zero of string
    val from_rational : (int * int) -> t   (* rational to complex *)     
    val conj : t -> t                       (* conjugate *)
    val re : t -> (int * int)               (* real part *)
    val im : t -> (int * int)               (* imaginary part *)
  end
  
module Rationals : RationalField =
      struct
        type t = int * int
        exception Bad_rational of string
        let zero = (0,1)
        let one = (1,1)
        let standard_form (n,d) = 
             let rec gcd (n,d) = if d = 0 then n else 
                                                if d < 0 then gcd (-d, n mod d) else gcd (d, n mod d); in

             let g = if d < 0 then -(gcd (n, d)) else gcd (n,d) in
             if(d = 0) then raise (Bad_rational "Division by zero") else
                (n/g, d/g) 
                
        let compare (r1,i1) (r2,i2) = 
            let (a,b) = standard_form (r1,i1) in
            let (c,d) = standard_form (r2,i2) in
            if(a * d < b * c) then -1 else if(a * d > b * c) then 1 else 0
        
        let add (r1,i1) (r2,i2) =
            let (a,b) = standard_form (r1,i1) in
            let (c,d) = standard_form (r2,i2) in
            let (k,l) = (a * d + b * c, b * d) in
            standard_form (k,l)
        
        let mul (r1,i1) (r2,i2) =
            let (a,b) = standard_form (r1,i1) in
            let (c,d) = standard_form (r2,i2) in
            let (k,l) = (a * c, b * d) in
            standard_form (k,l)
            
        let sub (r1,i1) (r2,i2) =
            let (a,b) = standard_form (r1,i1) in
            let (c,d) = standard_form (r2,i2) in
            let (k,l) = (a * d - b * c, b * d) in
            standard_form (k,l)

        let div (r1,i1) (r2,i2) =
            let (a,b) = standard_form (r1,i1) in 
            let (c,d) = standard_form (r2,i2) in
            let (k,l) = (a * d, b * c) in
            standard_form (k,l)

        let add_inv (r1,i1) =
            let (a,b) = standard_form (r1,i1) in
            let (k,l) = (-a,b) in
            standard_form (k,l)

        let mul_inv (r1,i1) =
            let (a,b) = standard_form(r1,i1) in
            let (k,l) = (b,a) in 
            standard_form(k,l)      

        let to_float (r1,i1) = (float_of_int r1)/.(float_of_int i1)   
        let from_int a = if a > 0 then (a,1) else (-a,1)   


        let to_string (r,i) = let (r,i) = standard_form (r,i) 
                                in if r > 0 then 
                                "+"^(string_of_int r) ^"/"^ (string_of_int i) 
                                else (string_of_int r) ^"/"^ (string_of_int i)

      end;;

       (* TEST CASES*)
       (* Rationals.zero = (0,1);;
       Rationals.one = (1,1);;
       Rationals.standard_form (6, -15) = (-2,5);;
       Rationals.compare (3,-5) (5,3) = -1;;
       Rationals.add (2,4) (3,6) = (1,1);;
       Rationals.mul (2,4) (3,6) = (1,4);;
       Rationals.sub (-2,4) (7,3) = (-17,6);;
       Rationals.div (-2,4) (7,3) = (-3,14);;
       (try Some (Rationals.div (1,2) (0,5)) with Rationals.Bad_rational _ -> None) = None;;
       Rationals.mul (1,-3) Rationals.zero = Rationals.zero;;
       Rationals.to_float (6,8) = 0.75;;  *)

    module GaussianRationals : GaussianRationalField =
      struct
        type t = (int * int) * (int * int)
        exception Division_by_zero of string
        let zero = ((0, 1), (0, 1))
        let one = ((1, 1), (0, 1))
        let compare (r1,i1) (r2,i2) = 
            let a = Rationals.compare r1 r2 
            in 
            if(a != 0) then a else Rationals.compare i1 i2
        let to_string (r,i) = let a = Rationals.standard_form r 
            in let b = Rationals.standard_form i in
            if (a = Rationals.zero && b = Rationals.zero) then ""
            else if(b = Rationals.zero) then Rationals.to_string a
            else if(a = Rationals.zero) then (Rationals.to_string b) ^ "*I"
            else (Rationals.to_string a)^(Rationals.to_string b) ^ "*I"
        let from_rational r = (Rationals.standard_form r,Rationals.zero)
        let add (r1,i1) (r2,i2) = (Rationals.add r1 r2,Rationals.add i1 i2)
        let mul (a,b) (c,d) = try (Rationals.sub (Rationals.mul a c) (Rationals.mul b d),Rationals.add (Rationals.mul a d) (Rationals.mul b c)) 
                                with Rationals.Bad_rational "Error: Denominator should not be 0" -> 
                                raise (Division_by_zero "Division by ZERO")
        let sub (r1,i1) (r2,i2) = (Rationals.sub r1 r2,Rationals.sub i1 i2)
        let div (a,b) (c,d) = try (Rationals.div (Rationals.add (Rationals.mul a c) (Rationals.mul b d)) (Rationals.add (Rationals.mul c c) (Rationals.mul d d)),
                                Rationals.div (Rationals.add (Rationals.mul b c) (Rationals.mul a d)) (Rationals.add (Rationals.mul c c) (Rationals.mul d d))) 
                                with Rationals.Bad_rational "Error: Denominator should not be 0" -> 
                                raise (Division_by_zero "Division by ZERO")
        let add_inv (r,i) = try (Rationals.add_inv r,Rationals.add_inv i) 
                            with Rationals.Bad_rational "Error: Denominator should not be 0" -> 
                            raise (Division_by_zero "Division by ZERO")
        
        let mul_inv (r,i) = try ((Rationals.div r (Rationals.add (Rationals.mul r r) (Rationals.mul i i))),(Rationals.div (Rationals.add_inv i) (Rationals.add (Rationals.mul r r) (Rationals.mul i i)))) 
                            with Rationals.Bad_rational "Error: denominator is 0" -> 
                            raise (Division_by_zero "Division by ZERO")
        let conj (r,i) = try (Rationals.standard_form r,Rationals.add_inv i) 
                         with Rationals.Bad_rational "Error: Denominator should not be 0" ->
                         raise (Division_by_zero "Division by ZERO")
        let re (r,i) = try Rationals.standard_form r 
                         with Rationals.Bad_rational "Error: Denominator should not be 0" ->
                         raise (Division_by_zero "Division by ZERO")
        let im (r,i) = try Rationals.standard_form i 
                        with Rationals.Bad_rational "Error: Denominator should not be 0" -> 
                        raise (Division_by_zero "Division by ZERO")
        
       end;;

    (* TEST CASES *)
       (* GaussianRationals.zero = ((0, 1), (0, 1));;
       GaussianRationals.one = ((1, 1), (0, 1));;
       GaussianRationals.mul((2,4),(7,-2)) ((3,6),(7,2)) = ((25, 2), (0, 1));;
       GaussianRationals.add_inv ((6,15), (7,21)) = ((-2,5),(-1,3));;
       GaussianRationals.mul_inv ((8,16),(3,7)) = ((98, 85), (-84, 85));;
       GaussianRationals.to_string ((2,-4),(4,-5)) = "-1/2-4/5*I";;
       GaussianRationals.re ((6, 9), (0, 1)) = (2, 3);;
       GaussianRationals.im ((6, 9), (2, 4)) = (1, 2);;
       GaussianRationals.conj ((2, 3), (0, 1)) = ((2, 3), (0, 1));; *)
;;