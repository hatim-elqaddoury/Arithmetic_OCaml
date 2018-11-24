
(*************HEADERS***********)


(* 
  we choose the base 1000000000, in the multiplication 
  we have : ((10 pow 9) pow 2) < (10 pow 18) max_int.
  that matches the system 64 bits.  
*) 

let base = 10;;  

type big_int = int array;;

type big_signed_int = {s: int; n: big_int};; 

let zero = ([|0|]: big_int) 
and one = ([|1|]: big_int);;

let zero_signed = ({s=1; n=[|0|]}:big_signed_int) 
and one_signed = ({s=1; n=[|1|]}:big_signed_int);;

(* constructor int -> big_int*)
let rec to_big_int n=
  assert (0 <= n);
  if (n/base)>0 then 
    Array.append (to_big_int (n/base)) [|n mod base|] 
  else  
   ([|n|]: big_int);;

(* constructor int -> big_signed_int*)
let rec to_big_signed_int x=
  if x<0 then 
    let res : big_signed_int = {s= -1; n=to_big_int (x *(-1))} in res 
  else  
    let res : big_signed_int = {s= 1; n=to_big_int x} in res ;;



(***************** NEEDED *******************)

let failwith msg = raise (Failure msg);;

let show (x: string)  = Printf.printf "%s" x;;


let sub_arr (a: big_int) s l = 
  let i = ref s and n = ref l in
  while a.(!i)=0 && !n > 1 do
    incr i; decr n;
  done;
  (Array.sub a !i !n;: big_int);; 

(*
 compare two big integers a and b :
 0: equals,  -1: b bigger , 1: a bigger 
*)
let comparaison (a: big_int) (b: big_int) =
  if Array.length a < Array.length b then -1
  else if Array.length a > Array.length b then 1
  else
    let i = ref 0 in
    begin
      while !i < Array.length a && a.(!i) = b.(!i) do
        incr i;
      done;
      if !i = Array.length a then 0
      else if a.(!i) > b.(!i) then 1
      else -1
    end;;

(***************** ADDITION *******************)

let rec addition (a: big_int) (b: big_int) =
  if Array.length a < Array.length b then addition b a 
  else begin 
  let result = ref a
  and remainder  = ref 0
  and i = ref (Array.length a - 1)
  and j = ref (Array.length b - 1)
  in begin
  while !j >= 0 do
    let d = a.(!i) + b.(!j) + !remainder
    in if d < base then begin
      remainder:= 0; a.(!i) <- d 
    end else begin
      remainder:= 1; a.(!i) <- d - base
    end;
    decr i; decr j; 
  done;
  while !remainder > 0 do
    if !i >= 0 then begin
      let d = a.(!i) + !remainder
      in if d < base then begin
        remainder:= 0; a.(!i) <- d 
      end else begin
        a.(!i) <- d - base
      end;
      decr i;
    end else begin
      result:= Array.make (Array.length a + 1) 0;
      Array.blit a 0 !result 1 (Array.length a);
      !result.(0) <- 1; remainder:= 0;
    end;
  done;
   !result
  end
  end;;

let add (a: big_int) (b: big_int) = addition a b;;

(*just for fast test, there is no big numbers here because the integer limited in 10 pow 18 cannot become a big integer with a large  number of shifts.*)
let addd a b =   let res = (addition (to_big_int a) (to_big_int b)) in res ;;

let adddd (a:string) (b:string) = 
  let res = (addition (to_big_int (int_of_string a)) (to_big_int (int_of_string b))) 
  in res ;;


(*************** SOUSTRACTION *****************)

let difference (a: big_int) (b: big_int) =
   if a<b then 
    failwith "swap the values between a and b." 
   else 
   let result = ref a
   and remainder  = ref 0
   and i = ref (Array.length a - 1)
   and j = ref (Array.length b - 1)
   in begin
     while !j >= 0 do
       let d = a.(!i) - b.(!j) - !remainder
       in if d >= 0 then begin
         remainder:= 0; a.(!i) <- d 
       end else begin
         remainder:= 1; a.(!i) <- d + base
       end;
       decr i; decr j; 
     done;
     while !remainder > 0 do
       let d = a.(!i) - !remainder
       in if d >= 0 then begin
         remainder:= 0; a.(!i) <- d 
       end else begin
         a.(!i) <- d + base
       end;
       decr i;
     done;
     if !i < 0 then begin
       i:= 0; j:= Array.length a - 1;
       while a.(!i) = 0 && !i < !j  do incr i; done;
       if !i >= 0 then result:= Array.sub a !i (Array.length a - !i);
     end;   
     !result
   end;;

let diff (a: big_int) (b: big_int) = 
  if a < b then     
  let res = zero in res 
  else
  let res = difference a b in res ;;

let difff a b = 
  if a < b then      
   let res = zero in res 
  else
  difference (to_big_int a) (to_big_int b) ;; 




(*************** MULTUPLICATION  *****************)

let shift_n_digit (a: big_int) n = (* move by n zeros to the left.  *)
  assert (n >= 0);
  if a = zero then zero
  else
    let res: big_int = Array.make (Array.length a + n) 0
    in begin
      Array.blit a 0 res 0 (Array.length a);
      res
    end;;

let small_mult (a: big_int) n =  (* scaling up the number part by part. *)
  assert (0 <= n && n < base);
  if n = 0 then zero
  else 
    let accu  = ref 0 
    and remainder = ref 0
    and res: big_int = Array.make (Array.length a + 1) 0
    in begin
      for i = (Array.length a) downto 1 do
        accu:= a.(i-1) * n + !remainder; 
        res.(i) <- !accu mod base; remainder:= !accu/base
      done;
      res.(0) <- !remainder;
      if !remainder = 0 then
        (Array.sub res 1 (Array.length a): big_int)
      else
        res  
    end;;

let long_mult (a: big_int) (b: big_int) =
  let i = ref 0
  and j = ref (Array.length b-1) in
  let result = ref (shift_n_digit (small_mult a b.(!i)) !j) 
  in begin
    while !j > 0 do
      incr i; decr j;
      result:= addition !result (shift_n_digit (small_mult a b.(!i)) !j)
    done;  
    !result
  end;;

let rec multiplication (a: big_int) (b: big_int) =
  if Array.length a < Array.length b then
    multiplication b a
  else if Array.length b < 20 then
    long_mult a b
  else 
    karatsuba a b
  and karatsuba (p: big_int) (q: big_int) =
  assert (Array.length p >= Array.length q);
  let len_p = Array.length p  in
  let len_q = Array.length q  in
  let     n = len_p / 2       in
  let     a = sub_arr p 0 (len_p - n)  in
  let     b = sub_arr p (len_p - n) n  in
  if len_q > n then  
    let      c = sub_arr q 0 (len_q - n)  in
    let      d = sub_arr q (len_q - n) n  in
    let     ac = multiplication a c  in
    let     bd = multiplication b d  in
    let  ad_bc = difference (difference (multiplication (add a b) (add c d)) ac) bd
    in
    addition (addition (shift_n_digit ac (2*n)) (shift_n_digit ad_bc n)) bd
  else  
    let     aq = multiplication a q in
    let     bq = multiplication b q in
    addition (shift_n_digit aq n) bq;;

let prd (a: big_int) (b: big_int) =  multiplication a b ;;

let prdd a b = 
  if a < b then      
  multiplication (to_big_int b) (to_big_int a)
  else
  multiplication (to_big_int a) (to_big_int b) ;; 


(*************** DIVISION *****************)

(*
  method Russes for dividing a by b: 
  a / b = ( 2 pow k ) +  ((2 pow k)*b)/b  ;;
  so we need to implement the integer power function 1st. 
*)
let pow b x =
  if x < 0 then invalid_arg "exponent cant be negative" else
  let rec aux accumulator b = function
    | 0 -> accumulator
    | 1 -> b * accumulator
    | e when (e mod 2) ==0 -> aux accumulator (b * b) (e / 2)
    | e -> aux (b * accumulator) (b * b) ((e - 1) / 2) in
  aux 1 b x ;;

let rec division (a: big_int) (b: big_int) =
  if (comparaison b zero)==0  then
    failwith "dividing by 0." 
  else
  let k = 0 in               (*Array.length (diff a one)) in*)      (* 2'k x b < a          2'k < a <2'k+1*)
  if (prd (to_big_int (pow 2 k )) b) > a then
    let res: big_int = zero in res
  else if (Array.length a > 20) then 
    let res: big_int = (add (to_big_int(pow 2 k )) (division ( diff a (prd (to_big_int (pow 2 k )) b) ) b)) in  res
  else
    let res: big_int = (add (to_big_int(pow 2 k )) (division ( diff a (prd (to_big_int (pow 2 k )) b) ) b)) in res;;

let div (a: big_int) (b: big_int) =  division a b ;;

let divv a b = 
  if (0 >= b )then
    failwith "dividing by 0." 
  else if a < b then  
    begin
        failwith "a<b."
    end
  else   
  let res = division (to_big_int a) (to_big_int b) in res ;;


(*************** FACTORIAL *****************)

let rec factorial (x: big_int) =
  let one = ([|1|]: big_int) in
  if (comparaison x one)<=0 then one
  else prd (add x one) ( factorial (diff x one)) ;;

let fact x = factorial (to_big_int x) ;;

(*************** MODULO *****************)

let modulo (a: big_int) (b: big_int) = 
  if (comparaison b a == 1) then 
    let res = zero in res
  else if (comparaison a b )== -1 then 
    let res = one in res 
  else  
    let d = (division a b) in 
    let l = (prd b d) 
    and k =  add a b in
    let res = diff k l 
    in res;;

let modu a b = let res= modulo (to_big_int a) (to_big_int b) in res  ;;


(*************** PGCD *****************)

let rec int_pgcd(a,b)=
  if a >= b then
    if b = 0 then a
    else
      int_pgcd(b, a mod b)
  else
    int_pgcd(b,a);;

let rec pgcd (a: big_int) (b: big_int)=
  if (comparaison a b )==1 then
    if (comparaison b zero)==0 then a
    else
      pgcd b (modulo a b)
  else 
    pgcd b a;;

let _pgcd (a, b) = pgcd (to_big_int a) (to_big_int b) ;;



(*************** SIGNED OPERATIONS *****************)


(* signed addition *)
let add_sign (a: big_signed_int) (b: big_signed_int) = 
  if  (a.s == -1 &&  b.s == -1)  then 
    let res : big_signed_int = { s= -1 ; n=add a.n b.n} in res 
  else  if (a.s == 1 && b.s == 1) then 
      let res : big_signed_int = { s= 1 ; n=add a.n b.n} in res
  else  if (a.s == -1 && b.s == 1) then 
          if (a.n >= b.n ) then 
              let res : big_signed_int = { s= -1 ; n=diff a.n b.n} in res
           else 
              let res : big_signed_int = { s= 1 ; n=diff b.n a.n} in res
  else   
          if (a.n >= b.n ) then 
              let res : big_signed_int = { s= 1 ; n=diff a.n b.n} in res
           else 
              let res : big_signed_int = { s= -1 ; n=diff b.n a.n} in res ;;

let add_signed (a, b) = add_sign (to_big_signed_int a) (to_big_signed_int b) ;;

(* signed subtraction *)
let diff_sign (a: big_signed_int) (b: big_signed_int) =
  if  (a.s == -1 &&  b.s == -1)  then 
          if (a.n >= b.n ) then 
              let res : big_signed_int = { s= -1 ; n=diff a.n b.n} in res
           else 
              let res : big_signed_int = { s= 1 ; n=diff a.n b.n} in res
  else  if (a.s == 1 && b.s == 1) then 
          if (a.n >= b.n ) then 
              let res : big_signed_int = { s=  1 ; n=diff a.n b.n} in res
           else 
              let res : big_signed_int = { s= -1 ; n=diff b.n a.n} in res
  else  if (a.s == -1 && b.s == 1) then 
              let res : big_signed_int = { s= -1 ; n=add a.n b.n} in res
  else   
              let res : big_signed_int = { s= 1 ; n=add a.n b.n} in res ;; 

let diff_signed (a, b) = diff_sign (to_big_signed_int a) (to_big_signed_int b) ;;

(* signed multiplication *)
let prd_sign (a: big_signed_int) (b: big_signed_int) =
  if (a.s == 1 &&  b.s == 1) || (a.s == -1 &&  b.s == -1) then
    let res : big_signed_int = { s= 1 ; n=prd a.n b.n} in res
  else 
    let res : big_signed_int = { s= -1 ; n=prd a.n b.n} in res ;;

let prd_signed (a, b) = prd_sign (to_big_signed_int a) (to_big_signed_int b) ;;

(* signed division *)
let div_sign (a: big_signed_int) (b: big_signed_int) =
  if (a.s == 1 &&  b.s == 1) || (a.s == -1 &&  b.s == -1) then
    let res : big_signed_int = { s= 1; n=div a.n b.n} in res
  else 
      if (comparaison (div a.n b.n) zero)==0 then 
        let res : big_signed_int = { s= 1; n=div a.n b.n} in res 
      else 
        let res : big_signed_int = { s= -1; n=div a.n b.n} in res ;;

let div_signed (a, b) = div_sign (to_big_signed_int a) (to_big_signed_int b) ;;

