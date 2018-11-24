#use "bigint.ml" ;;

type big_rat = {n: big_int; d: big_int};;
type big_signed_rat = {n: big_signed_int; d: big_signed_int};;


let print_big_int (x:big_int) =
  Format.printf "\n\nResult :  " ;
  Array.iter (Format.printf "%d" ) x ;
  Format.printf "\n\n\n" ;;
#install_printer print_big_int;;

let print_big_signed_int (x:big_signed_int) =
  Format.printf "\n\nResult :  " ;
  if(x.s== -1) then begin
  if (comparaison x.n zero)==0 then begin 
      Array.iter (Format.printf "%d" ) x.n ;
      Format.printf "\n\n\n" ;
  end else begin  
      Format.printf "-";
      Array.iter (Format.printf "%d" ) x.n ;
      Format.printf "\n\n\n" ;
  end
  end else begin
    Array.iter (Format.printf "%d" ) x.n ;
    Format.printf "\n\n\n" ;
  end;;
#install_printer print_big_signed_int;;

let print_big_rat (a: big_rat)=
  Format.printf "\n\nResult :  " ;
  if (comparaison a.d zero)<>0 then begin 
    Array.iter (Format.printf "%d" ) a.n ;
    Format.printf " / ";
    Array.iter (Format.printf "%d" ) a.d ;
    Format.printf "\n\n\n" end
  else 
    Array.iter (Format.printf "%d" ) a.n ;
    Format.printf "\n\n\n" ;;
#install_printer print_big_rat;;

let print_big_signed_rat (a: big_signed_rat)=
  Format.printf "\n\nResult :  " ;
  if (comparaison a.d.n zero)<>0 then       
      if  a.n.s== a.d.s then begin
        Array.iter (Format.printf "%d" ) a.n.n ;
        Format.printf " / ";
        Array.iter (Format.printf "%d" ) a.d.n ;
        Format.printf "\n\n\n" ;
      end else begin
          Format.printf "- (";
        Array.iter (Format.printf "%d" ) a.n.n ;
        Format.printf " / ";
        Array.iter (Format.printf "%d" ) a.d.n ;
        Format.printf ")\n\n\n" 
      end 
  else 
      if  a.n.s== a.d.s then begin
        Array.iter (Format.printf "%d" ) a.n.n ;
        Format.printf "\n\n\n" ;
      end else begin
        Array.iter (Format.printf "%d" ) a.n.n ;
        Format.printf "\n\n\n" 
      end ;;
#install_printer print_big_signed_rat;;



(***************** ADDITION *******************)

let addition_rat (a:big_rat) (b:big_rat) =
	assert ( (comparaison a.d zero)>0 && (comparaison b.d zero)>0 );
	
	if (comparaison a.n zero) == 0 && (comparaison b.n zero) == 0 then 
		let res : big_rat = {  n= zero; d= zero }  in res
	else if (comparaison a.n zero) == 0 then 
		let res : big_rat = {  n= b.n; d= b.d }  in res
    else if (comparaison b.n zero) == 0 then 
    	let res : big_rat = {  n= a.n; d= a.d }  in res
	else
		let x  = ( add ( prd a.n b.d) (prd a.d b.n) ) 
		and xx = ( add ( prd a.n b.d) (prd a.d b.n) ) 
		and y  = ( prd a.d b.d)
		and yy = ( prd a.d b.d)  in
		let res : big_rat = {  n= div xx (pgcd x y); 
						       d= div yy (pgcd x y)
						    }  in res ;;

let add_rat (a,b) (c,d) =
  if (b<=0 || d<=0)  then 
 	failwith "dividing by zero." 
  else 
  	addition_rat {n=(to_big_int a); d=(to_big_int b)} {n=(to_big_int c); d=(to_big_int d)} ;;  


(*************** SOUSTRACTION *****************)

let difference_rat (a:big_rat) (b:big_rat) =
	assert ( (comparaison a.d zero)>0 && (comparaison b.d zero)>0 );
	if (comparaison a.n zero) == 0 && (comparaison b.n zero) == 0 then 
		let res : big_rat = {  n= zero; d= zero }  in res
	else if (comparaison a.n zero) == 0 then 
		let res : big_rat = {  n= b.n; d= b.d }  in res
    else if (comparaison b.n zero) == 0 then 
    	let res : big_rat = {  n= a.n; d= a.d }  in res
	else
		let x  = ( diff ( prd a.n b.d) (prd a.d b.n) )
		and xx = ( diff ( prd a.n b.d) (prd a.d b.n) )
		and y  = ( prd a.d b.d)
		and yy = ( prd a.d b.d) in
		let res : big_rat = {  n= div xx (pgcd x y); 
						       d= div yy (pgcd x y)
						    }  in res ;;

let diff_rat (a,b) (c,d) =
  if (b<=0 || d<=0)  then 
 	failwith "dividing by zero." 
  else 
  	difference_rat {n=(to_big_int a); d=(to_big_int b)} {n=(to_big_int c); d=(to_big_int d)} ;;  


(*************** MULTIPLICATION *****************)

let multiplication_rat  (a:big_rat) (b:big_rat) =
	assert ( (comparaison a.d zero)>0 && (comparaison b.d zero)>0 );
	let x  = (prd a.n b.n)
	and xx = (prd a.n b.n); 
	and y  = (prd a.d b.d) 
	and yy = (prd a.d b.d)  in
	let res : big_rat = { n= div xx (modulo x y);  
						  d= div yy (modulo x y)
					    } in res ;;

let mult_rat (a,b) (c,d) =
  if (b<=0 || d<=0)  then 
 	failwith "dividing by zero." 
  else 
  	multiplication_rat {n=(to_big_int a); d=(to_big_int b)} {n=(to_big_int c); d=(to_big_int d)} ;;  


(*************** DIVISION *****************)

let division_rat (a:big_rat) (b:big_rat) =
	assert ( (comparaison a.d zero)>0 && (comparaison b.d zero)>0 );
	let x  = (prd a.n b.d)
	and xx = (prd a.n b.d)
	and y  = (prd a.d b.n)
	and yy = (prd a.d b.n)  in
	let res : big_rat = { n=div xx (pgcd x y); 
					 	  d=div yy (pgcd x y)
						}in res ;;

let div_rat (a,b) (c,d) =
  if (b<=0 || d<=0)  then 
 	failwith "dividing by zero." 
  else 
  	division_rat {n=(to_big_int a); d=(to_big_int b)} {n=(to_big_int c); d=(to_big_int d)} ;;  



(*************** SIGNED OPERATIONS *****************)

(* signed addition *) 
	  	
let add_rat_sign (a: big_signed_rat) (b: big_signed_rat) =
	assert ( comparaison (a.d.n) zero <> 0 && comparaison (b.d.n) zero <> 0 );
	if (comparaison a.n.n zero) == 0 then 
	let res : big_signed_rat = {  n= b.n; d= b.d }  in res
    else if (comparaison b.n.n zero) == 0 then 
    let res : big_signed_rat = {  n= a.n; d= a.d }  in res
	else if (comparaison a.n.n zero) == 0 && (comparaison b.n.n zero) == 0
	then let res : big_signed_rat = {  n= zero_signed; d= zero_signed }  in res
	else
	let x  = ( add_sign ( prd_sign a.n b.d) (prd_sign a.d b.n) ) 
	and xx = ( add_sign ( prd_sign a.n b.d) (prd_sign a.d b.n) ) 
	and y  = ( prd_sign a.d b.d)
	and yy = ( prd_sign a.d b.d)  in
	let res : big_signed_rat = {  n= div_sign xx {s=1;n=(pgcd x.n y.n)}; 
							      d= div_sign yy {s=1;n=(pgcd x.n y.n)}
							   }  in res ;;

let add_rat_signed (a,b) (c,d) = 
	if (b==0 || d==0)  then 
 		failwith "dividing by zero." 
 	 else 
  		add_rat_sign {n=(to_big_signed_int a); d=(to_big_signed_int b)} {n=(to_big_signed_int c); d=(to_big_signed_int d)} ;;  


(* signed addition *) 

let diff_rat_sign (a: big_signed_rat) (b: big_signed_rat) =
	if (comparaison a.n.n zero) == 0 then 
	let res : big_signed_rat = {  n= b.n; d= b.d }  in res
    else if (comparaison b.n.n zero) == 0 then 
    let res : big_signed_rat = {  n= a.n; d= a.d }  in res
	else if (comparaison a.n.n zero) == 0 && (comparaison b.n.n zero) == 0
	then let res : big_signed_rat = {  n= zero_signed; d= zero_signed }  in res
	else
	let x  = ( diff_sign ( prd_sign a.n b.d) (prd_sign a.d b.n) )
	and xx = ( diff_sign ( prd_sign a.n b.d) (prd_sign a.d b.n) )
	and y  = ( prd_sign a.d b.d)
	and yy = ( prd_sign a.d b.d) in
	let res : big_signed_rat = {  n= div_sign xx {s=1;n=(pgcd x.n y.n)}; 
							      d= div_sign yy {s=1;n=(pgcd x.n y.n)}
							   }  in res ;;

let diff_rat_signed (a,b) (c,d) =
  if (b==0 || d==0)  then 
 	failwith "dividing by zero." 
  else 
  	diff_rat_sign {n=(to_big_signed_int a); d=(to_big_signed_int b)} {n=(to_big_signed_int c); d=(to_big_signed_int d)} ;;  


(* signed multiplication *) 

let mult_rat_sign  (a: big_signed_rat) (b: big_signed_rat) =
	let x  = (prd_sign a.n b.n)
	and xx = (prd_sign a.n b.n); 
	and y  = (prd_sign a.d b.d) 
	and yy = (prd_sign a.d b.d)  in
	let res : big_signed_rat = {  n= div_sign xx {s=1;n=(pgcd x.n y.n)}; 
							      d= div_sign yy {s=1;n=(pgcd x.n y.n)}
							   }  in res ;;

let mult_rat_signed (a,b) (c,d) =
  if (b<=0 || d<=0)  then 
 	failwith "dividing by zero." 
  else 
  	mult_rat_sign {n=(to_big_signed_int a); d=(to_big_signed_int b)} {n=(to_big_signed_int c); d=(to_big_signed_int d)} ;;  


(* signed division *) 

let div_rat_sign  (a: big_signed_rat) (b: big_signed_rat) =
	let x  = (prd_sign a.n b.d)
	and xx = (prd_sign a.n b.d) 
	and y  = (prd_sign a.d b.n) 
	and yy = (prd_sign a.d b.n)  in
	let res : big_signed_rat = {  n= div_sign xx {s=1;n=(pgcd x.n y.n)}; 
							      d= div_sign yy {s=1;n=(pgcd x.n y.n)}
							   }  in res ;;

let div_rat_signed (a,b) (c,d) =
  if (b<=0 || d<=0)  then 
 	failwith "dividing by zero." 
  else 
  	div_rat_sign {n=(to_big_signed_int a); d=(to_big_signed_int b)} {n=(to_big_signed_int c); d=(to_big_signed_int d)} ;;  


show "\ndone successfully.\n\n" ;; 