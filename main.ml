#use "bigint.ml";;
#use "bigrat.ml";;
#use "init.ml" ;;


show "\n\n\n\n\n\n TESTS :  Arithmitique des grands nombres. :" ;;


show "\n\n I.  ENTIERs:" ;;

show "\n\n I.1 ADDITION :" ;;

	show "\n\n   Test 1.1 : (abi1 + bbi1) + (cbi1 + dbi1) =" ;;
	let (i1:big_signed_int) = add_sign (add_sign abi1 bbi1 ) (add_sign cbi1 dbi1) ;;


show "\n\n I.2 SOUSTRACTION :" ;;

	show "\n\n * Test 1.2 : (abi2 -bbi2) - (cbi2 * dbi2) =" ;;
	let (i2:big_signed_int) = diff_sign (diff_sign abi2 bbi2) (diff_sign cbi2 dbi2) ;;


show "\n\n I.3 MULTIPLICATION :" ;;

	show "\n\n * Test 1.3 : (abi3 * bbi3) * (cbi3 * dbi3) =" ;;
	let (i3:big_signed_int) = prd_sign i1 (prd_sign i1 (prd_sign (prd_sign abi3 bbi3) (prd_sign cbi3 dbi3))) ;;


show "\n\n I.4 DIVISION :" ;;

	show "\n\n * Test 1.4 : div_sign (div_sign cbi4 dbi4) (div_sign abi4 bbi4)  =" ;;
	let (i4:big_signed_int) =  div_sign (div_sign cbi4 dbi4) (div_sign abi4 bbi4)   ;;



show "\n\n I.5 FACTORIAL :" ;;

	show "\n\n * Test 1.4 :  factorial 1000  =" ;;
	let (i5:big_int) =  factorial [|1;0;0;0|];;



show "\n\n I.6 MODULO :" ;;

	show "\n\n * Test 1.4 :  modulo m1 m2 =" ;;
	let (i6:big_int) =  modulo m1 m2 ;;



show "\n\n I.7 PGCD :" ;;

	show "\n\n * Test 1.4 :  pgcd a1 b1 =" ;;
	let (i7:big_int) =  pgcd a1 b1   ;;







show "\n\n\n\n II. RATIONNELS :" ;;

show "\n\n II.1 ADDITION  :" ;;

	show "\n\n   Test 2.1 : (gbr + hbr) =" ;;
	let (r1:big_signed_rat) = add_rat_sign hbr gbr  ;;

show "\n\n II.2 SOUSTRACTION  :" ;;

	show "\n\n   Test 2.2 : (gbr - hbr)  =" ;;
	let (r2:big_signed_rat) = diff_rat_sign gbr hbr;;


show "\n\n II.3 MULTIPLICATION :" ;;

	show "\n\n   Test 2.3 :  gbr * hbr =" ;;
	let (r3:big_signed_rat) = mult_rat_sign hbr hbr ;;



show "\n\n II.4 DIVISION :" ;;

	show "\n\n   Test 2.4 : gbr / hbr =" ;;
	let (r4:big_signed_rat) = div_rat_sign gbr hbr   ;;






show "---------------------------------------------------FINISHED.-----------------------------------------------------" ;;
