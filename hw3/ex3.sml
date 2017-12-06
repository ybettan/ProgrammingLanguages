


(* Transpose *)

fun T ( nil :: _ ) = nil
	| T mat : real list list = 
		(map hd mat) :: T (map tl mat);


(* Add *)

infix ++;		
local 
	fun add_rows (nil , nil) = nil
		| add_rows (( x :: r1), ( y :: r2)) = 
			(x+y) :: add_rows(r1, r2);

in
	fun  nil ++ nil  = nil
		|  (row1 :: mat1) ++ (row2 :: mat2) =  
			add_rows(row1, row2) ::  (mat1 ++ mat2);
end;


(* Mull *)

infix **;
local 
	fun mul_rows nil nil = nil
		| mul_rows ( (x : real) :: r1) ( (y : real) :: r2) = 
			(x*y) :: mul_rows r1 r2;
			
	fun sum_row nil n : real = n
		| sum_row ((x : real) :: row) n = sum_row row (n+x);
		
	fun create_row nil _  = nil
		| create_row (row2 :: mat) row1 = (sum_row (mul_rows row1 row2) 0.0) :: create_row mat row1;
		
in 
	fun mat1 ** mat2 = map (create_row (T mat2)) mat1;
end;


(* Inverse *)

local
	fun remove_row mat1 ((row : real list) :: mat2) 0 = mat1 @ mat2
		| remove_row mat1 (row :: mat2) n = remove_row (mat1 @ [row]) mat2 (n-1);

	fun get_sub_mat mat n k= if (n=k) then (T (remove_row nil (T mat) n)) else 
		get_sub_mat mat n (k+1);

	fun det_aux (((x : real) :: nil ) :: nil) _ = x
		| det_aux (( x :: nil) :: mat) n = x*(det_aux (get_sub_mat mat n 0) 0)
		| det_aux (( x :: row) :: mat) n = x*(det_aux (get_sub_mat mat n 0) 0) - (det_aux ([row]@mat) (n+1));
												 
	fun det A = det_aux A 0;

	fun minor mat row col = T (remove_row nil ( T (remove_row nil mat row)) col);

	fun head y (x :: xs ) = [[y]@x]@xs;

	fun set_sign n (x : real) = if ( n mod 2 = 0) then x else (0.0-x);


	fun adjoint_row mat i j ((x : real) :: nil) = [set_sign (i+j) (det (minor mat i j))]
		| adjoint_row mat i j (x :: row) = (set_sign (i+j) (det (minor mat i j))) :: (adjoint_row mat i (j+1) row);
		
	fun adjoint_aux (row :: nil) mat i = [ adjoint_row mat i 0 row]
		| adjoint_aux ( row :: mat1) mat2 i = (adjoint_row mat2 i 0 row) :: (adjoint_aux mat1 mat2 (i+1));
		
	fun adjoint mat = T (adjoint_aux mat mat 0);

	fun mul_scalar mat (x : real) = map (map (fn y => x*y)) mat;
in
	fun inverse mat =  mul_scalar (adjoint mat) (1.0 / (det mat));
end;


(* this test returns valus that are e-15 and alike, numbers are not round *)

val MAT1 =[ [1.0, 1.0, 1.0, 1.0],
			[1.0, 2.0, 3.0, 4.0],
			[1.0, 4.0, 9.0, 16.0],
			[1.0, 8.0, 27.0, 64.0] ];

inverse MAT1;

(inverse MAT1) ** MAT1;








