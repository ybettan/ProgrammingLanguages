


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

fun max_city west north = map (fn y => (map (fn x => if x<y then x else y) north)) west;

local 
	fun enumarate_aux nil _ = nil
		| enumarate_aux ((x: int) :: xs) n = (x, n) :: (enumarate_aux xs (n+1));

	fun enumarate vec = enumarate_aux vec 0;

	fun drop_val row nil _ = row
		| drop_val row ((x,j) :: xs) (v,i) = if (x=v andalso i=j) then (row@xs) else drop_val (row@[(x,j)]) xs (v,i);

	fun range start stop = if start = stop then nil else start:: (range (start+1) stop);
	(* get to lists and build an empty mat with thier dimentions *)
	fun build_empty_city rows cols = map (fn row => (map (fn cell => 0) cols)) rows;
	(* insert an elemnt to a list *)
	fun insert_to_row (x::xs) (v,j) = if j = 0 then v::xs else x::(insert_to_row xs (v,(j-1)));
	(* insert an element to the mat *)
	fun insert_to_mat (row::mat) (v,i,j) = if i=0 then (insert_to_row row (v,j))::mat else row :: (insert_to_mat mat (v,(i-1),j));
	(* insert all elements in to the city/  mat *)
	fun insert_places mat nil = mat
		| insert_places mat (place::places) = insert_places (insert_to_mat mat place) places;
		
	(* check if a western has a match with north. *)
	fun find_full_match (w,i) nil = (0-1,0-1,0-1)
		| find_full_match (w,i) ((v,j)::north) = if (w=v) then (w,i,j) else find_full_match (w,i) north;
		
	fun has_a_match (v: int,i:int,j:int) = (v>=0 andalso i>=0 andalso j>=0)
		
	fun find_first (w:int,i:int) ((v:int,j:int)::north) = if w<=v then (w,i,j) else find_first (w,i) north;

	fun remove_match _ rNorth nil = rNorth
		| remove_match (v:int,i:int,j:int) rNorth ((n:int,k:int)::lNorth) = if (v=n andalso j=k) then (rNorth@lNorth)
													else (remove_match (v,i,j) (rNorth@[(n,k)]) lNorth);
		
	fun find_partners_west (rNorth,fNorth,places) nil = (rNorth,places)
		| find_partners_west (rNorth,fNorth,places) ((w:int,i:int):: west) = 
		let
			val full_match = find_full_match (w,i) rNorth;
			val first_match = find_first (w,i) fNorth;
			val new_rNorth = remove_match full_match [] rNorth;
		in
			if (has_a_match full_match) then find_partners_west (new_rNorth, fNorth, full_match::places) west
			else find_partners_west (rNorth, fNorth, first_match::places) west
		end;

	fun find_first_north (v,j:int) ((x,i:int):: west) = if (v<=x) then (v,i,j) else find_first_north (v,j) west;

	fun find_partners_north (nil,places) west = places
		| find_partners_north (((v,j)::rNorth),places) west = 
		let
			val new_place = find_first_north (v,j) west;
		in 
			find_partners_north (rNorth, (new_place::places)) west
		end;

	fun find_partners west north = find_partners_north (find_partners_west (north,north,nil) west) west;
in
	fun min_city west north = insert_places (build_empty_city west north) (find_partners (enumarate west) (enumarate north));
end;






