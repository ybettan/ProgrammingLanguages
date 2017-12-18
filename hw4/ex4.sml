


(* Part I Natural # C'tor *)

datatype natural = zero | Succ of natural;

exception NotNatural;

fun prev zero = raise NotNatural
	| prev (Succ n) = n;
	
fun natural_rep zero = 0
	| natural_rep n = 1 + natural_rep (prev n);
	
fun group_rep 0 = zero
	| group_rep n = if n<0 then raise NotNatural else Succ (group_rep (n-1));
	
infix less_eq;
fun zero less_eq _ = true
	| _ less_eq zero = false
	| n less_eq k = (prev n) less_eq (prev k);

infix gadd;
fun n gadd zero = n
	| n gadd k = (Succ n) gadd (prev k);

infix gmul;
fun zero gmul _ = zero
	| _ gmul zero = zero
	| n gmul k = n gadd ( n gmul (prev k));
	
infix gsub;
fun n gsub zero = n
	| n gsub k = (prev n) gsub (prev k);
	
infix gdiv;
fun n gdiv zero = raise Div
	| zero gdiv _ = zero
	| n gdiv k = if n less_eq (prev k) then zero 
			else Succ( (n gsub k) gdiv k);
	
	


(* Part II Splay Tree *)	

datatype (''a, 'b) SplayTree = 
	Nil
	| Br of ''a * 'b * ((''a, 'b) SplayTree) * ((''a, 'b) SplayTree);
	
datatype Rotate = Zig | ZigZig | ZigZag;
exception NotFound;  

fun size Nil = 0
	| size (Br ( _, _, left_t, right_t)) = 1 + (size left_t) + (size right_t);

	
fun insert _ (Nil,(nk, nv))= Br (nk,nv,Nil,Nil)
	| insert cmp ((Br (k,v,left_t,right_t)), (nk,nv)) = 
		case cmp (k,nk) of 
			EQUAL   => Br (k, nv, left_t, right_t)
		  | GREATER => Br (k, v, left_t, (insert cmp (right_t ,(nk,nv))))
		  | LESS    => Br (k, v, (insert cmp (left_t ,(nk,nv))), right_t)
		  

		
	
		
		
		
		
		
		
		
		
		
		
		
		
		
		