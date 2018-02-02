(* Alon Kwart 201025228 kwart@campus.technion.ac.il Yonathan Bettan 302279138 yonibettan@gmail.com*)

(* Alternating list *)

local 
	infixr 5 %%;
in
	datatype ('a,'b) hetrolist = hnil
					| %% of 'a * ('b,'a) hetrolist;
					

					
	fun build4 (x, one, y, two) = x %% one %% y %% two %% hnil;

	local
		fun unzip_aux hnil = nil
			| unzip_aux (a %% hnil) = a :: nil
			| unzip_aux (a %% (b %% t)) = a :: (unzip_aux t);
	in 
		fun unzip hnil = (nil, nil)
			| unzip (a %% hnil) = ((a::nil), nil)
			| unzip (a %% (b %% t)) = ((a:: (unzip_aux t)), (unzip_aux (b %% t)));
	end

	fun zip (nil, nil) = hnil
		| zip (nil , _ ) = raise Empty
		| zip ((a :: nil), nil) = a %% hnil
		| zip ((a1 :: a2 :: _), nil) = raise Empty
		| zip ((a :: A), (b :: B)) = a %% (b %% (zip (A,B)));
end


(* Sequence  *)

datatype 'a seq = Nil | Cons of 'a * (unit-> 'a seq);
exception EmptySeq;
fun head(Cons(x,_)) = x | head Nil = raise EmptySeq;
fun tail(Cons(_,xf)) = xf() | tail Nil = raise EmptySeq;
datatype direction = Back | Forward;
datatype 'a bseq =   bNil | bCons of 'a * (direction -> 'a bseq);
fun bHead(bCons(x,_)) = x | bHead bNil = raise EmptySeq;
fun bForward(bCons(_,xf)) = xf(Forward) | bForward bNil = raise EmptySeq;
fun bBack(bCons(_,xf)) = xf(Back) | bBack bNil = raise EmptySeq;

fun intbseq x = bCons(x, fn direct => if direct = Forward then intbseq(x + 1) else intbseq(x - 1));


fun bmap _ bNil = bNil
	| bmap func seque = bCons(func (bHead seque),
							  fn direct => 
								if direct = Forward then bmap func (bForward seque)
								else bmap func (bBack seque)
							);
							
fun bfilter _ _ bNil = bNil
	| bfilter filter direct (bCons( v, func )) = 
		if filter v then 
			bCons( v, fn x => (bfilter filter x (func x)))
		else 
			bfilter filter direct (func direct);

fun seq2bseq reverse regular =
		bCons((head regular), fn direct => (
			if direct = Forward then 
				seq2bseq (Cons((head regular), fn() => reverse)) (tail regular)
			else
				seq2bseq (tail reverse) (Cons((head reverse), fn()=>regular))
			)
		);
local
	fun jump_aux seque _ 0 _ = seque
		| jump_aux (bCons( _ , f)) n m direct = jump_aux (f(direct)) n (m-1) direct;
in
	fun bSeqJump seque n = bCons((bHead seque), fn direct => bSeqJump (jump_aux seque n n direct) n);
end











