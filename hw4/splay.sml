(* Alon Kwart 201025228 kwart@campus.technion.ac.il Yonathan Bettan 302279138 yonibettan@gmail.com*)


(* Part II Splay Tree *)	

datatype (''a, 'b) SplayTree = 
	Nil
	| Br of ''a * 'b * ((''a, 'b) SplayTree) * ((''a, 'b) SplayTree);
	
datatype Rotate = Zig | ZigZig | ZigZag;
exception NotFound;


local
	fun zig (Br (k, v,(Br (lk, lv, ltree_l, ltree_r)), tree_r)) = Br (lk, lv, ltree_l, (Br (k, v, ltree_r, tree_r)));;
	fun zag (Br (k, v, tree_l,(Br (rk, rv, rtree_l, rtree_r)))) = Br (rk, rv, (Br (k, v, tree_l, rtree_l)),rtree_r );
	fun zig_zig (Br (kz, vz, (Br (ky, vy, (Br(kx,vx,A,B)), C)), D)) = Br (kx,vx,A,(Br (ky,vy,B,(Br (kz,vz,C,D)))));
	fun zag_zag (Br (kx,vx,A,(Br (ky,vy,B,(Br (kz,vz,C,D)))))) = Br (kz, vz, (Br (ky, vy, (Br(kx,vx,A,B)), C)), D);
	fun zig_zag (Br (kz,vz,(Br(ky,vy,A,(Br (kx,vx,B,C)))),D)) = Br (kx,vx,(Br (ky,vy,A,B)),(Br (kz,vz,C,D)));
	fun zag_zig (Br (kz,vz,A,(Br(ky,vy,(Br(kx,vx,B,C)),D)))) = Br (kx,vx,(Br(kz,vz,A,B)),(Br(ky,vy,C,D)));
	fun insert_aux _ (Nil,(nk, nv))= Br (nk,nv,Nil,Nil)
		| insert_aux cmp ((Br (k,v,left_t,right_t)), (nk,nv)) = 
			case cmp (k,nk) of 
				EQUAL      => Br (k, nv, left_t, right_t)
			  | LESS       => Br (k, v, left_t, (insert_aux cmp (right_t ,(nk,nv))))
			  | GREATER    => Br (k, v, (insert_aux cmp (left_t ,(nk,nv))), right_t);

	fun splay_double cmp Nil nk = raise NotFound
		| splay_double cmp (Br (k,v,left_t,right_t)) nk =
			case cmp (nk,k) of
				EQUAL => Br (k,v,left_t,right_t)
				| LESS => 
					(case left_t of 
						Nil => raise NotFound
						| (Br (lk,lv,lleft_t,lright_t)) => 
							(case cmp (nk,lk) of
								EQUAL => Br (k,v,left_t,right_t)
								| LESS =>
									(case lleft_t of 
										Nil => raise NotFound (* Look for ZigZig or ZigZag *)
										| (Br (llk,_,_,_)) => 
											(case cmp (nk,llk) of 
												EQUAL => zig_zig (Br (k,v,left_t,right_t))
												| _ => splay_double cmp (Br (k,v,(splay_double cmp left_t nk),right_t)) nk
											)
									)
								| GREATER =>
									(case lright_t of 
										Nil => raise NotFound (* Look for ZigZag or ZigZag *)
										| (Br (rlk,_,_,_)) => 
											(case cmp (nk,rlk) of 
												EQUAL => zig_zag (Br (k,v,left_t,right_t))
												| _ => splay_double cmp (Br (k,v,(splay_double cmp left_t nk),right_t)) nk
											)
									)
							)
					)
				| GREATER =>
					(case right_t of 
						Nil => raise NotFound
						| (Br (rk,rv,rleft_t,rright_t)) => 
							(case cmp (nk,rk) of
								EQUAL => Br (k,v,left_t,right_t)
								| LESS =>
									(case rleft_t of 
										Nil => raise NotFound
										| (Br (lrk,_,_,_)) => 
											(case cmp (nk,lrk) of 
												EQUAL => zag_zig (Br (k,v,left_t,right_t))
												| _ => splay_double cmp (Br (k,v,left_t,(splay_double cmp right_t nk))) nk
											)
									)
								| GREATER =>
									(case rright_t of 
										Nil => raise NotFound 
										| (Br (rlk,_,_,_)) => 
											(case cmp (nk,rlk) of 
												EQUAL => zag_zag (Br (k,v,left_t,right_t))
												| _ => splay_double cmp (Br (k,v,left_t,(splay_double cmp right_t nk))) nk
											)
									)
							)
					);
					
	fun splay cmp Nil nk = raise NotFound
		| splay cmp tree nk =
			let
				val (Br (k,v,left_t,right_t)) = splay_double cmp tree nk
			in
			case cmp (nk,k) of
				EQUAL => Br (k,v,left_t,right_t)
				| LESS => 
					(case left_t of 
						Nil => raise NotFound
						| (Br (lk,lv,lleft_t,lright_t)) => 
							(case cmp (nk,lk) of
								EQUAL => zig (Br (k,v,left_t,right_t)) (* Zig *)
								| _ => raise NotFound
							)
					)
				| GREATER =>
					(case right_t of 
						Nil => raise NotFound
						| (Br (rk,rv,rleft_t,rright_t)) => 
							(case cmp (nk,rk) of
								EQUAL => zag (Br (k,v,left_t,right_t)) (* Zag *)
								| _ => raise NotFound
							)
					)
			end;
in
	fun size Nil = 0
		| size (Br ( _, _, left_t, right_t)) = 1 + (size left_t) + (size right_t);		
	fun insert cmp (tree,(k,v)) = splay cmp (insert_aux cmp (tree, (k, v))) k;	 
	fun get cmp ((tree : ('a,'b)SplayTree), (k : 'a)) = 
		let
			val Br (nk,nv,_,_) = splay cmp tree k
		in 
		nv
		end;
	fun inorder Nil = nil
		| inorder (Br (k, v, left_t, right_t)) = (inorder left_t)@[v]@(inorder right_t);
end