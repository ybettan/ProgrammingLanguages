(* Alon Kwart 201025228 kwart@campus.technion.ac.il *)
(* Yonathan Bettan 302279138 yonibettan@gmail.com *)

fun curry (func:('a*'b -> 'c )) = fn first_arg : 'a => fn second_arg : 'b => func(first_arg, second_arg);

fun uncurry (func: 'a -> 'b -> 'c) = fn (first_arg,second_arg) : 'a*'b => (func(first_arg))(second_arg);