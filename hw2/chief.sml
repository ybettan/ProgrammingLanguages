(* Yair Schiff 311617989 yair.s.schiff@gmail.com Hadar Serviansky 203994447 hadarser@gmail.com *)

fun dubchar c = str(c)^str(c);

fun apply_on_nth_char f n = fn s => if n < 0 orelse size(s) < n+1 then f(#"!") else f(String.sub(s,n));


fun perfect n = let fun divsum(curr, sum) : int = let
														fun my_div(curr_div) = if n mod curr_div = 0 then curr_div else 0
													in
														if curr=n then sum else divsum(curr+1, sum + my_div(curr))
													end

				in
					if divsum(1,0)=n then true else false
				end;

fun balance s = let 
					fun check_str(open_count,close_count,index) = if index = size(s) then open_count-close_count
													else if close_count > open_count then open_count-close_count
													else if String.sub(s,index) = #"(" then check_str(open_count+1, close_count, index+1)
													else if String.sub(s,index) = #")" then check_str(open_count, close_count+1, index+1)
													else check_str(open_count, close_count, index+1)
				in
					if check_str(0,0,0) = 0 then true else false
				end;

fun sig1 a b f = f(a,f(a,b));
fun sig2 (a,b) f = (a=1 andalso floor(b)=1 andalso f(b)="true");
fun sig3 f a b c = f a b;
fun sig4 a b c d = c+d;
fun sig5 f a g = g(f(a),f(a));
fun sig6 () () = 1;