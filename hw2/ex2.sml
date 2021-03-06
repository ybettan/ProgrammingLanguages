(* Alon Kwart 201025228 kwart@campus.technion.ac.il Yonathan Bettan 302279138 yonibettan@gmail.com*)


(* q1 *)
fun dubchar (x:char) = str(x)^str(x);

fun apply_on_nth_char (f:(char->'a)) = 
  fn n:int => fn s:string => if size(s)<n+1 orelse n<0 then f(#"!") else f(String.sub(s, n));


(* q2 *)
local
    fun devides ( n : int , d : int ) = n mod d = 0;
    
    fun sum_dividers( n : int, d : int, sum : int ) = 
      if n=d andalso sum = n then true
      else if n<=d orelse sum > n then false
      else if devides(n,d) then sum_dividers(n,d+1,sum+d)
      else sum_dividers(n,d+1,sum);
in 
    fun  perfect ( n : int ) = sum_dividers(n,1,0);
end;


(* q3 *)
fun balance (s:string) = let
  fun helper (s:string, opens:int, closes:int, index:int) = 
    if size(s)=index then 
      (if opens=closes then true else false)
       else if closes>opens then false
       else if String.sub(s, index)=(#"(") then helper(s, opens+1, closes, index+1)
       else if String.sub(s, index)=(#")") then helper(s, opens, closes+1, index+1)
       else helper(s, opens, closes, index+1)

in
  helper(s, 0, 0, 0)
end;


  
(* q4 *)
fun sig1 a b f = f(a, f(a, b));
fun sig2 (n, r) f = (n=0 andalso round(r)=0 andalso (f(r)="I Love Drugs"));
fun sig3 f a b c = f a b;
fun sig4 a b c d = c*d;
fun sig5 f a g = g(f(a), f(a));
fun sig6 () = fn () => 1;

