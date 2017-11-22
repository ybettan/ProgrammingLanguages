fun dubchar (x:char) = str(x)^str(x);

fun apply_on_nth_char (f:(char->'a)) = 
  fn n:int => fn s:string => if size(s)<n+1 orelse n<0 then f(#"!") else f(String.sub(s, n));

