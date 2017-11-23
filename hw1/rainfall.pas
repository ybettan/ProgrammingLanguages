{ Alon Kwart 201025228 kwart@campus.technion.ac.il }
{ Yonathan Bettan 302279138 yonibettan@gmail.com }

program rainfall;

var 
	STOP : integer = -999;
	current : integer = 0;
	counter : integer = 0;
	sum : longint = 0;
	fun_avg : real = 0;

begin
	while true do
	begin
		readln(current);
		{ if the current input is -999 we need to break }
		if current = STOP then
			Break;
		{ if the current input is negative we wont calculate it}
		if current < 0 then
			Continue;
		{ otherwise we can add it to the sum and increase the counter}
		sum := sum + current;
		Inc(counter);
	end;
	{if there was no input, meaning all the inputs till -999 were negative
	 we dont want to divide with zero. The actual output is avg of 0 elements}
	if counter <> 0 then
		fun_avg := sum / counter;
		
	writeln(fun_avg);
end.

