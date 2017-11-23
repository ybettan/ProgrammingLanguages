{ Alon Kwart 201025228 kwart@campus.technion.ac.il }
{ Yoni Bettan 302279138 yonibettan@gmail.com }

program gray;

var input , i : integer;
var word : array [0..255] of integer;
type Location = (Inside, Outside);

procedure print_code( code_word : array of integer; length : integer);
begin 
	for i:= 0 to length - 1 do
		if i <> length - 1 then
			write(code_word[i])
		else
			writeln(code_word[length-1]);
end;

procedure gray_aux(length : integer; prev_location : Location; code_word : array of integer; total : integer);
begin
	if length = total then
		print_code(code_word, total)
	else
	begin
		if prev_location = Inside then
		begin
			code_word[length] := 0;
			gray_aux(length + 1, Inside, code_word, total);
			code_word[length] := 1;
			gray_aux(length + 1, Outside, code_word, total);
		end
		else
		begin
			code_word[length] := 1;
			gray_aux(length + 1, Inside, code_word, total);
			code_word[length] := 0;
			gray_aux(length + 1, Outside, code_word, total);
		end;
	end;
end;

function inRange(input : integer) : Boolean;
begin
	inRange := true;
	if input < 1 then
		inRange := false;
	if input > 255 then
		inRange := false;
end;



{program gray}
begin
	for i:= 0 to 255 do
		word[i] := 9;
	read(input);
	if inRange(input) then
		gray_aux(0, Inside, word, input)
	else 
		writeln('Invalid Input');
end.