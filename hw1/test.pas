program test;

var input : real;

function inRange(input : integer) : Boolean;
begin
	inRange := true;
	if input < 1 then
		inRange := false;
	if input > 255 then
		inRange := false;
end;

begin
	read(input);
	if inRange(input) then
		writeln('Invalid Input');
	writeln('passed the line');
end.