program Funcs;

function Square(Number: Integer = 10): Integer;
begin
    Result := Number * Number;
end;

var Num: Integer;

begin
  Num := Square(5);
end.