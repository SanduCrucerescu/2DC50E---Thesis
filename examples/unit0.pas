unit Funcs;

interface

function Foo(X : Integer): Integer;
procedure Bar;

implementation

var Name: String;

function Foo(X : Integer = 10): Integer;
begin
    Result := X;
end;

procedure Bar;
var 
    X: Integer;
begin
    X := 1;
end;

end.