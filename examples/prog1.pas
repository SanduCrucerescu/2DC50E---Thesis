program Statement.Something.Foo.Bar;

type Baz = class
    public
        function Max(A: array of Real; N: Integer): Real; overload;
        function Max(A: array of Real): Integer; overload;
    end;


function Baz.Max(A: array of Real; N: Integer): Real; overload;
var
    X: Real;
    I: Integer;
begin
    X := A[0];
    for I := 1 to N - 1 do
        if X < A[I] then X := A[I];
    Max := X;
end;

function Baz.Max(A: array of Real): Integer; overload;
var
    X: Real;
    I: Integer;
begin
    X := A[0];
    for I := 1 to Length(A) - 1 do
        if X < A[I] then X := A[I];
    Max := X;
end;

procedure NumString(N: Integer; var S: string);
var
    V: Integer;
begin
    V := Abs(N);
    S := '';
    if N < 0 then S := '-' + S;
end;

begin
    X := Foo + Bar;
end.