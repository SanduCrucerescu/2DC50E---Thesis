unit A;

interface

function Hi(): string;
procedure Hello;

implementation

function Hi(): string;
begin
    Result := 'Hello World';
end;

procedure Hello;
begin
    WriteLn('Moin :D');
end;

end.
