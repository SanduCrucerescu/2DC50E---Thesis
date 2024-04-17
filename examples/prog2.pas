program Classes;

type 
    Foo = class
    private
        function Bar(): Integer; virtual; override;
        procedure Baz(Name: String); overload;
        procedure Baz(Name: String; Age: Integer); overload
    end;
    
function Foo.Bar(): Integer;
begin
    Result := 10;
end;

procedure Foo.Baz(Name: String);
begin
    WriteLine(Name);
end;

procedure Foo.Baz(Name: String; Age: Integer);
begin
    WriteLine(Name + Age);
end;

function Bar(): Integer;
begin
    Result := 1;
end;

begin
end.