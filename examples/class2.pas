program Classes2;

type 
    Foo = class
    public
        procedure SetWidth(W: double);
    end;
    
    
procedure Foo.SetWidth(W: double);
var
    Intermediate: double;
begin
    Intermediate := W;
end;
    
begin
end.