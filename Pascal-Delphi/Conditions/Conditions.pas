program conditions;

uses
  crt;

var
  Input: string;
  Age: integer;
  IsInteger: integer;

  procedure StudentAge(Num: integer);
  begin
    if (Num < 18) then
    begin
      WriteLn('The person is underage!');
    end
    else
    begin
      WriteLn('The person is eligible for this position.');
    end;
  end;



begin
  WriteLn('What is your age?');
  ReadLn(Input);

  Val(Input, Age, IsInteger);

  if (IsInteger = 0) then
  begin
    StudentAge(Age);
  end
  else
  begin
    WriteLn('Please insert an integer.');
    WriteLn('Press any key to exit.');
    ReadKey;
  end;
end.
