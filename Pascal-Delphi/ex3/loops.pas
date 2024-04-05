program loops;

uses
  crt,
  SysUtils;

type
  Student = record
    FirstName: string;
    LastName: string;
    AverageGrade: integer;
  end;
  StudentsArray = array of Student;
var
  Students: StudentsArray;

  function CheckIfInteger(Input: string): integer;
  var
    Response: integer;

  begin
    Response := 0;
    repeat
      Val(Input, Response);
      if (Response = 0) then
      begin
        WriteLn('Please write an integer, or press ENTER to exit.');
        ReadLn(Input);
        if (Input = '') then
        begin
          Halt;
        end;
      end;
    until (Response > 0);

    CheckIfInteger := Response;
  end;


  procedure InitArray();
  var
    Input: string;
    StudentNumber: integer;

  begin
    WriteLn('How many students are in the class?');
    ReadLn(Input);
    StudentNumber := CheckIfInteger(Input);
    SetLength(Students, StudentNumber - 1);
  end;

  procedure AddStudentInformation();
  var
    FirstName, LastName, AverageGrade: string;
    i: integer;
    StudentTemp: Student;

  begin
    for i := 0 to Length(Students) do
    begin
      WriteLn(Format('Student %d first name:', [i + 1]));
      ReadLn(FirstName);
      StudentTemp.FirstName := FirstName;
      WriteLn(Format('Student %d last name:', [i + 1]));
      ReadLn(LastName);
      StudentTemp.LastName := LastName;
      WriteLn(Format('Student %d average grade:', [i + 1]));
      ReadLn(AverageGrade);
      StudentTemp.AverageGrade := CheckIfInteger(AverageGrade);
      Students[i] := StudentTemp;
    end;
  end;

  procedure ListStudents();
  var
    i: integer;
    AverageGrade: double;
  begin
    WriteLn('First Name  |  Last Name  |  Average Grade');
    for i := 0 to Length(Students) do
    begin
      Write(Students[i].FirstName: 12);
      Write(' | ');
      Write(Students[i].LastName: 12);
      Write(' | ');
      WriteLn(Students[i].AverageGrade: 3);
      AverageGrade := AverageGrade + Students[i].AverageGrade;
    end;
    AverageGrade := AverageGrade / Length(Students);
    WriteLn(Format('Average class grade: %s', [AverageGrade]));
  end;

begin
  InitArray;
  AddStudentInformation;
  ListStudents;
end.
