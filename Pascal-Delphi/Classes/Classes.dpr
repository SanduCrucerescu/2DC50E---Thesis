program TaskManagerApp;

{$MODE delphi}
{$M+}

uses
  SysUtils,
  fgl,
  DateUtils;

type
  TTask = class
  private
    FTitle: string;
    FDescription: string;
    FDueDate: TDateTime;
    FCompleted: boolean;
    FPriority: integer;
  public
    constructor Create(const Title, Description: string; const DueDate: TDateTime;
      const Priority: integer);
    procedure MarkAsCompleted;
    property Title: string read FTitle;
    property Description: string read FDescription;
    property DueDate: TDateTime read FDueDate;
    property Completed: boolean read FCompleted;
    property Priority: integer read FPriority;
  end;

  TTaskList = class
  private
    FTasks: specialize TFPGObjectList<TTask>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddTask(const Task: TTask);
    procedure RemoveTask(const Task: TTask);
    function GetTaskCount: integer;
    function GetTask(const Index: integer): TTask;
    procedure SortTasksByPriority;
    procedure SaveTasksToFile(const FileName: string);
    procedure LoadTasksFromFile(const FileName: string);
  end;

  { TTask }

  constructor TTask.Create(const Title, Description: string;
  const DueDate: TDateTime; const Priority: integer);
  begin
    FTitle := Title;
    FDescription := Description;
    FDueDate := DueDate;
    FCompleted := False;
    FPriority := Priority;
  end;

  procedure TTask.MarkAsCompleted;
  begin
    FCompleted := True;
  end;

  { TTaskList }

  constructor TTaskList.Create;
  begin
    FTasks := specialize TFPGObjectList<TTask>.Create;
  end;

  destructor TTaskList.Destroy;
  begin
    FTasks.Free;
    inherited;
  end;

  procedure TTaskList.AddTask(const Task: TTask);
  begin
    FTasks.Add(Task);
  end;

  procedure TTaskList.RemoveTask(const Task: TTask);
  begin
    FTasks.Remove(Task);
  end;

  function TTaskList.GetTaskCount: integer;
  begin
    Result := FTasks.Count;
  end;

  function TTaskList.GetTask(const Index: integer): TTask;
  begin
    Result := FTasks[Index];
  end;

  function CompareTasksByPriority(const Left, Right: TTask): Integer;
begin
  Result := Left.Priority - Right.Priority;
end;

procedure TTaskList.SortTasksByPriority;
begin
  FTasks.Sort(@CompareTasksByPriority);
end;

  procedure TTaskList.SaveTasksToFile(const FileName: string);
  var
    I: integer;
    TaskFile: TextFile;
  begin
    AssignFile(TaskFile, FileName);
    Rewrite(TaskFile);
    try
      for I := 0 to FTasks.Count - 1 do
      begin
        with FTasks[I] do
          WriteLn(TaskFile, Format('%s,%s,%s,%d,%d',
            [Title, Description, DateToStr(DueDate), integer(Completed), Priority]));
      end;
    finally
      CloseFile(TaskFile);
    end;
  end;

  procedure TTaskList.LoadTasksFromFile(const FileName: string);
  var
    TaskFile: TextFile;
    Line: string;
    Title, Description: string;
    DueDate: TDateTime;
    Completed: boolean;
    Priority: integer;
  begin
    FTasks.Clear;
    AssignFile(TaskFile, FileName);
    Reset(TaskFile);
    try
      while not EOF(TaskFile) do
      begin
        ReadLn(TaskFile, Line);
        Title := Copy(Line, 1, Pos(',', Line) - 1);
        Delete(Line, 1, Pos(',', Line));
        Description := Copy(Line, 1, Pos(',', Line) - 1);
        Delete(Line, 1, Pos(',', Line));
        DueDate := StrToDate(Copy(Line, 1, Pos(',', Line) - 1));
        Delete(Line, 1, Pos(',', Line));
        Completed := boolean(StrToInt(Copy(Line, 1, Pos(',', Line) - 1)));
        Delete(Line, 1, Pos(',', Line));
        Priority := StrToInt(Line);
        AddTask(TTask.Create(Title, Description, DueDate, Priority));
        if Completed then
          FTasks.Last.MarkAsCompleted;
      end;
    finally
      CloseFile(TaskFile);
    end;
  end;

var
  TaskList: TTaskList;
  Choice: integer;
  Title, Description: string;
  DueDate: TDateTime;
  Priority: integer;
  TaskIndex: integer;
  DueDateStr: string;
  FormatSettings: TFormatSettings;

begin
  TaskList := TTaskList.Create;
  try
    // Load tasks from file (if exists)
    if FileExists('tasks.txt') then
      TaskList.LoadTasksFromFile('tasks.txt');

    repeat
      Writeln('Task Manager');
      Writeln('1. Add Task');
      Writeln('2. Remove Task');
      Writeln('3. Mark Task as Completed');
      Writeln('4. Display Tasks');
      Writeln('5. Sort Tasks by Priority');
      Writeln('6. Save Tasks');
      Writeln('0. Exit');
      Write('Enter your choice: ');
      Readln(Choice);
      
      FormatSettings.ShortDateFormat := 'yyyy-mm-dd';
      FormatSettings.DateSeparator := '-';
      case Choice of
        1:
          begin
             Write('Enter task title: ');
              Readln(Title);
              Write('Enter task description: ');
              Readln(Description);
              
              while True do
              begin
                Write('Enter task due date (YYYY-MM-DD): ');
                Readln(DueDateStr);
                
                try
                  DueDate := StrToDate(DueDateStr, FormatSettings);
                  Break;
                except
                  on EConvertError do
                  begin
                    Writeln('Invalid date format. Please enter a valid date (YYYY-MM-DD).');
                  end;
                end;
              end;
              
              Write('Enter task priority (1-5): ');
              Readln(Priority);
              TaskList.AddTask(TTask.Create(Title, Description, DueDate, Priority));
              Writeln('Task added successfully.');
          end;
        2:
        begin
          Write('Enter the index of the task to remove: ');
          Readln(TaskIndex);
          if (TaskIndex >= 0) and (TaskIndex < TaskList.GetTaskCount) then
          begin
            TaskList.RemoveTask(TaskList.GetTask(TaskIndex));
            Writeln('Task removed successfully.');
          end
          else
            Writeln('Invalid task index.');
        end;
        3:
        begin
          Write('Enter the index of the task to mark as completed: ');
          Readln(TaskIndex);
          if (TaskIndex >= 0) and (TaskIndex < TaskList.GetTaskCount) then
          begin
            TaskList.GetTask(TaskIndex).MarkAsCompleted;
            Writeln('Task marked as completed.');
          end
          else
            Writeln('Invalid task index.');
        end;
        4:
        begin
          Writeln('Tasks:');
          for TaskIndex := 0 to TaskList.GetTaskCount - 1 do
          begin
            with TaskList.GetTask(TaskIndex) do
              Writeln(Format(
                'Index: %d, Title: %s, Priority: %d, Due Date: %s, Completed: %s',
                [TaskIndex, Title, Priority, DateToStr(DueDate),
                BoolToStr(Completed, True)]));
          end;
        end;
        5:
        begin
          TaskList.SortTasksByPriority;
          Writeln('Tasks sorted by priority.');
        end;
        6:
        begin
          TaskList.SaveTasksToFile('tasks.txt');
          Writeln('Tasks saved to file.');
        end;
        0:
          Writeln('Exiting the program.');
        else
          Writeln('Invalid choice. Please try again.');
      end;

      Writeln;
    until Choice = 0;

  finally
    TaskList.Free;
  end;
end.
