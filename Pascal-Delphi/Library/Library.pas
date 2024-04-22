program LibraryManagementSystem;

{$MODE OBJFPC}
{$M+}

uses
  SysUtils;

type
  TBook = record
    Title: string;
    Author: string;
    ISBN: string;
    Available: boolean;
  end;

  TLibrary = class
  private
    FBooks: array of TBook;
    FCapacity: integer;
    FCount: integer;
  public
    constructor Create(Capacity: integer);
    procedure AddBook(Book: TBook);
    procedure RemoveBook(ISBN: string);
    function FindBook(ISBN: string): integer;
    procedure BorrowBook(ISBN: string);
    procedure ReturnBook(ISBN: string);
    procedure DisplayBooks;
    function GetBookCount: integer;
  end;

  constructor TLibrary.Create(Capacity: integer);
  begin
    FCapacity := Capacity;
    FCount := 0;
    SetLength(FBooks, FCapacity);
  end;

  procedure TLibrary.AddBook(Book: TBook);
  begin
    if FCount < FCapacity then
    begin
      FBooks[FCount] := Book;
      Inc(FCount);
      WriteLn('Book added successfully.');
    end
    else
      WriteLn('Library is full. Cannot add more books.');
  end;

  procedure TLibrary.RemoveBook(ISBN: string);
  var
    I, Index: integer;
  begin
    Index := FindBook(ISBN);
    if Index <> -1 then
    begin
      for I := Index to FCount - 2 do
        FBooks[I] := FBooks[I + 1];
      Dec(FCount);
      WriteLn('Book removed successfully.');
    end
    else
      WriteLn('Book not found.');
  end;

  function TLibrary.FindBook(ISBN: string): integer;
  var
    I: integer;
  begin
    Result := -1;
    for I := 0 to FCount - 1 do
    begin
      if FBooks[I].ISBN = ISBN then
      begin
        Result := I;
        Break;
      end;
    end;
  end;

  procedure TLibrary.BorrowBook(ISBN: string);
  var
    Index: integer;
  begin
    Index := FindBook(ISBN);
    if (Index <> -1) and FBooks[Index].Available then
    begin
      FBooks[Index].Available := False;
      WriteLn('Book borrowed successfully.');
    end
    else if Index = -1 then
      WriteLn('Book not found.')
    else
      WriteLn('Book is not available for borrowing.');
  end;

  procedure TLibrary.ReturnBook(ISBN: string);
  var
    Index: integer;
  begin
    Index := FindBook(ISBN);
    if (Index <> -1) and not FBooks[Index].Available then
    begin
      FBooks[Index].Available := True;
      WriteLn('Book returned successfully.');
    end
    else if Index = -1 then
      WriteLn('Book not found.')
    else
      WriteLn('Book is already available.');
  end;

  procedure TLibrary.DisplayBooks;
  var
    I: integer;
  begin
    WriteLn('Library Books:');
    for I := 0 to FCount - 1 do
    begin
      WriteLn('Title: ', FBooks[I].Title);
      WriteLn('Author: ', FBooks[I].Author);
      WriteLn('ISBN: ', FBooks[I].ISBN);
      if FBooks[I].Available then
        WriteLn('Available: Yes')
      else
        WriteLn('Available: No');
      WriteLn;
    end;
  end;

  function TLibrary.GetBookCount: integer;
  begin
    Result := FCount;
  end;

var
  LibraryObj: TLibrary;
  Book: TBook;
  Choice: integer;
  ISBN: string;
begin
  LibraryObj := TLibrary.Create(100);

  repeat
    WriteLn('Library Management System');
    WriteLn('1. Add Book');
    WriteLn('2. Remove Book');
    WriteLn('3. Borrow Book');
    WriteLn('4. Return Book');
    WriteLn('5. Display Books');
    WriteLn('6. Exit');
    Write('Enter your choice: ');
    ReadLn(Choice);

    case Choice of
      1:
      begin
        Write('Enter book title: ');
        ReadLn(Book.Title);
        Write('Enter book author: ');
        ReadLn(Book.Author);
        Write('Enter book ISBN: ');
        ReadLn(Book.ISBN);
        Book.Available := True;
        LibraryObj.AddBook(Book);
      end;
      2:
      begin
        Write('Enter book ISBN: ');
        ReadLn(ISBN);
        LibraryObj.RemoveBook(ISBN);
      end;
      3:
      begin
        Write('Enter book ISBN: ');
        ReadLn(ISBN);
        LibraryObj.BorrowBook(ISBN);
      end;
      4:
      begin
        Write('Enter book ISBN: ');
        ReadLn(ISBN);
        LibraryObj.ReturnBook(ISBN);
      end;
      5:
        LibraryObj.DisplayBooks;
      6:
        WriteLn('Exiting the program.');
      else
        WriteLn('Invalid choice. Please try again.');
    end;

    WriteLn;
  until Choice = 6;

  WriteLn('Total books in the library: ', LibraryObj.GetBookCount);

  ReadLn;
end.
