{$MODE OBJFPC}
{$H+}

program pointers;

uses
  SysUtils;

type
  PContact = ^TContact;

  TContact = record
    Name: string;
    Email: string;
    PhoneNumber: string;
  end;

  PAddressBook = ^TAddressBook;

  TAddressBook = record
    Contacts: array of PContact;
    Count: integer;
  end;

  procedure InitAddressBook(var AddressBook: TAddressBook);
  begin
    SetLength(AddressBook.Contacts, 0);
    AddressBook.Count := 0;
  end;

  procedure FreeAddressBook(var AddressBook: TAddressBook);
  var
    I: integer;
  begin
    for I := 0 to AddressBook.Count - 1 do
      Dispose(AddressBook.Contacts[I]);
    AddressBook.Contacts := nil;
    AddressBook.Count := 0;
  end;

  procedure AddContact(var AddressBook: TAddressBook;
  const Name, Email, PhoneNumber: string);
  var
    NewContact: PContact;
  begin
    New(NewContact);
    NewContact^.Name := Name;
    NewContact^.Email := Email;
    NewContact^.PhoneNumber := PhoneNumber;
    SetLength(AddressBook.Contacts, AddressBook.Count + 1);
    AddressBook.Contacts[AddressBook.Count] := NewContact;
    Inc(AddressBook.Count);
  end;

  function SearchContact(const AddressBook: TAddressBook; const Name: string): PContact;
  var
    I: integer;
  begin
    Result := nil;
    for I := 0 to AddressBook.Count - 1 do
      if AddressBook.Contacts[I]^.Name = Name then
      begin
        Result := AddressBook.Contacts[I];
        Break;
      end;
  end;

  procedure RemoveContact(var AddressBook: TAddressBook; const Name: string);
  var
    I, J: integer;
    Contact: PContact;
  begin
    Contact := SearchContact(AddressBook, Name);
    if Contact <> nil then
    begin
      for I := 0 to AddressBook.Count - 1 do
        if AddressBook.Contacts[I] = Contact then
        begin
          Dispose(AddressBook.Contacts[I]);
          for J := I to AddressBook.Count - 2 do
            AddressBook.Contacts[J] := AddressBook.Contacts[J + 1];
          Dec(AddressBook.Count);
          SetLength(AddressBook.Contacts, AddressBook.Count);
          Break;
        end;
    end;
  end;

  procedure PrintContacts(const AddressBook: TAddressBook);
  var
    I: integer;
  begin
    for I := 0 to AddressBook.Count - 1 do
    begin
      Writeln('Name: ', AddressBook.Contacts[I]^.Name);
      Writeln('Email: ', AddressBook.Contacts[I]^.Email);
      Writeln('Phone Number: ', AddressBook.Contacts[I]^.PhoneNumber);
      Writeln;
    end;
  end;

var
  AddressBook: TAddressBook;
  Name, Email, PhoneNumber, Command: string;

begin
  InitAddressBook(AddressBook);
  repeat
    Write('Enter command (add/search/remove/print/exit): ');
    Readln(Command);
    case Command of
      'add':
      begin
        Write('Enter name: ');
        Readln(Name);
        Write('Enter email: ');
        Readln(Email);
        Write('Enter phone number: ');
        Readln(PhoneNumber);
        AddContact(AddressBook, Name, Email, PhoneNumber);
      end;
      'search':
      begin
        Write('Enter name to search: ');
        Readln(Name);
        if SearchContact(AddressBook, Name) <> nil then
        begin
          Writeln('Name: ', SearchContact(AddressBook, Name)^.Name);
          Writeln('Email: ', SearchContact(AddressBook, Name)^.Email);
          Writeln('Phone Number: ', SearchContact(AddressBook, Name)^.PhoneNumber);
        end
        else
          Writeln('Contact not found');
      end;
      'remove':
      begin
        Write('Enter name to remove: ');
        Readln(Name);
        RemoveContact(AddressBook, Name);
      end;
      'print':
        PrintContacts(AddressBook);
    end;
  until Command = 'exit';
  FreeAddressBook(AddressBook);
end.
