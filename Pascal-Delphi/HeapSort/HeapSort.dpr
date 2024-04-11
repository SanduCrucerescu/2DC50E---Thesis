(*Floyd's Algorithm*)

program HeapSort;

{$modeSwitch class+}
{$modeSwitch exceptions+}

uses
  SysUtils,
  Classes;

type
  EtdTListException = class(Exception);
  TtdCompareFunc = function(aData1, aData2: pointer): integer;

  procedure TDValidateListRange(aList: TList; aStart, aEnd: integer; aMessage: string);
  begin
    if (aList = nil) then
      raise EtdTListException.Create(Format('List is nil:: %s', [aMessage]));
    if (aStart < 0) or (aStart >= aList.Count) or (aEnd < 0) or
      (aEnd >= aList.Count) or (aStart > aEnd) then
      raise EtdTListException.Create(Format('Invalid range: %d-%d: %s',
        [aStart, aEnd, aMessage]));
  end;

  procedure HSTrickleDown(aList: PPointerList; aFromInx: integer;
    aCount: integer; aCompare: TtdCompareFunc);
  var
    Item: pointer;
    ChildInx: integer;
    ParentInx: integer;
  begin
{first do a simple trickle down continually replacing parent with
larger child until we reach the bottom level of the heap}
    Item := aList^[aFromInx];
    ChildInx := (aFromInx * 2) + 1;
    while (ChildInx < aCount) do
    begin
      if (succ(ChildInx) < aCount) and
        (aCompare(aList^[ChildInx], aList^[succ(ChildInx)]) < 0) then
        Inc(ChildInx);
      aList^[aFromInx] := aList^[ChildInx];
      aFromInx := ChildInx;
      ChildInx := (aFromInx * 2) + 1;
    end;
    {now bubble up from where we ended up}
    ParentInx := (aFromInx - 1) div 2;
    while (aFromInx > 0) and (aCompare(Item, aList^[ParentInx]) > 0) do
    begin
      aList^[aFromInx] := aList^[ParentInx];
      aFromInx := ParentInx;
      ParentInx := (aFromInx - 1) div 2;
    end;
    {save the item where we ended up after the bubble up}
    aList^[aFromInx] := Item;
  end;

  procedure HSTrickleDownStd(aList: PPointerList; aFromInx: integer;
    aCount: integer; aCompare: TtdCompareFunc);
  var
    Item: pointer;
    ChildInx: integer;
  begin
    Item := aList^[aFromInx];
    ChildInx := (aFromInx * 2) + 1;
    while (ChildInx < aCount) do
    begin
      if (succ(ChildInx) < aCount) and
        (aCompare(aList^[ChildInx], aList^[succ(ChildInx)]) < 0) then
        Inc(ChildInx);
      if aCompare(Item, aList^[ChildInx]) >= 0 then
        Break;
      aList^[aFromInx] := aList^[ChildInx];
      aFromInx := ChildInx;
      ChildInx := (aFromInx * 2) + 1;
    end;
    aList^[aFromInx] := Item;
  end;

  procedure TDHeapSort(aList: TList; aFirst: integer; aLast: integer;
    aCompare: TtdCompareFunc);
  var
    ItemCount: integer;
    Inx: integer;
    Temp: pointer;
  begin
    TDValidateListRange(aList, aFirst, aLast, 'TDHeapSort');
    {convert the list into a heap-Floyd’s Algorithm}
    ItemCount := aLast - aFirst + 1;
    for Inx := pred(ItemCount div 2) downto 0 do
      HSTrickleDownStd(@aList.List^[aFirst], Inx, ItemCount, aCompare);
{now remove the items one at a time from the heap, placing them at
the end of the array}
    for Inx := pred(ItemCount) downto 0 do
    begin
      Temp := aList.List^[aFirst];
      aList.List^[aFirst] := aList.List^[aFirst + Inx];
      aList.List^[aFirst + Inx] := Temp;
      HSTrickleDown(@aList.List^[aFirst], 0, Inx, aCompare);
    end;
  end;

  function TDCompareLongint(aData1, aData2: pointer): integer;
  var
    L1: longint absolute aData1;
    L2: longint absolute aData2;
  begin
    if (L1 < L2) then
      TDCompareLongint := -1
    else if (L1 = L2) then
      TDCompareLongint := 0
    else
      TDCompareLongint := 1;
  end;

var
  arr: TList;
  i: integer;
begin
  arr := TList.Create;

  try
    arr.Add(Pointer(5));
    arr.Add(Pointer(876));
    arr.Add(Pointer(234));
    arr.Add(Pointer(9));
    arr.Add(Pointer(12));
    arr.Add(Pointer(44));
    arr.Add(Pointer(2));

    TDHeapSort(arr, 0, arr.Count - 1, TtdCompareFunc(@TDCompareLongint));

    for i := 0 to arr.Count - 1 do
      WriteLn(integer(arr[i]));
  finally
    arr.Free;
  end;
end.
