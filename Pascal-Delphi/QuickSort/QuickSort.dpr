program QuickSort;

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


  procedure QSS(aList: TList; aFirst: integer; aLast: integer; aCompare: TtdCompareFunc);
  var
    L, R: integer;
    Pivot: pointer;
    Temp: pointer;
  begin
    {while there are at least two items to sort}
    while (aFirst < aLast) do
    begin
      {the pivot is the middle item}
      Pivot := aList.List^[(aFirst + aLast) div 2];
      {set indexes and partition}
      L := pred(aFirst);
      R := succ(aLast);
      while True do
      begin
        repeat
          Dec(R);
        until (aCompare(aList.List^[R], Pivot) <= 0);
        repeat
          Inc(L);
        until (aCompare(aList.List^[L], Pivot) >= 0);
        if (L >= R) then
          Break;
        Temp := aList.List^[L];
        aList.List^[L] := aList.List^[R];
        aList.List^[R] := Temp;
      end;
      {quicksort the first subfile}
      if (aFirst < R) then
        QSS(aList, aFirst, R, aCompare);
      {quicksort the second subfile - recursion removal}
      aFirst := succ(R);
    end;
  end;

  procedure TDQuickSortStd(aList: TList; aFirst: integer; aLast: integer;
    aCompare: TtdCompareFunc);
  begin
    TDValidateListRange(aList, aFirst, aLast, 'TDQuickSortStd');
    QSS(aList, aFirst, aLast, aCompare);
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

    TDQuickSortStd(arr, 0, arr.Count - 1, TtdCompareFunc(@TDCompareLongint));

    for i := 0 to arr.Count - 1 do
      WriteLn(integer(arr[i]));
  finally
    arr.Free;
  end;
end.
