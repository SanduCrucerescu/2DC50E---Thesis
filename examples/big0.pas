unit CgApplogUnit;
(***************************************************************************************************
 Copyright (c) 2013 Danfoss

 File Description: ?

 History
 Vers:  Date:      Name:  Change ID:     Comment:
 -----  -------    -----  -------------  ----------------------------------------------------------
 5.0.3  20Apr2010  ML     F00099         Added UniversalUnit in uses

 @Version   5.1.5 2011.06.12   tolsson   F00100:A101   XE upgrade

 @Version 7.0.1  2013.05.02 mpersson  P100002692     only add applog nodes to the list of applog nodes
 @Version 7.0.1  2013.05.22 mpersson  P1dev00002195  restructure the TApplogMemoryManager class hierarchy
 @Version 7.0.6  2013.11.29 mpersson  P100002860     get the applog allocation order from the p1x instead of the p1xt

 @Version 7.1.1 2014.01.22 tolsson  F00271         Standardize on lower case file extensions

 @Version 7.2.2 2015.04.23 mpersson P100003322       generate error when application log size is zero

 @Version 11.0.7 2018.11.15 tolsson P1dev00004277    Fix issue with encrypted SSD files

 @Version 11.1.3 2019.03.18 dandersson    M00139     FixInsight
 ****************************************************************************************************)

interface

uses
  Classes,
  XMLDoc,
  XMLIntf,

  //Universal Units: (M00062)
  UniversalPlatformUnit,
  UniversalTypesUnit,
  UniversalConstantsUnit,
  UniversalUnit;

type
  TWriteCodeLineProcedure = procedure(const s : string);

  TApplogMemoryManager = class
  strict private
    procedure CheckAndPrepareAllocation(const PortName : string; AFileSize : Integer; out PortIndex, NumberOfBlocks : Integer);
  protected
    //strict private
    FMultipleApplogAllowed  : Boolean;
    FWriteCodeLineProcedure : TWriteCodeLineProcedure;

    FPortNames      : array of string;
    FPortSizes      : array of Integer;
    FPortBlockSizes : array of Integer;
    FBlockNumbers   : array of Integer;
    FPortInUse      : array of Boolean;
    procedure WriteLine(const Line : string); virtual; abstract;
    procedure Allocate(const PortName : string; FileSize : Integer);
  public
    constructor Create(XMLDoc : IXmlDocument; WriteProc : TWriteCodeLineProcedure);
    destructor Destroy; override;
  end;

  //This is the original TApplogMemoryManager
  TApplog2007MemoryManager = class(TApplogMemoryManager)
  protected
    procedure WriteLine(const Line : string); override;
  public
    constructor Create(XMLDoc : IXmlDocument; WriteProc : TWriteCodeLineProcedure);
    destructor Destroy; override;

    procedure AllocateMemory(const PortName : string; FileSize : Integer); virtual;
  end;

  TApplog2012MemoryManager = class(TApplogMemoryManager)
  protected
    FApplogAreas    : StringsDictionary;
    FCurrentStrings : TStrings;
    procedure AllocateMemoryForAllApplogDefinitions(const P1xtName : string);
    procedure WriteLine(const Line : string); override;
  public
    constructor Create(XMLDoc : IXmlDocument; WriteProc : TWriteCodeLineProcedure; const P1xtName : string);
    destructor Destroy; override;

    procedure WriteAllocation(const ApplogAreaId : string);
  end;

  TApplogSysData = class
  strict private
    FPortNames      : array of string;
    FPortBlockSizes : array of Integer;
  public
    constructor Create(XMLDoc : IXmlDocument);
    destructor Destroy; override;

    function GetBlockSize(const PortName : string) : Integer;
  end;

function GetApplogMemoryManager : TApplogMemoryManager;

function GetApplogSysData : TApplogSysData;

implementation

uses

  //P1dev00004277
  CompileChainUtilityUnit,

  SysUtils,
  Generics.Collections,
  ErrorFuncUnit,
  CgMainUnit,
  CgOutPrtUnit,
  P1xtUnit,
  StringUnit,
  ExrUnit,
  P1xUnit;

var
  ApplogMemoryManager : TApplogMemoryManager;
  ApplogSysData       : TApplogSysData;

function GetApplogSysData : TApplogSysData;
var
  XMLDoc : IXmlDocument;
begin
  if not Assigned(ApplogSysData) then begin
    XMLDoc := TXmlDocument.Create(nil);
    XMLDoc.LoadFromFile(CgSysFileXmlName);
    ApplogSysData := TApplogSysData.Create(XMLDoc);
  end;
  Result := ApplogSysData;
end;

function GetApplogMemoryManager : TApplogMemoryManager;
var
  XMLDoc : IXmlDocument;
begin
  if not Assigned(ApplogMemoryManager) then begin
    XMLDoc := TXmlDocument.Create(nil);
    XMLDoc.LoadFromFile(CgSysFileXmlName);

    if (HasApplogEditor2007) then begin
      ApplogMemoryManager := TApplog2007MemoryManager.Create(XMLDoc, OutPrtWr);
    end else if (HasApplogEditor2012) then begin
      ApplogMemoryManager := TApplog2012MemoryManager.Create(XMLDoc, OutPrtWr, ChangeFileExt(ProjectFileName, P1xtFileExtension));
    end else begin
      raise EP1Error.Create(_E_587_INTERNAL_UNEXPECTEDLY_REACHED_ELSE_IN_ps, ['GetApplogMemoryManager']);
    end;
  end;
  Result := ApplogMemoryManager;
end;

{TApplogMemoryManager}

{*------------------------------------------------------------------------------
 Write memory allocation code for one applog area

 What:
 Write memory allocation code for one applog area using the write function
 passed in the constructor

 @param PortName   Applog port
 @param FileSize   File size
 ------------------------------------------------------------------------------*}
procedure TApplogMemoryManager.Allocate(const PortName : string; FileSize : Integer);
var
  i                         : Integer;
  PortIndex, NumberOfBlocks : Integer;
begin
  CheckAndPrepareAllocation(PortName, FileSize, PortIndex, NumberOfBlocks);

  //Write the code
  for i := 1 to NumberOfBlocks do begin
    if (i <> 1) then begin
      WriteLine(',_CREATE_APPL_FILE_SECTOR((_ApplogStartSector+' + IntToStr(FBlockNumbers[PortIndex]) + '))');
    end else begin
      WriteLine('_CREATE_APPL_FILE_SECTOR((_ApplogStartSector+' + IntToStr(FBlockNumbers[PortIndex]) + '))');
    end;

    FPortInUse[PortIndex] := true;
    Inc(FBlockNumbers[PortIndex]);
  end;
end;

procedure TApplogMemoryManager.CheckAndPrepareAllocation(const PortName : string; AFileSize : Integer;
  out PortIndex, NumberOfBlocks : Integer);
var
  i : Integer;
begin
  //Find the port
  PortIndex := -1;

  for i := low(FPortNames) to high(FPortNames) do begin
    if (FPortNames[i] = PortName) then begin
      PortIndex := i;
      Break;
    end;
  end;

  if (PortIndex < 0) then begin
    raise EP1Error.Create(_E_315_APPLICATION_LOG_PORT_NOT_DEFINED_ps, [PortName]);
  end;

  //Check if multiple applog is used and if it is allowed
  if (FPortInUse[PortIndex] and (not FMultipleApplogAllowed)) then begin
    raise EP1Error.Create(_E_314_MAXIMUM_NUMBER_OF_APPLICATION_LOG_FILES_ps_EXCEEDED_ps, ['(1)', '']);
  end;

  //Check available space
  if ((FBlockNumbers[PortIndex] * FPortBlockSizes[PortIndex] + AFileSize) > FPortSizes[PortIndex]) then begin
    raise EP1Error.Create(_E_339_INTERNAL_OUT_OF_APPLOG_MEMORY, []);
  end;

  //Calculate number of blocks to use
  NumberOfBlocks := AFileSize div FPortBlockSizes[PortIndex];

  if ((AFileSize mod FPortBlockSizes[PortIndex]) <> 0) then begin
    raise EP1Error.Create(_E_340_INTERNAL_NOT_AN_INTEGER_AMOUNT_OF_APPLOG_BLOCKS, []);
  end;
end;

{*------------------------------------------------------------------------------
 Create a new instance

 What:
 Extract sys xml data about applog and initiate data structures

 @param XMLDoc   Sys xml document
 @param WriteProc   Procedure that writes text lines to file
 ------------------------------------------------------------------------------*}
constructor TApplogMemoryManager.Create(XMLDoc : IXmlDocument; WriteProc : TWriteCodeLineProcedure);
var
  PortNodes                                  : IXMLNodeCollection;
  PortNode, ApplogMemoryNode, FileFormatNode : IXmlNode;
  FFValue                                    : Integer;
  FFText, ApplogMemoryKeyword                : string;
  i                                          : Integer;
  ApplogNode                                 : IXmlNode;
begin
  FWriteCodeLineProcedure := WriteProc;
  SetLength(FPortNames, 0);
  SetLength(FPortSizes, 0);
  SetLength(FPortBlockSizes, 0);
  SetLength(FBlockNumbers, 0);
  SetLength(FPortInUse, 0);
  FileFormatNode := XMLDoc.DocumentElement.ChildNodes.FindNode('FileFormat');
  FFText := XmlVal(FileFormatNode);
  FFText := Copy(FFText, 1, 3);
  FFValue := StrToInt(FFText);

  if (FFValue < 4) then begin
    ApplogMemoryKeyword := 'FileDrive';
    FMultipleApplogAllowed := false;
  end else begin
    ApplogMemoryKeyword := 'DataDrive';
    FMultipleApplogAllowed := true;
  end;

  PortNodes := GetXmlCollection(XMLDoc.DocumentElement, 'Ports');

  for i := 0 to PortNodes.Count - 1 do begin
    PortNode := PortNodes[i];
    ApplogMemoryNode := PortNode.ChildNodes.FindNode(ApplogMemoryKeyword);
    if (Assigned(ApplogMemoryNode)) then begin

      //add port only if it is an applog port
      if (XmlChildFindPath(ApplogMemoryNode, ['HWImplementation', 'Applog'], ApplogNode) or (ApplogMemoryKeyword = 'FileDrive'))
      then begin

        SetLength(FPortNames, Length(FPortNames) + 1);
        SetLength(FPortSizes, Length(FPortSizes) + 1);
        SetLength(FPortBlockSizes, Length(FPortBlockSizes) + 1);
        SetLength(FBlockNumbers, Length(FBlockNumbers) + 1);
        SetLength(FPortInUse, Length(FPortInUse) + 1);

        FPortNames[high(FPortNames)] := XmlSubNodeVal(ApplogMemoryNode, 'Name');
        FPortSizes[high(FPortSizes)] := StrToInt(Trim(XmlSubNodeVal(ApplogMemoryNode, 'Size')));
        FPortBlockSizes[high(FPortBlockSizes)] := StrToInt(Trim(XmlSubNodeVal(ApplogMemoryNode, 'BlockSize')));

        FBlockNumbers[high(FBlockNumbers)] := 0;
        FPortInUse[high(FPortInUse)] := false;
      end;
    end;
  end;
end;

destructor TApplogMemoryManager.Destroy;
var
  i : Integer;
begin
  for i := low(FPortNames) to high(FPortNames) do begin
    Finalize(FPortNames[i]);
  end;

  Finalize(FPortNames);
  Finalize(FPortSizes);
  Finalize(FPortBlockSizes);
  Finalize(FBlockNumbers);
  Finalize(FPortInUse);

  inherited;
end;

{TApplogSysData}

constructor TApplogSysData.Create(XMLDoc : IXmlDocument);
var
  PortNodes                                  : IXMLNodeCollection;
  PortNode, ApplogMemoryNode, FileFormatNode : IXmlNode;
  FFValue                                    : Integer;
  FFText, ApplogMemoryKeyword                : string;
  i                                          : Integer;
begin
  SetLength(FPortNames, 0);
  SetLength(FPortBlockSizes, 0);
  FileFormatNode := XMLDoc.DocumentElement.ChildNodes.FindNode('FileFormat');
  FFText := XmlVal(FileFormatNode);

  FFText := Copy(FFText, 1, 3);
  FFValue := StrToInt(FFText);

  if (FFValue < 4) then begin
    ApplogMemoryKeyword := 'FileDrive';
  end else begin
    ApplogMemoryKeyword := 'DataDrive';
  end;
  PortNodes := GetXmlCollection(XMLDoc.DocumentElement, 'Ports');

  for i := 0 to PortNodes.Count - 1 do begin
    PortNode := PortNodes[i];
    ApplogMemoryNode := PortNode.ChildNodes.FindNode(ApplogMemoryKeyword);
    if (Assigned(ApplogMemoryNode)) then begin
      SetLength(FPortNames, Length(FPortNames) + 1);
      SetLength(FPortBlockSizes, Length(FPortBlockSizes) + 1);
      FPortNames[high(FPortNames)] := XmlSubNodeVal(ApplogMemoryNode, 'Name');
      FPortBlockSizes[high(FPortBlockSizes)] := StrToInt(Trim(XmlSubNodeVal(ApplogMemoryNode, 'BlockSize')));
    end;
  end;
end;

destructor TApplogSysData.Destroy;
begin
  inherited;
end;

function TApplogSysData.GetBlockSize(const PortName : string) : Integer;
var
  i : Integer;
begin
  for i := low(FPortNames) to high(FPortNames) do begin
    if (FPortNames[i] = PortName) then begin
      Result := FPortBlockSizes[i];
      Exit;
    end;
  end;

  raise EP1Error.Create(_E_315_APPLICATION_LOG_PORT_NOT_DEFINED_ps, [PortName]);
end;

{TApplog2012MemoryManager}
procedure TApplog2012MemoryManager.AllocateMemoryForAllApplogDefinitions(const P1xtName : string);
var
  P1xtFile                        : TP1xtFile;
  SsdName                         : string;
  XMLDoc                          : IXmlDocument;
  SizeNode, PortNameNode, UidNode : IXmlNode;
  Size                            : Int64;
  FileDek                         : TFileDek;

  //P100003322
  DescriptionNode : IXmlNode;

  //P1dev00004277
  SsdFileName        : string;
  SsdFileIsEncrypted : Boolean;

begin
  UniversalDoNothingWithString(P1xtName); //M00139
  P1xtFile := nil;
  XMLDoc := nil;
  try
    for FileDek in P1x.FAllFilesHash.Values do begin

      //F00271       if (Eq(FileDek.FileExt, 'SSD')) then begin
      if FileDek.FileExtEnum in [fdeeSsd] then begin

        SsdName := FileDek.FileName;

        //P1dev00004277        XMLDoc := TXmlDocument.Create(nil);
        //XMLDoc.LoadFromFile(ChangeFileExt(SsdName, SsdFileExtension));
        SsdFileName := ChangeFileExt(SsdName, SsdFileExtension);
        XMLDoc := LoadSsdFileAndCheckVersion(SsdFileName, SsdFileIsEncrypted);

        if (XmlChildFind(XMLDoc.DocumentElement, SsdElSize, SizeNode)) then begin
          Size := StrToInt64(SizeNode.NodeValue);
          if (not XmlChildFind(XMLDoc.DocumentElement, SsdElPortName, PortNameNode)) then begin
            raise EP1Error.Create(_E_70_INTERNAL_INVALID_FORMAT_IN_FILE_ps, [SsdName]);
          end;

          if (not XmlChildFind(XMLDoc.DocumentElement, SsdElGuid, UidNode)) then begin
            raise EP1Error.Create(_E_70_INTERNAL_INVALID_FORMAT_IN_FILE_ps, [SsdName]);
          end;

          //P100003322
          if (0 = Size) then begin
            if (not XmlChildFind(XMLDoc.DocumentElement, SsdElDescription, DescriptionNode)) then begin
              raise EP1Error.Create(_E_70_INTERNAL_INVALID_FORMAT_IN_FILE_ps, [SsdName]);
            end;
            raise EP1Error.Create(_E_639_INCORRECT_SIZE_IN_ps_APPLICATION_LOG_SIZE_MUST_BE_GREATER_THAN_ZERO,
              [DescriptionNode.NodeValue]);
          end;

          FCurrentStrings := TStringList.Create;
          Allocate(PortNameNode.NodeValue, Size);
          FApplogAreas.Add(UidNode.NodeValue, FCurrentStrings);
        end;
      end;
    end;
  finally
    P1xtFile.Free;
    XMLDoc := nil;
    FCurrentStrings := nil;
  end;
end;

{*------------------------------------------------------------------------------
 Create a new instance

 What:
 Call the superclass' constructor and initiate the hash table of applog definitions.
 Allocate memory for all definitions and store the allocations in the hash table.

 @param XMLDoc   Sys xml document
 @param WriteProc   Procedure that writes text lines to file
 @param P1xtName   The name of the project's p1xt file
 ------------------------------------------------------------------------------*}
constructor TApplog2012MemoryManager.Create(XMLDoc : IXmlDocument; WriteProc : TWriteCodeLineProcedure; const P1xtName : string);
begin
  inherited Create(XMLDoc, WriteProc);

  FCurrentStrings := nil;
  FApplogAreas := StringsDictionary.Create([doOwnsValues]);
  AllocateMemoryForAllApplogDefinitions(P1xtName);
end;

destructor TApplog2012MemoryManager.Destroy;
begin
  FApplogAreas.Free;

  inherited;
end;

{*------------------------------------------------------------------------------
 Write the allocation of an applog area to file

 @param ApplogAreaId   Applog area GUID
 ------------------------------------------------------------------------------*}
procedure TApplog2012MemoryManager.WriteAllocation(const ApplogAreaId : string);
var
  Declarations : TStrings;
  Line         : string;
begin
  if (not FApplogAreas.TryGetValue(ApplogAreaId, Declarations)) then begin
    raise EP1Error.Create(_E_569_INTERNAL_UNEXPECTED_BEHAVIOR_IN_FUNCTION_ps_ps, ['TApplog2012MemoryManager.WriteAllocation',
      'Applog area ' + ApplogAreaId + ' was not found']);
  end;

  for Line in Declarations do begin
    FWriteCodeLineProcedure(Line);
  end;
end;

procedure TApplog2012MemoryManager.WriteLine(const Line : string);
begin
  if (Assigned(FCurrentStrings)) then begin
    FCurrentStrings.Add(Line);
  end;
end;

{TApplog2007MemoryManager}

{*------------------------------------------------------------------------------
 Write memory allocation for an applog area

 What:
 Call the method in the superclass

 @param PortName   Port name
 @param FileSize   File size
 ------------------------------------------------------------------------------*}
procedure TApplog2007MemoryManager.AllocateMemory(const PortName : string; FileSize : Integer);
begin
  Allocate(PortName, FileSize);
end;

{*------------------------------------------------------------------------------
 Create a new instance

 @param XMLDoc   Sys xml document
 @param WriteProc   Procedure that writes text lines to file
 ------------------------------------------------------------------------------*}
constructor TApplog2007MemoryManager.Create(XMLDoc : IXmlDocument; WriteProc : TWriteCodeLineProcedure);
begin
  UniversalDoNothingWithTypes(@XMLDoc);    //M00139
  UniversalDoNothingWithTypes(@WriteProc); //M00139
  inherited;
end;

destructor TApplog2007MemoryManager.Destroy;
begin

  inherited;
end;

procedure TApplog2007MemoryManager.WriteLine(const Line : string);
begin
  FWriteCodeLineProcedure(Line);
end;

initialization

ApplogMemoryManager := nil;
ApplogSysData := nil;

end.
