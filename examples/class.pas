unit Class;

interface

type
    TMemoryStream = class(TCustomMemoryStream)
      private
        FCapacity: Longint;
        procedure SetCapacity(NewCapacity: Longint);
      protected
        function Realloc(var NewCapacity: Longint): Pointer; virtual;
        property Capacity: Longint read FCapacity write SetCapacity;
      public
        destructor Destroy; override;
        procedure Clear;
        procedure LoadFromStream(Stream: TStream);
        procedure LoadFromFile(const FileName: string);
        procedure SetSize(const NewSize: Int64); override;
        procedure SetSize(NewSize: Longint); override;
        function Write(const Buffer; Count: Longint): Longint; override;
        function Write(const Buffer: TBytes; Offset, Count: Longint): Longint; override;
      end; // deprecated 'Use TBytesStream';

implementation

end.