program Classes;

{$MODE OBJFPC}
{$M+}

uses
  crt,
  SysUtils;

type
  ShapeType = (Triangle, Square, Rectangle, Cube);
  TrinagleType = (Acute, Optuse, Right, Isosceles, Equilateral, Scalene);

  (* Main Class *)
  Shape = class
  protected
    Width, Height: double;
    SType: ShapeType;

  public
    constructor Create(W, H: double; T: ShapeType);

    procedure SetWidth(W: double);
    function GetWidth(): double;

    procedure SetHeight(H: double);
    function GetHeight(): double;

    procedure SetType(T: ShapeType);
    function GetType(): ShapeType;

    procedure Display(); virtual;
  end;

  (* Derived Class *)
  TriangleObj = class(Shape)
  private
    TType: TrinagleType;

  public
    constructor Create(T: ShapeType); overload;
    constructor Create(T: ShapeType; TT: TrinagleType; W, H: double); overload;

    procedure SetTType(TT: TrinagleType);
    function GetTType(): TrinagleType;

    procedure Display(); override;
  end;

  (* Main Class Implementations *)

  constructor Shape.Create(W, H: double; T: ShapeType);
  begin
    Width := W;
    Height := H;
    SType := T;
  end;

  procedure Shape.SetHeight(H: double);
  begin
    Height := H;
  end;

  function Shape.GetHeight(): double;
  begin
    GetHeight := Height;
  end;

  procedure Shape.SetWidth(W: double);
  begin
    Width := W;
  end;

  function Shape.GetWidth(): double;
  begin
    GetWidth := Width;
  end;

  procedure Shape.SetType(T: ShapeType);
  begin
    SType := T;
  end;

  function Shape.GetType(): ShapeType;
  begin
    GetType := SType;
  end;


  procedure Shape.Display();
  begin
    WriteLn(Format('Shape type: %s', [SType]));
    WriteLn(Format('Shape width: %s', [Width]));
    WriteLn(Format('Shape height: %s', [Height]));
  end;

  (* Derived Class Implementation *)

  constructor TriangleObj.Create(T: ShapeType);
  begin
    inherited Create(0.0, 0.0, T);
  end;

  constructor TriangleObj.Create(T: ShapeType; TT: TrinagleType; W, H: double);
  begin
    inherited Create(W, H, T);
    TType := TT;
  end;

  procedure TriangleObj.SetTType(TT: TrinagleType);
  begin
    TType := TT;
  end;

  function TriangleObj.GetTType(): TrinagleType;
  begin
    GetTType := TType;
  end;

  procedure TriangleObj.Display();
  var
    TypeStr: string;
  begin
    WriteStr(TypeStr), TType);
    WriteLn(Format('Trinagle type: %s', [TypeStr]));
    WriteLn(Format('Shape width: %s', [FloatToStr(Width)]));
    WriteLn(Format('Shape height: %s', [FloatToStr(Height)]));
  end;

var
  TObj: TriangleObj;

begin
  TObj := TriangleObj.Create(Triangle, Optuse, 20.0, 35.9);
  TObj.Display;
end.
