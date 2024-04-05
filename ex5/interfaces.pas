{$MODE OBJFPC}
{$H+}

program interfaces;

uses
  SysUtils;

type
  IProduct = interface
    function GetName: string;
    function GetPrice: double;
    property Name: string read GetName;
    property Price: double read GetPrice;
  end;

  TProduct = class(TInterfacedObject, IProduct)
  private
    FName: string;
    FPrice: double;
  public
    constructor Create(const AName: string; const APrice: double);
    function GetName: string;
    function GetPrice: double;
  end;

  TShoppingCart = class
  private
    FItems: array of IProduct;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddItem(const AProduct: IProduct);
    function GetTotalCost: double;
    property Count: integer read GetCount;
  end;

  { TProduct }

  constructor TProduct.Create(const AName: string; const APrice: double);
  begin
    FName := AName;
    FPrice := APrice;
  end;

  function TProduct.GetName: string;
  begin
    Result := FName;
  end;

  function TProduct.GetPrice: double;
  begin
    Result := FPrice;
  end;

  { TShoppingCart }

  constructor TShoppingCart.Create;
  begin
    SetLength(FItems, 0);
  end;

  destructor TShoppingCart.Destroy;
  var
    I: integer;
  begin
    for I := 0 to High(FItems) do
      FItems[I] := nil;
    inherited;
  end;

  function TShoppingCart.GetCount: integer;
  begin
    Result := Length(FItems);
  end;

  procedure TShoppingCart.AddItem(const AProduct: IProduct);
  begin
    SetLength(FItems, Length(FItems) + 1);
    FItems[High(FItems)] := AProduct;
  end;

  function TShoppingCart.GetTotalCost: double;
  var
    I: integer;
    TotalCost: double;
  begin
    TotalCost := 0;
    for I := 0 to High(FItems) do
      TotalCost := TotalCost + FItems[I].Price;
    Result := TotalCost;
  end;

var
  Cart: TShoppingCart;
  ProductName: string;
  ProductPrice: double;
  Choice: string;
  I: integer;

begin
  Cart := TShoppingCart.Create;
  try
    repeat
      Write('Enter product name (or "x" to finish): ');
      ReadLn(ProductName);

      if ProductName.ToLower <> 'x' then
      begin
        Write('Enter product price: ');
        ReadLn(ProductPrice);

        Cart.AddItem(TProduct.Create(ProductName, ProductPrice));
      end;

    until ProductName.ToLower = 'x';

    WriteLn('Shopping Cart Contents:');
    WriteLn('------------------------');
    for I := 0 to Cart.Count - 1 do
      WriteLn(Cart.FItems[I].Name + ': $' + Cart.FItems[I].Price.ToString);

    WriteLn('------------------------');
    WriteLn('Total Cost: $' + Cart.GetTotalCost.ToString);

    Write('Press Enter to exit...');
    ReadLn(Choice);
  finally
    Cart.Free;
  end;
end.
