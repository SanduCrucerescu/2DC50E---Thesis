unit TransactionUnit;

{$MODE OBJFPC}
{$M+}

interface

type
  TTransaction = class
  private
    FAccountNumber: string;
    FAmount: double;
    FTransactionType: string;
  public
    constructor Create(AccountNumber: string; Amount: double; TransactionType: string);
    function GetAccountNumber: string;
    function GetAmount: double;
    function GetTransactionType: string;
  end;

implementation

{ TTransaction }

constructor TTransaction.Create(AccountNumber: string; Amount: double;
  TransactionType: string);
begin
  FAccountNumber := AccountNumber;
  FAmount := Amount;
  FTransactionType := TransactionType;
end;

function TTransaction.GetAccountNumber: string;
begin
  Result := FAccountNumber;
end;

function TTransaction.GetAmount: double;
begin
  Result := FAmount;
end;

function TTransaction.GetTransactionType: string;
begin
  Result := FTransactionType;
end;

end.
