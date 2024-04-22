unit AccountUnit;

{$MODE OBJFPC}
{$M+}
{$modeSwitch exceptions+}


interface

uses
  TransactionUnit;

type
  TAccount = class
  private
    FAccountHolder: string;
    FAccountNumber: string;
    FBalance: double;
  public
    constructor Create(AccountHolder: string; AccountNumber: string; Balance: double);
    function GetAccountHolder: string;
    function GetAccountNumber: string;
    function GetBalance: double;
    procedure Deposit(Amount: double);
    procedure Withdraw(Amount: double);
  end;

  TAccountList = class
  private
    FAccounts: array of TAccount;
    FCount: integer;
  public
    constructor Create;
    procedure AddAccount(Account: TAccount);
    function GetAccount(Index: integer): TAccount;
    function GetCount: integer;
    procedure ProcessTransaction(Transaction: TTransaction);
  end;

implementation

uses
  SysUtils;

{ TAccount }

constructor TAccount.Create(AccountHolder: string; AccountNumber: string;
  Balance: double);
begin
  FAccountHolder := AccountHolder;
  FAccountNumber := AccountNumber;
  FBalance := Balance;
end;

function TAccount.GetAccountHolder: string;
begin
  Result := FAccountHolder;
end;

function TAccount.GetAccountNumber: string;
begin
  Result := FAccountNumber;
end;

function TAccount.GetBalance: double;
begin
  Result := FBalance;
end;

procedure TAccount.Deposit(Amount: double);
begin
  FBalance := FBalance + Amount;
end;

procedure TAccount.Withdraw(Amount: double);
begin
  if Amount <= FBalance then
    FBalance := FBalance - Amount
  else
    raise Exception.Create('Insufficient funds.');
end;

{ TAccountList }

constructor TAccountList.Create;
begin
  FCount := 0;
end;

procedure TAccountList.AddAccount(Account: TAccount);
begin
  SetLength(FAccounts, FCount + 1);
  FAccounts[FCount] := Account;
  Inc(FCount);
end;

function TAccountList.GetAccount(Index: integer): TAccount;
begin
  Result := FAccounts[Index];
end;

function TAccountList.GetCount: integer;
begin
  Result := FCount;
end;

procedure TAccountList.ProcessTransaction(Transaction: TTransaction);
var
  I: integer;
  Account: TAccount;
begin
  for I := 0 to FCount - 1 do
  begin
    Account := FAccounts[I];
    if Account.GetAccountNumber = Transaction.GetAccountNumber then
    begin
      if Transaction.GetTransactionType = 'Deposit' then
        Account.Deposit(Transaction.GetAmount)
      else if Transaction.GetTransactionType = 'Withdrawal' then
        Account.Withdraw(Transaction.GetAmount);
      Break;
    end;
  end;
end;

end.
