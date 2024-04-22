unit ReportUnit;

{$MODE OBJFPC}
{$M+}

interface

uses
  AccountUnit;

procedure GenerateReport(AccountList: TAccountList);

implementation

uses
  SysUtils;

procedure GenerateReport(AccountList: TAccountList);
var
  I: integer;
  Account: TAccount;
  TotalBalance: double;
begin
  TotalBalance := 0;
  WriteLn('Account Report:');
  for I := 0 to AccountList.GetCount - 1 do
  begin
    Account := AccountList.GetAccount(I);
    WriteLn('Account Number: ', Account.GetAccountNumber);
    WriteLn('Account Holder: ', Account.GetAccountHolder);
    WriteLn('Balance: ', FormatFloat('$#,##0.00', Account.GetBalance));
    WriteLn;
    TotalBalance := TotalBalance + Account.GetBalance;
  end;

  WriteLn('Total Balance: ', FormatFloat('$#,##0.00', TotalBalance));
end;

end.
