program BankAccountSystem;

{$MODE OBJFPC}
{$M+}

uses
  SysUtils,
  AccountUnit,
  TransactionUnit,
  ReportUnit;

var
  AccountList: TAccountList;
  Choice: integer;
  AccountNumber: string;
  AccountHolder: string;
  InitialBalance: double;
  Amount: double;
  TransactionType: string;

  procedure ShowMenu;
  begin
    WriteLn('Bank Account Management System');
    WriteLn('1. Create Account');
    WriteLn('2. Deposit');
    WriteLn('3. Withdraw');
    WriteLn('4. Generate Report');
    WriteLn('5. Exit');
    Write('Enter your choice: ');
  end;

begin
  AccountList := TAccountList.Create;

  repeat
    ShowMenu;
    ReadLn(Choice);

    case Choice of
      1: // Create Account
      begin
        Write('Enter account holder name: ');
        ReadLn(AccountHolder);
        Write('Enter account number: ');
        ReadLn(AccountNumber);
        Write('Enter initial balance: ');
        ReadLn(InitialBalance);
        AccountList.AddAccount(TAccount.Create(AccountHolder,
          AccountNumber, InitialBalance));
        WriteLn('Account created successfully.');
      end;

      2: // Deposit
      begin
        Write('Enter account number: ');
        ReadLn(AccountNumber);
        Write('Enter amount to deposit: ');
        ReadLn(Amount);
        AccountList.ProcessTransaction(TTransaction.Create(AccountNumber,
          Amount, 'Deposit'));
        WriteLn('Deposit successful.');
      end;

      3: // Withdraw
      begin
        Write('Enter account number: ');
        ReadLn(AccountNumber);
        Write('Enter amount to withdraw: ');
        ReadLn(Amount);
        try
          AccountList.ProcessTransaction(TTransaction.Create(AccountNumber,
            Amount, 'Withdrawal'));
          WriteLn('Withdrawal successful.');
        except
          on E: Exception do
            WriteLn('Error: ', E.Message);
        end;
      end;

      4: // Generate Report
        GenerateReport(AccountList);

      5: // Exit
        WriteLn('Exiting the program.');

      else
        WriteLn('Invalid choice. Please try again.');
    end;

    WriteLn;
  until Choice = 5;

  ReadLn;
end.
