using System;
namespace BankAccountSystem4
{
    public class Account
    {
        private string accountHolder;
        private string accountNumber;
        private double balance;
        public Account(string accountHolder, string accountNumber, double balance)
        {
            this.accountHolder = accountHolder;
            this.accountNumber = accountNumber;
            this.balance = balance;
        }

        public string GetAccountHolder()
        {
            return accountHolder;
        }

        public string GetAccountNumber()
        {
            return accountNumber;
        }

        public double GetBalance()
        {
            return balance;
        }

        public void Deposit(double amount)
        {
            balance += amount;
        }

        public void Withdraw(double amount)
        {
            if (amount <= balance)
            {
                balance -= amount;
            }
            else
            {
                throw new Exception("Insufficient funds.");
            }
        }
    }

    public class AccountList
    {
        private Account[] accounts;
        private int count;

        public AccountList()
        {
            count = 0;
        }

        public void AddAccount(Account account)
        {
            Array.Resize(ref accounts, count + 1);
            accounts[count] = account;
            count++;
        }

        public Account GetAccount(int index)
        {
            return accounts[index];
        }

        public int GetCount()
        {
            return count;
        }

        public void ProcessTransaction(Transaction transaction)
        {
            for (int i = 0; i < count; i++)
            {
                Account account = accounts[i];
                if (account.GetAccountNumber() == transaction.GetAccountNumber())
                {
                    if (transaction.GetTransactionType() == "Deposit")
                    {
                        account.Deposit(transaction.GetAmount());
                    }
                    else if (transaction.GetTransactionType() == "Withdrawal")
                    {
                        account.Withdraw(transaction.GetAmount());
                    }
                    break;
                }
            }
        }
    }
}