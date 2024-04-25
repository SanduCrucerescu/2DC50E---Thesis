using NUnit.Framework;
using NUnit.Framework.Legacy;
using System;

namespace BankAccountSystem4.Tests
{
    [TestFixture]
    public class BankAccountSystemTests
    {
        private AccountList accountList;

        [SetUp]
        public void Setup()
        {
            accountList = new AccountList();
        }

        // Smoke Tests
        [Test]
        public void SmokeTest_CreateAccount_AccountCreated()
        {
            string accountHolder = "John Doe";
            string accountNumber = "1234567890";
            double initialBalance = 1000;

            accountList.AddAccount(new Account(accountHolder, accountNumber, initialBalance));

            ClassicAssert.AreEqual(1, accountList.GetCount());
        }

        [Test]
        public void SmokeTest_Deposit_BalanceUpdated()
        {
            Account account = new Account("John Doe", "1234567890", 1000);
            accountList.AddAccount(account);
            double depositAmount = 500;

            accountList.ProcessTransaction(new Transaction(account.GetAccountNumber(), depositAmount, "Deposit"));

            ClassicAssert.AreEqual(1500, account.GetBalance());
        }

        [Test]
        public void SmokeTest_Withdraw_BalanceUpdated()
        {
            Account account = new Account("John Doe", "1234567890", 1000);
            accountList.AddAccount(account);
            double withdrawalAmount = 500;

            accountList.ProcessTransaction(new Transaction(account.GetAccountNumber(), withdrawalAmount, "Withdrawal"));

            ClassicAssert.AreEqual(500, account.GetBalance());
        }

        // Unit Tests
        [Test]
        public void UnitTest_GetAccountHolder_ReturnsAccountHolder()
        {
            string accountHolder = "John Doe";
            Account account = new Account(accountHolder, "1234567890", 1000);

            string result = account.GetAccountHolder();

            ClassicAssert.AreEqual(accountHolder, result);
        }

        [Test]
        public void UnitTest_GetAccountNumber_ReturnsAccountNumber()
        {
            string accountNumber = "1234567890";
            Account account = new Account("John Doe", accountNumber, 1000);

            string result = account.GetAccountNumber();

            ClassicAssert.AreEqual(accountNumber, result);
        }

        [Test]
        public void UnitTest_GetBalance_ReturnsBalance()
        {
            double balance = 1000;
            Account account = new Account("John Doe", "1234567890", balance);

            double result = account.GetBalance();

            ClassicAssert.AreEqual(balance, result);
        }

        [Test]
        public void UnitTest_Deposit_UpdatesBalance()
        {
            Account account = new Account("John Doe", "1234567890", 1000);
            double depositAmount = 500;

            account.Deposit(depositAmount);

            ClassicAssert.AreEqual(1500, account.GetBalance());
        }

        [Test]
        public void UnitTest_Withdraw_UpdatesBalance()
        {
            Account account = new Account("John Doe", "1234567890", 1000);
            double withdrawalAmount = 500;

            account.Withdraw(withdrawalAmount);

            ClassicAssert.AreEqual(500, account.GetBalance());
        }

        [Test]
        public void UnitTest_Withdraw_InsufficientFunds_ThrowsException()
        {
            Account account = new Account("John Doe", "1234567890", 1000);
            double withdrawalAmount = 1500;

            Assert.Throws<Exception>(() => account.Withdraw(withdrawalAmount));
        }

        // Class Name Tests
        [Test]
        public void ClassNameTest_TAccount_Exists()
        {
            var account = new Account("John Doe", "1234567890", 1000);

            ClassicAssert.IsInstanceOf<Account>(account);
        }

        [Test]
        public void ClassNameTest_TAccountList_Exists()
        {
            var accountList = new AccountList();

            ClassicAssert.IsInstanceOf<AccountList>(accountList);
        }

        [Test]
        public void ClassNameTest_TTransaction_Exists()
        {
            var transaction = new Transaction("1234567890", 1000, "Deposit");

            ClassicAssert.IsInstanceOf<Transaction>(transaction);
        }

        [Test]
        public void IntegrationTest_AddAccount_AccountAdded()
        {
            string accountHolder = "John Doe";
            string accountNumber = "1234567890";
            double initialBalance = 1000;

            accountList.AddAccount(new Account(accountHolder, accountNumber, initialBalance));

            ClassicAssert.AreEqual(1, accountList.GetCount());
            Account addedAccount = accountList.GetAccount(0);
            ClassicAssert.AreEqual(accountHolder, addedAccount.GetAccountHolder());
            ClassicAssert.AreEqual(accountNumber, addedAccount.GetAccountNumber());
            ClassicAssert.AreEqual(initialBalance, addedAccount.GetBalance());
        }

        [Test]
        public void IntegrationTest_ProcessTransaction_Deposit_BalanceUpdated()
        {
            Account account = new Account("John Doe", "1234567890", 1000);
            accountList.AddAccount(account);
            double depositAmount = 500;

            accountList.ProcessTransaction(new Transaction(account.GetAccountNumber(), depositAmount, "Deposit"));

            Account updatedAccount = accountList.GetAccount(0);
            ClassicAssert.AreEqual(1500, updatedAccount.GetBalance());
        }

        [Test]
        public void IntegrationTest_ProcessTransaction_Withdraw_BalanceUpdated()
        {
            Account account = new Account("John Doe", "1234567890", 1000);
            accountList.AddAccount(account);
            double withdrawalAmount = 500;

            accountList.ProcessTransaction(new Transaction(account.GetAccountNumber(), withdrawalAmount, "Withdrawal"));

            Account updatedAccount = accountList.GetAccount(0);
            ClassicAssert.AreEqual(500, updatedAccount.GetBalance());
        }

    }
}