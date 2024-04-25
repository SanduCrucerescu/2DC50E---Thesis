using NUnit.Framework;
using NUnit.Framework.Legacy;
using System;
using System.IO;

namespace Pointers10.Test
{
    [TestFixture]
    public class AddressBookTests
    {
        private StringWriter consoleOutput;
        private StringReader consoleInput;

        [SetUp]
        public void Setup()
        {
            consoleOutput = new StringWriter();
            Console.SetOut(consoleOutput);
        }

        [TearDown]
        public void TearDown()
        {
            consoleOutput.Dispose();
            if (consoleInput != null)
                consoleInput.Dispose();
        }

        // Smoke Tests
        [Test]
        public void SmokeTest_ProgramRuns_WithoutCrashing()
        {
            consoleInput = new StringReader("exit\n");
            Console.SetIn(consoleInput);

            Program.Main();

            Assert.Pass("Program runs without crashing.");
        }

        // // Unit Tests
        // [Test]
        // public void AddContact_AddsContactToAddressBook()
        // {
        //     var addressBook = new Program.TAddressBook();
        //     Program.InitAddressBook(addressBook);

        //     Program.AddContact(addressBook, "John Doe", "john@example.com", "1234567890");

        //     ClassicAssert.AreEqual(1, addressBook.Count);
        //     ClassicAssert.AreEqual("John Doe", addressBook.Contacts[0].Name);
        //     ClassicAssert.AreEqual("john@example.com", addressBook.Contacts[0].Email);
        //     ClassicAssert.AreEqual("1234567890", addressBook.Contacts[0].PhoneNumber);
        // }

        // [Test]
        // public void SearchContact_ReturnsContactWhenFound()
        // {
        //     var addressBook = new Program.TAddressBook();
        //     Program.InitAddressBook(addressBook);
        //     Program.AddContact(addressBook, "John Doe", "john@example.com", "1234567890");

        //     var contact = Program.SearchContact(addressBook, "John Doe");

        //     ClassicAssert.IsNotNull(contact);
        //     ClassicAssert.AreEqual("John Doe", contact.Name);
        //     ClassicAssert.AreEqual("john@example.com", contact.Email);
        //     ClassicAssert.AreEqual("1234567890", contact.PhoneNumber);
        // }

        // [Test]
        // public void SearchContact_ReturnsNullWhenContactNotFound()
        // {
        //     var addressBook = new Program.TAddressBook();
        //     Program.InitAddressBook(addressBook);

        //     var contact = Program.SearchContact(addressBook, "Jane Smith");

        //     ClassicAssert.IsNull(contact);
        // }

        // [Test]
        // public void RemoveContact_RemovesContactFromAddressBook()
        // {
        //     var addressBook = new Program.TAddressBook();
        //     Program.InitAddressBook(addressBook);
        //     Program.AddContact(addressBook, "John Doe", "john@example.com", "1234567890");

        //     Program.RemoveContact(addressBook, "John Doe");

        //     ClassicAssert.AreEqual(0, addressBook.Count);
        // }

        // Functional Tests
        [Test]
        public void AddCommand_AddsContactToAddressBook()
        {
            consoleInput = new StringReader("add\nJohn Doe\njohn@example.com\n1234567890\nexit\n");
            Console.SetIn(consoleInput);

            Program.Main();

            Assert.That(consoleOutput.ToString(), Contains.Substring("Enter name: Enter email: Enter phone number: "));
        }

        [Test]
        public void SearchCommand_DisplaysContactDetails()
        {
            consoleInput = new StringReader("add\nJohn Doe\njohn@example.com\n1234567890\nsearch\nJohn Doe\nexit\n");
            Console.SetIn(consoleInput);

            Program.Main();

            Assert.That(consoleOutput.ToString(), Contains.Substring("Name: John Doe\nEmail: john@example.com\nPhone Number: 1234567890"));
        }

        [Test]
        public void RemoveCommand_RemovesContactFromAddressBook()
        {
            consoleInput = new StringReader("add\nJohn Doe\njohn@example.com\n1234567890\nremove\nJohn Doe\nprint\nexit\n");
            Console.SetIn(consoleInput);

            Program.Main();

            Assert.That(consoleOutput.ToString(), Does.Not.Contain("Name: John Doe"));
        }

        [Test]
        public void PrintCommand_DisplaysAllContacts()
        {
            consoleInput = new StringReader("add\nJohn Doe\njohn@example.com\n1234567890\nadd\nJane Smith\njane@example.com\n9876543210\nprint\nexit\n");
            Console.SetIn(consoleInput);

            Program.Main();

            Assert.That(consoleOutput.ToString(), Contains.Substring("Name: John Doe\nEmail: john@example.com\nPhone Number: 1234567890\n\nName: Jane Smith\nEmail: jane@example.com\nPhone Number: 9876543210"));
        }
    }
}