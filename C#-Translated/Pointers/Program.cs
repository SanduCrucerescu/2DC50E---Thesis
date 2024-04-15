using System;
using System.Collections.Generic;
using NUnit.Framework;
using NUnit.Framework.Legacy;

/*
    The translation was without any issues. Although Claude removed the pointers this is the explanation given:
    In the C# version of the code, pointers were removed because C# does not have explicit pointer syntax like Delphi does. Instead, C# uses managed memory and reference types to achieve similar functionality without the need for manual memory management with pointers.
    In the Delphi code, the PContact and PAddressBook types were pointer types, which allowed dynamic allocation and deallocation of memory for individual TContact records and the TAddressBook record using New, Dispose, and manual memory management.
    In C#, reference types like classes and lists are used instead of pointers. The Contact class represents an individual contact, and the AddressBook class contains a List<Contact> to store the contacts. Memory for these objects is automatically managed by the .NET runtime's garbage collector, eliminating the need for manual memory management with pointers.
    The dynamic allocation and deallocation of contacts in the Delphi code have been replaced with simpler operations in C#:
    - Instead of New(NewContact), a new Contact object is created using the new keyword and object initializer syntax: new Contact { Name = name, Email = email, PhoneNumber = phoneNumber }.
     Instead of Dispose(AddressBook.Contacts[I]), references to Contact objects are removed from the Contacts list using the Remove method.
    - Instead of manual array resizing, the List<Contact> automatically resizes as contacts are added or removed.
    By using reference types and managed memory, the C# code avoids the complexity and potential issues associated with manual memory management with pointers, while providing similar functionality to the original Delphi code.
*/

namespace Pointers1;

public class Program
{
    public class Contact
    {
        public string Name { get; set; }
        public string Email { get; set; }
        public string PhoneNumber { get; set; }
    }

    public class AddressBook
    {
        public List<Contact> Contacts { get; set; }

        public AddressBook()
        {
            Contacts = new List<Contact>();
        }

        public void AddContact(string name, string email, string phoneNumber)
        {
            Contact newContact = new Contact
            {
                Name = name,
                Email = email,
                PhoneNumber = phoneNumber
            };
            Contacts.Add(newContact);
        }

        public Contact SearchContact(string name)
        {
            return Contacts.Find(c => c.Name == name);
        }

        public void RemoveContact(string name)
        {
            Contact contact = SearchContact(name);
            if (contact != null)
            {
                Contacts.Remove(contact);
            }
        }

        public void PrintContacts()
        {
            foreach (Contact contact in Contacts)
            {
                Console.WriteLine($"Name: {contact.Name}");
                Console.WriteLine($"Email: {contact.Email}");
                Console.WriteLine($"Phone Number: {contact.PhoneNumber}");
                Console.WriteLine();
            }
        }
    }

    static void Main(string[] args)
    {
        AddressBook addressBook = new AddressBook();
        string command, name, email, phoneNumber;

        while (true)
        {
            Console.Write("Enter command (add/search/remove/print/exit): ");
            command = Console.ReadLine();

            switch (command)
            {
                case "add":
                    Console.Write("Enter name: ");
                    name = Console.ReadLine();
                    Console.Write("Enter email: ");
                    email = Console.ReadLine();
                    Console.Write("Enter phone number: ");
                    phoneNumber = Console.ReadLine();
                    addressBook.AddContact(name, email, phoneNumber);
                    break;
                case "search":
                    Console.Write("Enter name to search: ");
                    name = Console.ReadLine();
                    Contact contact = addressBook.SearchContact(name);
                    if (contact != null)
                    {
                        Console.WriteLine($"Name: {contact.Name}");
                        Console.WriteLine($"Email: {contact.Email}");
                        Console.WriteLine($"Phone Number: {contact.PhoneNumber}");
                    }
                    else
                    {
                        Console.WriteLine("Contact not found");
                    }
                    break;
                case "remove":
                    Console.Write("Enter name to remove: ");
                    name = Console.ReadLine();
                    addressBook.RemoveContact(name);
                    break;
                case "print":
                    addressBook.PrintContacts();
                    break;
                case "exit":
                    return;
                default:
                    Console.WriteLine("Invalid command");
                    break;
            }
        }
    }

    [TestFixture]
    public class Pointer
    {
        private AddressBook addressBook;

        [SetUp]
        public void Setup()
        {
            addressBook = new AddressBook();
        }

        [Test]
        public void AddContact_ShouldAddContactToList()
        {
            string name = "John Doe";
            string email = "john.doe@example.com";
            string phoneNumber = "1234567890";

            addressBook.AddContact(name, email, phoneNumber);

            ClassicAssert.AreEqual(1, addressBook.Contacts.Count);
            ClassicAssert.AreEqual(name, addressBook.Contacts[0].Name);
            ClassicAssert.AreEqual(email, addressBook.Contacts[0].Email);
            ClassicAssert.AreEqual(phoneNumber, addressBook.Contacts[0].PhoneNumber);
        }

        [Test]
        public void SearchContact_ShouldReturnContactWhenFound()
        {
            string name = "Jane Smith";
            string email = "jane.smith@example.com";
            string phoneNumber = "0987654321";
            addressBook.AddContact(name, email, phoneNumber);

            Contact foundContact = addressBook.SearchContact(name);

            ClassicAssert.IsNotNull(foundContact);
            ClassicAssert.AreEqual(name, foundContact.Name);
            ClassicAssert.AreEqual(email, foundContact.Email);
            ClassicAssert.AreEqual(phoneNumber, foundContact.PhoneNumber);
        }

        [Test]
        public void SearchContact_ShouldReturnNullWhenNotFound()
        {
            string nonExistentName = "NonExistent";

            Contact foundContact = addressBook.SearchContact(nonExistentName);

            ClassicAssert.IsNull(foundContact);
        }

        [Test]
        public void RemoveContact_ShouldRemoveContactFromList()
        {
            string name = "Alice Brown";
            string email = "alice.brown@example.com";
            string phoneNumber = "5551234567";
            addressBook.AddContact(name, email, phoneNumber);

            addressBook.RemoveContact(name);

            ClassicAssert.AreEqual(0, addressBook.Contacts.Count);
        }

        [Test]
        public void RemoveContact_ShouldNotThrowExceptionWhenContactNotFound()
        {
            string nonExistentName = "NonExistent";

            Assert.DoesNotThrow(() => addressBook.RemoveContact(nonExistentName));
        }

        [Test]
        public void SmokeTest_AddressBookConstructor()
        {
            Assert.DoesNotThrow(() => new AddressBook());
        }

        [Test]
        public void SmokeTest_AddContactMethod()
        {
            string name = "Test Contact";
            string email = "test@example.com";
            string phoneNumber = "1234567890";

            Assert.DoesNotThrow(() => addressBook.AddContact(name, email, phoneNumber));
        }

        [Test]
        public void SmokeTest_SearchContactMethod()
        {
            string name = "Test Contact";

            Assert.DoesNotThrow(() => addressBook.SearchContact(name));
        }

        [Test]
        public void SmokeTest_RemoveContactMethod()
        {
            string name = "Test Contact";

            Assert.DoesNotThrow(() => addressBook.RemoveContact(name));
        }

        [Test]
        public void SmokeTest_PrintContactsMethod()
        {
            Assert.DoesNotThrow(() => addressBook.PrintContacts());
        }
    }
}