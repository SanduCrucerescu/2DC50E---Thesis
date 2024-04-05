using System;
using System.Collections.Generic;

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


class Program
{
    class Contact
    {
        public string Name { get; set; }
        public string Email { get; set; }
        public string PhoneNumber { get; set; }
    }

    class AddressBook
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
}