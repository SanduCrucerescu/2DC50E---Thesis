namespace Pointers4; // was not created I did it manually

using System;
using System.Collections.Generic;
class Program
{
    class Contact
    {
        public string Name;
        public string Email;
        public string PhoneNumber;
    }

    class AddressBook
    {
        public List<Contact> Contacts = new List<Contact>();
        public int Count => Contacts.Count;

        public void InitAddressBook()
        {
            Contacts.Clear();
        }

        public void FreeAddressBook()
        {
            Contacts.Clear();
        }

        public void AddContact(string Name, string Email, string PhoneNumber)
        {
            Contact NewContact = new Contact
            {
                Name = Name,
                Email = Email,
                PhoneNumber = PhoneNumber
            };
            Contacts.Add(NewContact);
        }

        public Contact SearchContact(string Name)
        {
            return Contacts.Find(c => c.Name == Name);
        }

        public void RemoveContact(string Name)
        {
            Contact Contact = SearchContact(Name);
            if (Contact != null)
            {
                Contacts.Remove(Contact);
            }
        }

        public void PrintContacts()
        {
            foreach (Contact Contact in Contacts)
            {
                Console.WriteLine("Name: " + Contact.Name);
                Console.WriteLine("Email: " + Contact.Email);
                Console.WriteLine("Phone Number: " + Contact.PhoneNumber);
                Console.WriteLine();
            }
        }
    }

    static void Main()
    {
        AddressBook AddressBook = new AddressBook();
        AddressBook.InitAddressBook();
        string Name, Email, PhoneNumber, Command;

        while (true)
        {
            Console.Write("Enter command (add/search/remove/print/exit): ");
            Command = Console.ReadLine();
            switch (Command)
            {
                case "add":
                    Console.Write("Enter name: ");
                    Name = Console.ReadLine();
                    Console.Write("Enter email: ");
                    Email = Console.ReadLine();
                    Console.Write("Enter phone number: ");
                    PhoneNumber = Console.ReadLine();
                    AddressBook.AddContact(Name, Email, PhoneNumber);
                    break;
                case "search":
                    Console.Write("Enter name to search: ");
                    Name = Console.ReadLine();
                    Contact FoundContact = AddressBook.SearchContact(Name);
                    if (FoundContact != null)
                    {
                        Console.WriteLine("Name: " + FoundContact.Name);
                        Console.WriteLine("Email: " + FoundContact.Email);
                        Console.WriteLine("Phone Number: " + FoundContact.PhoneNumber);
                    }
                    else
                    {
                        Console.WriteLine("Contact not found");
                    }
                    break;
                case "remove":
                    Console.Write("Enter name to remove: ");
                    Name = Console.ReadLine();
                    AddressBook.RemoveContact(Name);
                    break;
                case "print":
                    AddressBook.PrintContacts();
                    break;
                case "exit":
                    return;
            }
        }
    }
}