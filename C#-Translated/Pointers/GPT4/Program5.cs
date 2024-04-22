namespace Pointers10; // was not created I did it manually

using System;
using System.Collections.Generic;

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
        public List<Contact> Contacts { get; set; } = new List<Contact>();
        public int Count => Contacts.Count;

        public void AddContact(string name, string email, string phoneNumber)
        {
            Contacts.Add(new Contact { Name = name, Email = email, PhoneNumber = phoneNumber });
        }

        public Contact SearchContact(string name)
        {
            return Contacts.Find(contact => contact.Name == name);
        }

        public void RemoveContact(string name)
        {
            var contact = SearchContact(name);
            if (contact != null)
            {
                Contacts.Remove(contact);
            }
        }

        public void PrintContacts()
        {
            foreach (var contact in Contacts)
            {
                Console.WriteLine($"Name: {contact.Name}");
                Console.WriteLine($"Email: {contact.Email}");
                Console.WriteLine($"Phone Number: {contact.PhoneNumber}");
                Console.WriteLine();
            }
        }
    }

    static void Main()
    {
        AddressBook addressBook = new AddressBook();
        string name, email, phoneNumber, command;

        do
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
                    var result = addressBook.SearchContact(name);
                    if (result != null)
                    {
                        Console.WriteLine($"Name: {result.Name}");
                        Console.WriteLine($"Email: {result.Email}");
                        Console.WriteLine($"Phone Number: {result.PhoneNumber}");
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
            }
        } while (command != "exit");
    }
}