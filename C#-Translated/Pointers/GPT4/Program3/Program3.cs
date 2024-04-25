namespace Pointers8; // was not created I did it manually

using System;
using System.Collections.Generic;

class Program
{
    internal class Contact
    {
        public string Name { get; set; }
        public string Email { get; set; }
        public string PhoneNumber { get; set; }
    }

    internal class AddressBook
    {
        public List<Contact> Contacts { get; set; } = new List<Contact>();
        public int Count => Contacts.Count;

        public void InitAddressBook()
        {
            Contacts.Clear();
        }

        public void FreeAddressBook()
        {
            Contacts.Clear();
        }

        public void AddContact(string name, string email, string phoneNumber)
        {
            Contacts.Add(new Contact { Name = name, Email = email, PhoneNumber = phoneNumber });
        }

        public Contact SearchContact(string name)
        {
            return Contacts.Find(c => c.Name == name);
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

    internal static void Main(string[] args)
    {
        var addressBook = new AddressBook();
        addressBook.InitAddressBook();
        string name, email, phoneNumber, command;

        do
        {
            Console.Write("Enter command (add/search/remove/print/exit): ");
            command = Console.ReadLine().ToLower();

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
                    var contact = addressBook.SearchContact(name);
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
            }
        } while (command != "exit");

        addressBook.FreeAddressBook();
    }
}