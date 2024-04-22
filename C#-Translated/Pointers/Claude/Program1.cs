namespace Pointers1; // was not created I did it manually

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
        public List<Contact> Contacts;
        public int Count;

        public AddressBook()
        {
            Contacts = new List<Contact>();
            Count = 0;
        }
    }

    static void InitAddressBook(AddressBook addressBook)
    {
        addressBook.Contacts.Clear();
        addressBook.Count = 0;
    }

    static void FreeAddressBook(AddressBook addressBook)
    {
        addressBook.Contacts.Clear();
        addressBook.Count = 0;
    }

    static void AddContact(AddressBook addressBook, string name, string email, string phoneNumber)
    {
        Contact newContact = new Contact { Name = name, Email = email, PhoneNumber = phoneNumber };
        addressBook.Contacts.Add(newContact);
        addressBook.Count++;
    }

    static Contact SearchContact(AddressBook addressBook, string name)
    {
        return addressBook.Contacts.Find(contact => contact.Name == name);
    }

    static void RemoveContact(AddressBook addressBook, string name)
    {
        Contact contact = SearchContact(addressBook, name);
        if (contact != null)
        {
            addressBook.Contacts.Remove(contact);
            addressBook.Count--;
        }
    }

    static void PrintContacts(AddressBook addressBook)
    {
        foreach (Contact contact in addressBook.Contacts)
        {
            Console.WriteLine($"Name: {contact.Name}");
            Console.WriteLine($"Email: {contact.Email}");
            Console.WriteLine($"Phone Number: {contact.PhoneNumber}");
            Console.WriteLine();
        }
    }

    static void Main()
    {
        AddressBook addressBook = new AddressBook();
        InitAddressBook(addressBook);

        while (true)
        {
            Console.Write("Enter command (add/search/remove/print/exit): ");
            string command = Console.ReadLine();

            switch (command)
            {
                case "add":
                    Console.Write("Enter name: ");
                    string name = Console.ReadLine();
                    Console.Write("Enter email: ");
                    string email = Console.ReadLine();
                    Console.Write("Enter phone number: ");
                    string phoneNumber = Console.ReadLine();
                    AddContact(addressBook, name, email, phoneNumber);
                    break;
                case "search":
                    Console.Write("Enter name to search: ");
                    string searchName = Console.ReadLine();
                    Contact searchResult = SearchContact(addressBook, searchName);
                    if (searchResult != null)
                    {
                        Console.WriteLine($"Name: {searchResult.Name}");
                        Console.WriteLine($"Email: {searchResult.Email}");
                        Console.WriteLine($"Phone Number: {searchResult.PhoneNumber}");
                    }
                    else
                    {
                        Console.WriteLine("Contact not found");
                    }
                    break;
                case "remove":
                    Console.Write("Enter name to remove: ");
                    string removeName = Console.ReadLine();
                    RemoveContact(addressBook, removeName);
                    break;
                case "print":
                    PrintContacts(addressBook);
                    break;
                case "exit":
                    FreeAddressBook(addressBook);
                    return;
                default:
                    Console.WriteLine("Invalid command");
                    break;
            }
        }
    }
}