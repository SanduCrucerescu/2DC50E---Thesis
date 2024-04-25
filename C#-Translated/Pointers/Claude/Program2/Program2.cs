namespace Pointers2; // was not created I did it manually

using System;
using System.Collections.Generic;

class Program
{
    internal class Contact
    {
        public string Name;
        public string Email;
        public string PhoneNumber;
    }

    internal class AddressBook
    {
        public List<Contact> Contacts;
        public int Count;

        public AddressBook()
        {
            Contacts = new List<Contact>();
            Count = 0;
        }
    }

    internal static void InitAddressBook(AddressBook addressBook)
    {
        addressBook.Contacts.Clear();
        addressBook.Count = 0;
    }

    internal static void FreeAddressBook(AddressBook addressBook)
    {
        addressBook.Contacts.Clear();
        addressBook.Count = 0;
    }

    internal static void AddContact(AddressBook addressBook, string name, string email, string phoneNumber)
    {
        Contact newContact = new Contact
        {
            Name = name,
            Email = email,
            PhoneNumber = phoneNumber
        };
        addressBook.Contacts.Add(newContact);
        addressBook.Count++;
    }

    internal static Contact SearchContact(AddressBook addressBook, string name)
    {
        foreach (Contact contact in addressBook.Contacts)
        {
            if (contact.Name == name)
            {
                return contact;
            }
        }
        return null;
    }

    internal static void RemoveContact(AddressBook addressBook, string name)
    {
        Contact contact = SearchContact(addressBook, name);
        if (contact != null)
        {
            addressBook.Contacts.Remove(contact);
            addressBook.Count--;
        }
    }

    internal static void PrintContacts(AddressBook addressBook)
    {
        foreach (Contact contact in addressBook.Contacts)
        {
            Console.WriteLine("Name: " + contact.Name);
            Console.WriteLine("Email: " + contact.Email);
            Console.WriteLine("Phone Number: " + contact.PhoneNumber);
            Console.WriteLine();
        }
    }

    internal static void Main(string[] args)
    {
        AddressBook addressBook = new AddressBook();
        InitAddressBook(addressBook);
        string name, email, phoneNumber, command;

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
                    AddContact(addressBook, name, email, phoneNumber);
                    break;
                case "search":
                    Console.Write("Enter name to search: ");
                    name = Console.ReadLine();
                    Contact searchResult = SearchContact(addressBook, name);
                    if (searchResult != null)
                    {
                        Console.WriteLine("Name: " + searchResult.Name);
                        Console.WriteLine("Email: " + searchResult.Email);
                        Console.WriteLine("Phone Number: " + searchResult.PhoneNumber);
                    }
                    else
                    {
                        Console.WriteLine("Contact not found");
                    }
                    break;
                case "remove":
                    Console.Write("Enter name to remove: ");
                    name = Console.ReadLine();
                    RemoveContact(addressBook, name);
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