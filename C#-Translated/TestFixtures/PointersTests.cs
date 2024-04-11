namespace TestFixtures;

using NUnit.Framework;
using NUnit.Framework.Legacy;
using Pointers;


[TestFixture]
public class Pointer
{
    private Pointers.Program.AddressBook addressBook;

    [SetUp]
    public void Setup()
    {
        addressBook = new Pointers.Program.AddressBook();
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

        Pointers.Program.Contact foundContact = addressBook.SearchContact(name);

        ClassicAssert.IsNotNull(foundContact);
        ClassicAssert.AreEqual(name, foundContact.Name);
        ClassicAssert.AreEqual(email, foundContact.Email);
        ClassicAssert.AreEqual(phoneNumber, foundContact.PhoneNumber);
    }

    [Test]
    public void SearchContact_ShouldReturnNullWhenNotFound()
    {
        string nonExistentName = "NonExistent";

        Pointers.Program.Contact foundContact = addressBook.SearchContact(nonExistentName);

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
        Assert.DoesNotThrow(() => new Pointers.Program.AddressBook());
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
