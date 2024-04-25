namespace Interfaces8.Test
{
    using NUnit.Framework;
    using NUnit.Framework.Legacy;

    [TestFixture]
    public class InterfacesTests
    {
        [Test]
        public void AddItem_ShouldIncreaseCount()
        {
            ShoppingCart cart = new ShoppingCart();
            IProduct product = new Product("Test Product", 10.0);

            cart.AddItem(product);

            ClassicAssert.AreEqual(1, cart.Count);
        }

        [Test]
        public void GetTotalCost_ShouldReturnCorrectTotal()
        {
            ShoppingCart cart = new ShoppingCart();
            cart.AddItem(new Product("Product 1", 10.0));
            cart.AddItem(new Product("Product 2", 20.0));

            double totalCost = cart.GetTotalCost();

            ClassicAssert.AreEqual(30.0, totalCost);
        }

        [Test]
        public void GetName_ShouldReturnCorrectName()
        {
            IProduct product = new Product("Test Product", 10.0);

            string name = product.Name;

            ClassicAssert.AreEqual("Test Product", name);
        }

        [Test]
        public void GetPrice_ShouldReturnCorrectPrice()
        {
            IProduct product = new Product("Test Product", 10.0);

            double price = product.Price;

            ClassicAssert.AreEqual(10.0, price);
        }

        [Test]
        public void Count_ShouldReturnCorrectCountAfterAddingAndRemovingItems()
        {
            ShoppingCart cart = new ShoppingCart();
            IProduct product1 = new Product("Product 1", 10.0);
            IProduct product2 = new Product("Product 2", 20.0);

            cart.AddItem(product1);
            cart.AddItem(product2);
            cart.items.RemoveAt(0);

            ClassicAssert.AreEqual(1, cart.Count);
        }

        [TestCase("Test Product", 10.0)]
        [TestCase("Another Product", 20.0)]
        public void SmokeTest_ProductCreation(string name, double price)
        {
            IProduct product = new Product(name, price);

            ClassicAssert.IsNotNull(product);
            ClassicAssert.AreEqual(name, product.Name);
            ClassicAssert.AreEqual(price, product.Price);
        }

        [Test]
        public void SmokeTest_ShoppingCartCreation()
        {
            ShoppingCart cart = new ShoppingCart();

            ClassicAssert.IsNotNull(cart);
            ClassicAssert.AreEqual(0, cart.Count);
        }
    }
}