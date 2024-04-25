namespace Interfaces10.Test
{
    using NUnit.Framework;
    using NUnit.Framework.Legacy;

    [TestFixture]
    public class InterfacesTests
    {
        [Test]
        public void AddItem_ShouldIncreaseCount()
        {
            TShoppingCart cart = new TShoppingCart();
            IProduct product = new TProduct("Test Product", 10.0);

            cart.AddItem(product);

            ClassicAssert.AreEqual(1, cart.Count);
        }

        [Test]
        public void GetTotalCost_ShouldReturnCorrectTotal()
        {
            TShoppingCart cart = new TShoppingCart();
            cart.AddItem(new TProduct("Product 1", 10.0));
            cart.AddItem(new TProduct("Product 2", 20.0));

            double totalCost = cart.GetTotalCost();

            ClassicAssert.AreEqual(30.0, totalCost);
        }

        [Test]
        public void GetName_ShouldReturnCorrectName()
        {
            IProduct product = new TProduct("Test Product", 10.0);

            string name = product.Name;

            ClassicAssert.AreEqual("Test Product", name);
        }

        [Test]
        public void GetPrice_ShouldReturnCorrectPrice()
        {
            IProduct product = new TProduct("Test Product", 10.0);

            double price = product.Price;

            ClassicAssert.AreEqual(10.0, price);
        }

        [Test]
        public void Count_ShouldReturnCorrectCountAfterAddingAndRemovingItems()
        {
            TShoppingCart cart = new TShoppingCart();
            IProduct product1 = new TProduct("Product 1", 10.0);
            IProduct product2 = new TProduct("Product 2", 20.0);

            cart.AddItem(product1);
            cart.AddItem(product2);
            cart.FItems.RemoveAt(0);

            ClassicAssert.AreEqual(1, cart.Count);
        }

        [TestCase("Test Product", 10.0)]
        [TestCase("Another Product", 20.0)]
        public void SmokeTest_ProductCreation(string name, double price)
        {
            IProduct product = new TProduct(name, price);

            ClassicAssert.IsNotNull(product);
            ClassicAssert.AreEqual(name, product.Name);
            ClassicAssert.AreEqual(price, product.Price);
        }

        [Test]
        public void SmokeTest_ShoppingCartCreation()
        {
            TShoppingCart cart = new TShoppingCart();

            ClassicAssert.IsNotNull(cart);
            ClassicAssert.AreEqual(0, cart.Count);
        }
    }
}