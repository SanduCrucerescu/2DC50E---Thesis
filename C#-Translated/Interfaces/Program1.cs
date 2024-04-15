using System;
using System.Collections.Generic;
using NUnit.Framework;
using NUnit.Framework.Legacy;
/*
    The translation was without errors expect the fact that ShoppingCart was private and
    could not be accessed outside, a way to fix this is to either make it public or a getter.
*/

namespace Interfaces1
{
    public interface IProduct
    {
        string GetName();
        double GetPrice();
    }
    public class Product : IProduct
    {
        private string fName;
        private double fPrice;

        public Product(string aName, double aPrice)
        {
            fName = aName;
            fPrice = aPrice;
        }

        public string GetName()
        {
            return fName;
        }

        public double GetPrice()
        {
            return fPrice;
        }
    }

    public class ShoppingCart
    {
        public List<IProduct> fItems = new List<IProduct>(); // changed to public

        public int Count
        {
            get { return fItems.Count; }
        }

        public void AddItem(IProduct aProduct)
        {
            fItems.Add(aProduct);
        }

        public double GetTotalCost()
        {
            double totalCost = 0;
            foreach (IProduct item in fItems)
            {
                totalCost += item.GetPrice();
            }
            return totalCost;
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            ShoppingCart cart = new ShoppingCart();
            string productName;
            double productPrice;

            do
            {
                Console.Write("Enter product name (or 'x' to finish): ");
                productName = Console.ReadLine();

                if (!productName.ToLower().Equals("x"))
                {
                    Console.Write("Enter product price: ");
                    productPrice = double.Parse(Console.ReadLine());

                    cart.AddItem(new Product(productName, productPrice));
                }
            } while (!productName.ToLower().Equals("x"));

            Console.WriteLine("Shopping Cart Contents:");
            Console.WriteLine("------------------------");
            for (int i = 0; i < cart.Count; i++)
            {
                IProduct item = cart.fItems[i];
                Console.WriteLine($"{item.GetName()}: ${item.GetPrice()}");
            }

            Console.WriteLine("------------------------");
            Console.WriteLine($"Total Cost: ${cart.GetTotalCost()}");

            Console.Write("Press Enter to exit...");
            Console.ReadLine();
        }
    }

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

            string name = product.GetName();

            ClassicAssert.AreEqual("Test Product", name);
        }

        [Test]
        public void GetPrice_ShouldReturnCorrectPrice()
        {
            IProduct product = new Product("Test Product", 10.0);

            double price = product.GetPrice();

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
            cart.fItems.RemoveAt(0);

            ClassicAssert.AreEqual(1, cart.Count);
        }

        [TestCase("Test Product", 10.0)]
        [TestCase("Another Product", 20.0)]
        public void SmokeTest_ProductCreation(string name, double price)
        {
            IProduct product = new Product(name, price);

            ClassicAssert.IsNotNull(product);
            ClassicAssert.AreEqual(name, product.GetName());
            ClassicAssert.AreEqual(price, product.GetPrice());
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