using System;
using System.Collections.Generic;

/*
    The translation was without errors expect the fact that ShoppingCart was private and
    could not be accessed outside, a way to fix this is to either make it public or a getter.
*/

namespace Interfaces2
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
}