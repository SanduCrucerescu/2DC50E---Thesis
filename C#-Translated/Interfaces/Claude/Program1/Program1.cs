using System;
using System.Collections.Generic;

/*
The IProduct interface and TProduct class are defined similarly to the Delphi code.
The TShoppingCart class uses a List<IProduct> instead of an array to store the items.
The GetCount property is simplified using the Count property of the List<IProduct>.
The Destroy method is not needed in C# as the garbage collector handles memory management.
The GetTotalCost method uses a foreach loop to iterate over the items and calculate the total cost.
The Main method is similar to the Delphi code, with minor syntax differences.
The try-finally block is used to ensure that the Cart reference is set to null at the end.
*/

namespace Interfaces1
{
    interface IProduct
    {
        string Name { get; }
        double Price { get; }
    }

    class TProduct : IProduct
    {
        private string FName;
        private double FPrice;

        public TProduct(string AName, double APrice)
        {
            FName = AName;
            FPrice = APrice;
        }

        public string Name => FName;
        public double Price => FPrice;
    }

    class TShoppingCart
    {
        internal List<IProduct> FItems;

        public TShoppingCart()
        {
            FItems = new List<IProduct>();
        }

        public int Count => FItems.Count;

        public void AddItem(IProduct AProduct)
        {
            FItems.Add(AProduct);
        }

        public double GetTotalCost()
        {
            double TotalCost = 0;
            foreach (var item in FItems)
            {
                TotalCost += item.Price;
            }
            return TotalCost;
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            var Cart = new TShoppingCart();
            try
            {
                while (true)
                {
                    Console.Write("Enter product name (or 'x' to finish): ");
                    string ProductName = Console.ReadLine();

                    if (ProductName.ToLower() == "x")
                        break;

                    Console.Write("Enter product price: ");
                    double ProductPrice = double.Parse(Console.ReadLine());

                    Cart.AddItem(new TProduct(ProductName, ProductPrice));
                }

                Console.WriteLine("Shopping Cart Contents:");
                Console.WriteLine("------------------------");
                for (int I = 0; I < Cart.Count; I++)
                {
                    Console.WriteLine($"{Cart.FItems[I].Name}: ${Cart.FItems[I].Price}");
                }

                Console.WriteLine("------------------------");
                Console.WriteLine($"Total Cost: ${Cart.GetTotalCost()}");

                Console.Write("Press Enter to exit...");
                Console.ReadLine();
            }
            finally
            {
                Cart = null;
            }
        }
    }
}