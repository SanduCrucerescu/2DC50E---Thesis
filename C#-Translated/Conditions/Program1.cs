// using System;
// using NUnit.Framework;
// using NUnit.Framework.Legacy;

// namespace Conditions1
// {
//     public class Program
//     {
//         public static void Main()
//         {
//             Console.Write("What is your age? ");
//             string input = Console.ReadLine();

//             if (int.TryParse(input, out int age))
//             {
//                 StudentAge(age);
//             }
//             else
//             {
//                 Console.WriteLine("Please insert an integer.");
//                 Console.WriteLine("Press any key to exit.");
//                 Console.ReadKey();
//             }
//         }

//         public static void StudentAge(int num)
//         {
//             if (num < 18)
//             {
//                 Console.WriteLine("The person is underage!");
//             }
//             else
//             {
//                 Console.WriteLine("The person is eligible for this position.");
//             }
//         }
//     }

//     [TestFixture]
//     public class ConditionsTests
//     {
//         private const string exptTrue = "The person is eligible for this position.";
//         private const string exptFalse = "The person is underage!";

//         [TestCase(89)]
//         [TestCase(19)]
//         public void Should_Return_True_If_Over_18(int x)
//         {
//             using (var sw = new StringWriter())
//             {
//                 Console.SetOut(sw);

//                 Program.StudentAge(x);

//                 var result = sw.ToString().Trim();
//                 ClassicAssert.AreEqual(exptTrue, result);
//             }
//         }

//         [TestCase(17)]
//         [TestCase(2)]
//         public void Should_Return_False_If_Under_18(int x)
//         {
//             using (var sw = new StringWriter())
//             {
//                 Console.SetOut(sw);

//                 Program.StudentAge(x);

//                 var result = sw.ToString().Trim();
//                 ClassicAssert.AreEqual(exptFalse, result);
//             }
//         }
//     }
// }