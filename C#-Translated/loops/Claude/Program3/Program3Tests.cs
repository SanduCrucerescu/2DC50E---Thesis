namespace loops3.Test
{
    using NUnit.Framework;
    using NUnit.Framework.Legacy;

    using System;
    using NUnit.Framework;

    [TestFixture]
    public class LoopsProgramTests
    {
        [Test]
        public void CheckIfInteger_ValidInput_ReturnsInteger()
        {
            var input = "42";

            var result = Program.CheckIfInteger(input);

            ClassicAssert.AreEqual(42, result);
        }

        [Test]
        public void CheckIfInteger_InvalidInput_PromptsForValidInput()
        {
            var input = new StringReader("abc\n123\n");
            Console.SetIn(input);
            var output = new StringWriter();
            Console.SetOut(output);

            var result = Program.CheckIfInteger("abc");

            ClassicAssert.AreEqual(123, result);
            StringAssert.Contains("Please write an integer", output.ToString());
        }

        [Test]
        public void InitArray_ValidInput_InitializesStudentsArray()
        {
            var input = new StringReader("5\n");
            Console.SetIn(input);

            Program.InitArray();

            ClassicAssert.AreEqual(5, Program.Students.Count);
        }

        [Test]
        public void AddStudentInformation_ValidInput_AddsStudentToArray()
        {
            Program.Students = new List<Program.Student>(1);
            var input = new StringReader("John\nDoe\n85\n");
            Console.SetIn(input);

            Program.AddStudentInformation();

            ClassicAssert.AreEqual("John", Program.Students[0].FirstName);
            ClassicAssert.AreEqual("Doe", Program.Students[0].LastName);
            ClassicAssert.AreEqual(85, Program.Students[0].AverageGrade);
        }

        [Test]
        public void ListStudents_StudentsAdded_DisplaysStudentInformation()
        {
            Program.Students = new List<Program.Student>
            {
                new Program.Student { FirstName = "John", LastName = "Doe", AverageGrade = 80 },
                new Program.Student { FirstName = "Jane", LastName = "Smith", AverageGrade = 90 }
            };
            var output = new StringWriter();
            Console.SetOut(output);

            Program.ListStudents();

            StringAssert.Contains("John", output.ToString());
            StringAssert.Contains("Doe", output.ToString());
            StringAssert.Contains("80", output.ToString());
            StringAssert.Contains("Jane", output.ToString());
            StringAssert.Contains("Smith", output.ToString());
            StringAssert.Contains("90", output.ToString());
            StringAssert.Contains("Average class grade: 85", output.ToString());
        }
    }
}