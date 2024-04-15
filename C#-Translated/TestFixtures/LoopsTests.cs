
namespace loops.Test;
using loops;
using NUnit.Framework.Legacy;

[Category("Template")]
[TestFixture]
public class LoopsTests
{
    [TestCase("2")]
    [TestCase("10")]
    public void Should_Check_If_The_Array_Has_The_Right_Size(string x)
    {

        using (var sw = new StringWriter())
        {

            Student[] students;
            using (var sr = new StringReader(x))
            {
                Console.SetIn(sr);
                Console.SetOut(sw);

                Program.InitArray(out students);

                ClassicAssert.IsNotNull(students);
                ClassicAssert.AreEqual(Int32.Parse(x), students.Length);
            }
        }
    }

    [Test]
    public void Should_Test_If_Students_Are_Added()
    {
        using (var sw = new StringWriter())
        {
            var students = new Student[2];

            string[] simulatedInput = { "John", "Doe", "90", "Jane", "Smith", "85" };
            StringReader sr = new StringReader(string.Join(Environment.NewLine, simulatedInput));
            Console.SetIn(sr);
            Console.SetOut(sw);

            Program.AddStudentInformation(students);
            ClassicAssert.IsNotNull(students);
            ClassicAssert.AreEqual(2, students.Length);


            ClassicAssert.AreEqual("John", students[0].FirstName);
            ClassicAssert.AreEqual("Doe", students[0].LastName);
            ClassicAssert.AreEqual(90, students[0].AverageGrade);


            ClassicAssert.AreEqual("Jane", students[1].FirstName);
            ClassicAssert.AreEqual("Smith", students[1].LastName);
            ClassicAssert.AreEqual(85, students[1].AverageGrade);
        }
    }
}
