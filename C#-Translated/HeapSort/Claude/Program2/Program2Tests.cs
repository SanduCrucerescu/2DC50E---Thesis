using NUnit.Framework;
using NUnit.Framework.Legacy;
using static HeapSort2.Program;

namespace HeapSort2Fix.Test
{
    [TestFixture]
    [Category("Run")]
    public class HeapSortTests
    {
        [Test]
        public void SmokeTest()
        {
            List<object> arr = new List<object> { 5, 876, 234, 9, 12, 44, 2 };
            Program.TDHeapSort(arr, 0, arr.Count - 1, Program.TDCompareLongint);
            ClassicAssert.AreEqual(new List<object> { 2, 5, 9, 12, 44, 234, 876 }, arr);
        }

        [Test]
        public void UnitTest_TDValidateListRange()
        {
            List<object> arr = new List<object> { 1, 2, 3, 4, 5 };
            ClassicAssert.DoesNotThrow(() => Program.TDValidateListRange(arr, 0, 4, "Valid range"));
            ClassicAssert.Throws<EtdTListException>(() => Program.TDValidateListRange(null, 0, 4, "Null list"));
            ClassicAssert.Throws<EtdTListException>(() => Program.TDValidateListRange(arr, -1, 4, "Invalid start index"));
            ClassicAssert.Throws<EtdTListException>(() => Program.TDValidateListRange(arr, 0, 5, "Invalid end index"));
            ClassicAssert.Throws<EtdTListException>(() => Program.TDValidateListRange(arr, 3, 2, "Start > End"));
        }

        [Test]
        public void FunctionalTest_HeapSort()
        {
            List<object> arr = new List<object> { 10, 5, 8, 2, 1, 6, 3, 4, 7, 9 };
            Program.TDHeapSort(arr, 0, arr.Count - 1, Program.TDCompareLongint);
            ClassicAssert.AreEqual(new List<object> { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 }, arr);
        }

        [Test]
        public void UnitTest_HSTrickleDown()
        {
            object[] arr = [10, 5, 8, 2, 1, 6, 3, 4, 7, 9];
            Program.HSTrickleDown(arr, 0, arr.Length, Program.TDCompareLongint);
            ClassicAssert.AreEqual(new List<object> { 10, 9, 8, 7, 1, 6, 3, 4, 2, 5 }, arr);
        }

        [Test]
        public void UnitTest_HSTrickleDownStd()
        {
            object[] arr = [10, 5, 8, 2, 1, 6, 3, 4, 7, 9];
            Program.HSTrickleDownStd(arr, 0, arr.Length, Program.TDCompareLongint);
            ClassicAssert.AreEqual(new List<object> { 10, 9, 8, 4, 7, 6, 3, 2, 1, 5 }, arr);
        }

        [Test]
        public void UnitTest_TDCompareLongint()
        {
            ClassicAssert.AreEqual(-1, Program.TDCompareLongint(1, 2));
            ClassicAssert.AreEqual(0, Program.TDCompareLongint(5, 5));
            ClassicAssert.AreEqual(1, Program.TDCompareLongint(10, 7));
        }

        [Test]
        public void EdgeCase_EmptyList()
        {
            List<object> arr = new List<object>();
            Program.TDHeapSort(arr, 0, arr.Count - 1, Program.TDCompareLongint);
            ClassicAssert.IsEmpty(arr);
        }

        [Test]
        public void EdgeCase_SingleElementList()
        {
            List<object> arr = new List<object> { 5 };
            Program.TDHeapSort(arr, 0, arr.Count - 1, Program.TDCompareLongint);
            ClassicAssert.AreEqual(new List<object> { 5 }, arr);
        }

        [Test]
        public void ErrorHandling_NullList()
        {
            ClassicAssert.Throws<EtdTListException>(() => Program.TDHeapSort(null, 0, 0, Program.TDCompareLongint));
        }

        [Test]
        public void ErrorHandling_InvalidRange()
        {
            List<object> arr = new List<object> { 1, 2, 3, 4, 5 };
            ClassicAssert.Throws<EtdTListException>(() => Program.TDHeapSort(arr, 2, 1, Program.TDCompareLongint));
        }

        [Test]
        public void PerformanceTest_LargeList()
        {
            List<object> arr = new List<object>();
            int size = 100000;
            Random random = new Random();
            for (int i = 0; i < size; i++)
            {
                arr.Add(random.Next());
            }
            Program.TDHeapSort(arr, 0, arr.Count - 1, Program.TDCompareLongint);
            // Assert that the list is sorted
            for (int i = 0; i < size - 1; i++)
            {
                ClassicAssert.LessOrEqual((int)arr[i], (int)arr[i + 1]);
            }
        }
    }
}