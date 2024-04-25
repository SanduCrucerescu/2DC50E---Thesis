using NUnit.Framework;
using NUnit.Framework.Legacy;
using static HeapSort1.HeapSort;

namespace HeapSort1.Test
{
    [TestFixture]
    public class HeapSortTests
    {
        [Test]
        public void SmokeTest()
        {
            List<object> arr = new List<object> { 5, 876, 234, 9, 12, 44, 2 };
            HeapSort.TDHeapSort(arr, 0, arr.Count - 1, HeapSort.TDCompareLongint);
            ClassicAssert.AreEqual(new List<object> { 2, 5, 9, 12, 44, 234, 876 }, arr);
        }

        [Test]
        public void UnitTest_TDValidateListRange()
        {
            List<object> arr = new List<object> { 1, 2, 3, 4, 5 };
            ClassicAssert.DoesNotThrow(() => HeapSort.TDValidateListRange(arr, 0, 4, "Valid range"));
            ClassicAssert.Throws<EtdTListException>(() => HeapSort.TDValidateListRange(null, 0, 4, "Null list"));
            ClassicAssert.Throws<EtdTListException>(() => HeapSort.TDValidateListRange(arr, -1, 4, "Invalid start index"));
            ClassicAssert.Throws<EtdTListException>(() => HeapSort.TDValidateListRange(arr, 0, 5, "Invalid end index"));
            ClassicAssert.Throws<EtdTListException>(() => HeapSort.TDValidateListRange(arr, 3, 2, "Start > End"));
        }

        [Test]
        public void FunctionalTest_HeapSort()
        {
            List<object> arr = new List<object> { 10, 5, 8, 2, 1, 6, 3, 4, 7, 9 };
            HeapSort.TDHeapSort(arr, 0, arr.Count - 1, HeapSort.TDCompareLongint);
            ClassicAssert.AreEqual(new List<object> { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 }, arr);
        }

        [Test]
        public void UnitTest_HSTrickleDown()
        {
            object[] arr = [10, 5, 8, 2, 1, 6, 3, 4, 7, 9];
            HeapSort.HSTrickleDown(arr, 0, arr.Length, HeapSort.TDCompareLongint);
            ClassicAssert.AreEqual(new List<object> { 10, 9, 8, 7, 1, 6, 3, 4, 2, 5 }, arr);
        }

        [Test]
        public void UnitTest_HSTrickleDownStd()
        {
            object[] arr = [10, 5, 8, 2, 1, 6, 3, 4, 7, 9];
            HeapSort.HSTrickleDownStd(arr, 0, arr.Length, HeapSort.TDCompareLongint);
            ClassicAssert.AreEqual(new List<object> { 10, 9, 8, 4, 7, 6, 3, 2, 1, 5 }, arr);
        }

        [Test]
        public void UnitTest_TDCompareLongint()
        {
            ClassicAssert.AreEqual(-1, HeapSort.TDCompareLongint(1, 2));
            ClassicAssert.AreEqual(0, HeapSort.TDCompareLongint(5, 5));
            ClassicAssert.AreEqual(1, HeapSort.TDCompareLongint(10, 7));
        }

        [Test]
        public void EdgeCase_EmptyList()
        {
            List<object> arr = new List<object>();
            HeapSort.TDHeapSort(arr, 0, arr.Count - 1, HeapSort.TDCompareLongint);
            ClassicAssert.IsEmpty(arr);
        }

        [Test]
        public void EdgeCase_SingleElementList()
        {
            List<object> arr = new List<object> { 5 };
            HeapSort.TDHeapSort(arr, 0, arr.Count - 1, HeapSort.TDCompareLongint);
            ClassicAssert.AreEqual(new List<object> { 5 }, arr);
        }

        [Test]
        public void ErrorHandling_NullList()
        {
            ClassicAssert.Throws<EtdTListException>(() => HeapSort.TDHeapSort(null, 0, 0, HeapSort.TDCompareLongint));
        }

        [Test]
        public void ErrorHandling_InvalidRange()
        {
            List<object> arr = new List<object> { 1, 2, 3, 4, 5 };
            ClassicAssert.Throws<EtdTListException>(() => HeapSort.TDHeapSort(arr, 2, 1, HeapSort.TDCompareLongint));
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
            HeapSort.TDHeapSort(arr, 0, arr.Count - 1, HeapSort.TDCompareLongint);
            // Assert that the list is sorted
            for (int i = 0; i < size - 1; i++)
            {
                ClassicAssert.LessOrEqual((int)arr[i], (int)arr[i + 1]);
            }
        }
    }
}