namespace TestFixtures;

[TestFixture]
public class HeapSortTests
{
    [Test]
    public void SmokeTest()
    {
        // Smoke test to ensure the basic functionality works
        List<object> arr = new List<object> { 5, 876, 234, 9, 12, 44, 2 };
        HeapSortUtils.TDHeapSort(arr, 0, arr.Count - 1, HeapSortUtils.TDCompareLongint);
        Assert.AreEqual(new List<object> { 2, 5, 9, 12, 44, 234, 876 }, arr);
    }

    [Test]
    public void UnitTest_TDValidateListRange()
    {
        // Unit test for TDValidateListRange method
        List<object> arr = new List<object> { 1, 2, 3, 4, 5 };
        Assert.DoesNotThrow(() => HeapSortUtils.TDValidateListRange(arr, 0, 4, "Valid range"));
        Assert.Throws<EtdTListException>(() => HeapSortUtils.TDValidateListRange(null, 0, 4, "Null list"));
        Assert.Throws<EtdTListException>(() => HeapSortUtils.TDValidateListRange(arr, -1, 4, "Invalid start index"));
        Assert.Throws<EtdTListException>(() => HeapSortUtils.TDValidateListRange(arr, 0, 5, "Invalid end index"));
        Assert.Throws<EtdTListException>(() => HeapSortUtils.TDValidateListRange(arr, 3, 2, "Start > End"));
    }

    [Test]
    public void FunctionalTest_HeapSort()
    {
        // Functional test for the heap sort algorithm
        List<object> arr = new List<object> { 10, 5, 8, 2, 1, 6, 3, 4, 7, 9 };
        HeapSortUtils.TDHeapSort(arr, 0, arr.Count - 1, HeapSortUtils.TDCompareLongint);
        Assert.AreEqual(new List<object> { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 }, arr);
    }

    [Test]
    public void UnitTest_HSTrickleDown()
    {
        // Unit test for HSTrickleDown method
        List<object> arr = new List<object> { 10, 5, 8, 2, 1, 6, 3, 4, 7, 9 };
        HeapSortUtils.HSTrickleDown(arr, 0, arr.Count, HeapSortUtils.TDCompareLongint);
        Assert.AreEqual(new List<object> { 10, 9, 8, 7, 1, 6, 3, 4, 2, 5 }, arr);
    }

    [Test]
    public void UnitTest_HSTrickleDownStd()
    {
        // Unit test for HSTrickleDownStd method
        List<object> arr = new List<object> { 10, 5, 8, 2, 1, 6, 3, 4, 7, 9 };
        HeapSortUtils.HSTrickleDownStd(arr, 0, arr.Count, HeapSortUtils.TDCompareLongint);
        Assert.AreEqual(new List<object> { 10, 9, 8, 4, 7, 6, 3, 2, 1, 5 }, arr);
    }

    [Test]
    public void UnitTest_TDCompareLongint()
    {
        // Unit test for TDCompareLongint method
        Assert.AreEqual(-1, HeapSortUtils.TDCompareLongint(1, 2));
        Assert.AreEqual(0, HeapSortUtils.TDCompareLongint(5, 5));
        Assert.AreEqual(1, HeapSortUtils.TDCompareLongint(10, 7));
    }

    [Test]
    public void EdgeCase_EmptyList()
    {
        // Edge case test for sorting an empty list
        List<object> arr = new List<object>();
        HeapSortUtils.TDHeapSort(arr, 0, arr.Count - 1, HeapSortUtils.TDCompareLongint);
        Assert.IsEmpty(arr);
    }

    [Test]
    public void EdgeCase_SingleElementList()
    {
        // Edge case test for sorting a list with a single element
        List<object> arr = new List<object> { 5 };
        HeapSortUtils.TDHeapSort(arr, 0, arr.Count - 1, HeapSortUtils.TDCompareLongint);
        Assert.AreEqual(new List<object> { 5 }, arr);
    }

    [Test]
    public void ErrorHandling_NullList()
    {
        // Error handling test for passing a null list
        Assert.Throws<EtdTListException>(() => HeapSortUtils.TDHeapSort(null, 0, 0, HeapSortUtils.TDCompareLongint));
    }

    [Test]
    public void ErrorHandling_InvalidRange()
    {
        // Error handling test for passing an invalid range
        List<object> arr = new List<object> { 1, 2, 3, 4, 5 };
        Assert.Throws<EtdTListException>(() => HeapSortUtils.TDHeapSort(arr, 2, 1, HeapSortUtils.TDCompareLongint));
    }

    [Test]
    public void PerformanceTest_LargeList()
    {
        // Performance test for sorting a large list
        List<object> arr = new List<object>();
        int size = 100000;
        Random random = new Random();
        for (int i = 0; i < size; i++)
        {
            arr.Add(random.Next());
        }
        HeapSortUtils.TDHeapSort(arr, 0, arr.Count - 1, HeapSortUtils.TDCompareLongint);
        // Assert that the list is sorted
        for (int i = 0; i < size - 1; i++)
        {
            Assert.LessOrEqual((int)arr[i], (int)arr[i + 1]);
        }
    }
}
