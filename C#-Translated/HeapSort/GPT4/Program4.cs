namespace HeapSort9; // was not created I did it manually

using System;
using System.Collections.Generic;

// Exception class for List validation
public class EtdTListException : Exception
{
    public EtdTListException(string message) : base(message) { }
}

public class HeapSortProgram
{
    // Delegate for comparison function
    public delegate int TtdCompareFunc(object data1, object data2);

    // Validates the range of the list
    public static void TDValidateListRange(List<object> list, int start, int end, string message)
    {
        if (list == null)
            throw new EtdTListException($"List is nil:: {message}");
        if (start < 0 || start >= list.Count || end < 0 || end >= list.Count || start > end)
            throw new EtdTListException($"Invalid range: {start}-{end}: {message}");
    }

    // Trickle down operation in heap
    public static void HSTrickleDown(List<object> list, int fromIndex, int count, TtdCompareFunc compare)
    {
        object item = list[fromIndex];
        int childIndex = (fromIndex * 2) + 1;
        while (childIndex < count)
        {
            if ((childIndex + 1) < count && compare(list[childIndex], list[childIndex + 1]) < 0)
                childIndex++;
            list[fromIndex] = list[childIndex];
            fromIndex = childIndex;
            childIndex = (fromIndex * 2) + 1;
        }

        int parentIndex = (fromIndex - 1) / 2;
        while (fromIndex > 0 && compare(item, list[parentIndex]) > 0)
        {
            list[fromIndex] = list[parentIndex];
            fromIndex = parentIndex;
            parentIndex = (fromIndex - 1) / 2;
        }
        list[fromIndex] = item;
    }

    // Standard trickle down using a comparison function
    public static void HSTrickleDownStd(List<object> list, int fromIndex, int count, TtdCompareFunc compare)
    {
        object item = list[fromIndex];
        int childIndex = (fromIndex * 2) + 1;
        while (childIndex < count)
        {
            if ((childIndex + 1) < count && compare(list[childIndex], list[childIndex + 1]) < 0)
                childIndex++;
            if (compare(item, list[childIndex]) >= 0)
                break;
            list[fromIndex] = list[childIndex];
            fromIndex = childIndex;
            childIndex = (fromIndex * 2) + 1;
        }
        list[fromIndex] = item;
    }

    // Heap sort algorithm
    public static void TDHeapSort(List<object> list, int first, int last, TtdCompareFunc compare)
    {
        TDValidateListRange(list, first, last, "TDHeapSort");
        int itemCount = last - first + 1;
        for (int i = (itemCount / 2) - 1; i >= 0; i--)
            HSTrickleDownStd(list, first + i, itemCount, compare);

        for (int i = itemCount - 1; i >= 0; i--)
        {
            object temp = list[first];
            list[first] = list[first + i];
            list[first + i] = temp;
            HSTrickleDown(list, first, i, compare);
        }
    }

    // Comparison function for int
    public static int TDCompareLongint(object data1, object data2)
    {
        int l1 = (int)data1;
        int l2 = (int)data2;
        return l1.CompareTo(l2);
    }

    public static void Main()
    {
        List<object> arr = new List<object> { 5, 876, 234, 9, 12, 44, 2 };

        try
        {
            TDHeapSort(arr, 0, arr.Count - 1, TDCompareLongint);

            foreach (var item in arr)
                Console.WriteLine((int)item);
        }
        finally
        {
            // Cleanup if necessary
        }
    }
}