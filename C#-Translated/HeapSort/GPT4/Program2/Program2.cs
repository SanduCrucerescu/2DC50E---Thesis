namespace HeapSort7; // was not created I did it manually

using System;
using System.Collections.Generic;

class Program
{
    internal class EtdTListException : Exception
    {
        public EtdTListException(string message) : base(message)
        {
        }
    }

    internal delegate int TtdCompareFunc(object data1, object data2);

    internal static void TDValidateListRange(List<object> list, int start, int end, string message)
    {
        if (list == null)
            throw new EtdTListException($"List is nil:: {message}");
        if (start < 0 || start >= list.Count || end < 0 || end >= list.Count || start > end)
            throw new EtdTListException($"Invalid range: {start}-{end}: {message}");
    }

    internal static void HSTrickleDown(List<object> list, int fromIndex, int count, TtdCompareFunc compare)
    {
        object item = list[fromIndex];
        int childIndex = (fromIndex * 2) + 1;
        while (childIndex < count)
        {
            if ((childIndex + 1 < count) && (compare(list[childIndex], list[childIndex + 1]) < 0))
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

    internal static void HSTrickleDownStd(List<object> list, int fromIndex, int count, TtdCompareFunc compare)
    {
        object item = list[fromIndex];
        int childIndex = (fromIndex * 2) + 1;
        while (childIndex < count)
        {
            if ((childIndex + 1 < count) && (compare(list[childIndex], list[childIndex + 1]) < 0))
                childIndex++;
            if (compare(item, list[childIndex]) >= 0)
                break;
            list[fromIndex] = list[childIndex];
            fromIndex = childIndex;
            childIndex = (fromIndex * 2) + 1;
        }
        list[fromIndex] = item;
    }

    internal static void TDHeapSort(List<object> list, int first, int last, TtdCompareFunc compare)
    {
        TDValidateListRange(list, first, last, "TDHeapSort");
        int itemCount = last - first + 1;
        for (int i = (itemCount / 2) - 1; i >= 0; i--)
            HSTrickleDownStd(list, i, itemCount, compare);
        for (int i = itemCount - 1; i >= 0; i--)
        {
            object temp = list[first];
            list[first] = list[first + i];
            list[first + i] = temp;
            HSTrickleDown(list, first, i, compare);
        }
    }

    internal static int TDCompareLongint(object data1, object data2)
    {
        int l1 = (int)data1;
        int l2 = (int)data2;
        if (l1 < l2)
            return -1;
        else if (l1 == l2)
            return 0;
        else
            return 1;
    }

    static void Main(string[] args)
    {
        List<object> arr = new List<object>();

        try
        {
            arr.Add(5);
            arr.Add(876);
            arr.Add(234);
            arr.Add(9);
            arr.Add(12);
            arr.Add(44);
            arr.Add(2);

            TDHeapSort(arr, 0, arr.Count - 1, TDCompareLongint);

            foreach (int item in arr)
                Console.WriteLine(item);
        }
        finally
        {
            arr.Clear();
        }
    }
}