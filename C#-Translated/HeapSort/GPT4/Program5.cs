namespace HeapSort10; // was not created I did it manually

using System;
using System.Collections.Generic;

class EtdTListException : Exception
{
    public EtdTListException(string message) : base(message) { }
}

delegate int TtdCompareFunc(object aData1, object aData2);

class Program
{
    static void TDValidateListRange(List<object> aList, int aStart, int aEnd, string aMessage)
    {
        if (aList == null)
            throw new EtdTListException($"List is nil:: {aMessage}");
        if (aStart < 0 || aStart >= aList.Count || aEnd < 0 || aEnd >= aList.Count || aStart > aEnd)
            throw new EtdTListException($"Invalid range: {aStart}-{aEnd}: {aMessage}");
    }

    static void HSTrickleDown(object[] aList, int aFromInx, int aCount, TtdCompareFunc aCompare)
    {
        object Item = aList[aFromInx];
        int ChildInx = (aFromInx * 2) + 1;
        while (ChildInx < aCount)
        {
            if ((ChildInx + 1) < aCount && aCompare(aList[ChildInx], aList[ChildInx + 1]) < 0)
                ChildInx++;
            aList[aFromInx] = aList[ChildInx];
            aFromInx = ChildInx;
            ChildInx = (aFromInx * 2) + 1;
        }

        int ParentInx = (aFromInx - 1) / 2;
        while (aFromInx > 0 && aCompare(Item, aList[ParentInx]) > 0)
        {
            aList[aFromInx] = aList[ParentInx];
            aFromInx = ParentInx;
            ParentInx = (aFromInx - 1) / 2;
        }
        aList[aFromInx] = Item;
    }

    static void HSTrickleDownStd(object[] aList, int aFromInx, int aCount, TtdCompareFunc aCompare)
    {
        object Item = aList[aFromInx];
        int ChildInx = (aFromInx * 2) + 1;
        while (ChildInx < aCount)
        {
            if ((ChildInx + 1) < aCount && aCompare(aList[ChildInx], aList[ChildInx + 1]) < 0)
                ChildInx++;
            if (aCompare(Item, aList[ChildInx]) >= 0)
                break;
            aList[aFromInx] = aList[ChildInx];
            aFromInx = ChildInx;
            ChildInx = (aFromInx * 2) + 1;
        }
        aList[aFromInx] = Item;
    }

    static void TDHeapSort(List<object> aList, int aFirst, int aLast, TtdCompareFunc aCompare)
    {
        TDValidateListRange(aList, aFirst, aLast, "TDHeapSort");
        int ItemCount = aLast - aFirst + 1;
        object[] listArray = aList.ToArray();
        for (int Inx = (ItemCount / 2) - 1; Inx >= 0; Inx--)
            HSTrickleDownStd(listArray, Inx, ItemCount, aCompare);

        for (int Inx = ItemCount - 1; Inx >= 0; Inx--)
        {
            object Temp = listArray[0];
            listArray[0] = listArray[Inx];
            listArray[Inx] = Temp;
            HSTrickleDown(listArray, 0, Inx, aCompare);
        }

        for (int i = 0; i < listArray.Length; i++)
            aList[i] = listArray[i];
    }

    static int TDCompareLongint(object aData1, object aData2)
    {
        int L1 = Convert.ToInt32(aData1);
        int L2 = Convert.ToInt32(aData2);
        if (L1 < L2)
            return -1;
        else if (L1 == L2)
            return 0;
        else
            return 1;
    }

    static void Main()
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

            TDHeapSort(arr, 0, arr.Count - 1, new TtdCompareFunc(TDCompareLongint));

            foreach (var item in arr)
            {
                Console.WriteLine(item);
            }
        }
        catch (Exception ex)
        {
            Console.WriteLine(ex.Message);
        }
    }
}