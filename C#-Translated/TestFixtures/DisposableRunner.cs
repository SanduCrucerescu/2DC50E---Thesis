namespace TestFixtures;

using System;
using System.IO;
using System.Reflection;

public class DisposableRunner : IDisposable
{
    private StringWriter sw;

    public DisposableRunner(string fullyQualifiedClassName, string method, object[] paramsArr = null)
    {
        sw = new StringWriter();
        Type type = Type.GetType(fullyQualifiedClassName);

        if (type != null)
        {
            object obj = Activator.CreateInstance(type);
            MethodInfo objMethod = type.GetMethod(method);

            if (objMethod != null)
            {
                Console.SetOut(sw);
                objMethod.Invoke(obj, paramsArr);
            }
        }
    }

    public string GetOutput()
    {
        return sw.ToString().Trim();
    }

    public void Dispose()
    {
        sw.Dispose();
    }
}
