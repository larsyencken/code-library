//
// testUnicode.cs
//
// Tests some unicode stuff on the console.
//

using System;
using System.Text;
 
public class HelloWorld {
	public static void Main(string[] args)
	{
        string s = "お酒を飲む?";
        Console.WriteLine(s);
        Console.WriteLine("The length is {0}", s.Length.ToString());
        char c = s[0];

        for (int i = 0; i < 20; i++)
        {
            char k = (char) (c + i);
            Console.Write(k.ToString());
        }
        Console.WriteLine();
	}
}
