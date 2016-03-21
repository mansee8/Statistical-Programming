import java.io.*;
import java.util.*;
class LexicographicalOrdering
{
public static void main(String args[])
{
Scanner cin=new Scanner(System.in);
System.out.println("Enter the String whose rank in to be found");
String s=cin.next();
int rank=findrank(s);
}
public static int findrank(String s)
{
int n=s.length();
char chars[]=s.toCharArray();
char sorteds[]=Arrays.sort(chars);
for(int i=1;i<n;i++)
{
//fixing the first element and then getting all the permutations
System.out.println(sorteds[i]);
}

}


}


