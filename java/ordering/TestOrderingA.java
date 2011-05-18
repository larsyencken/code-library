import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

class TestOrderingA implements Comparable<TestOrderingA>{

        
    public int value;

    public TestOrderingA(int i){
        value = i;
    }
    
    public static void print(ArrayList<TestOrderingA> list){
        for (TestOrderingA t: list){
            System.out.println(t.value);
        }
    }

    public static void main(String[] arr){
        System.out.println("TestOrderingA (hell)");
        ArrayList<TestOrderingA> list = new ArrayList<TestOrderingA>();
        list.add(new TestOrderingA(2));
        list.add(new TestOrderingA(1));
        list.add(new TestOrderingA(3));
        print(list);    
        Collections.sort(list);
        print(list);
    }
    
    public int compareTo(TestOrderingA rhs){
        if (value == rhs.value){
            return 0;
        }
        return (value < rhs.value) ? -1 : 1;
    }
   
}
