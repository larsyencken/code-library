import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

class TestOrderingB {
        
    public int value;

    public TestOrderingB(int i){
        value = i;
    }

    public static void main(String[] arr){
        System.out.println("TestOrderingB (hellicious)");
        ArrayList<TestOrderingB> list = new ArrayList<TestOrderingB>();
        list.add(new TestOrderingB(2));
        list.add(new TestOrderingB(1));
        list.add(new TestOrderingB(3));
        
        for (TestOrderingB t: list){
            System.out.println(t.value);

        }
        Collections.sort(list, new Cmp());
        for (TestOrderingB t: list){
            System.out.println(t.value);

        }

    }
    
    static class Cmp implements Comparator<TestOrderingB>{
        
        public int compare(TestOrderingB rhs, TestOrderingB lhs){
            if (lhs.value == rhs.value){
                return 0;
            }
            return (lhs.value < rhs.value) ? -1 : 1;
        }
         
    } 
}
