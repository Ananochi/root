public class TestLocking1 {
  public static void main(String[] args) {
    DoubleArrayList dal1 = new DoubleArrayList();
    dal1.add(42.1); dal1.add(7.2); dal1.add(9.3); dal1.add(13.4); 
    dal1.set(2, 11.3);
    for (int i=0; i<dal1.size(); i++)
      System.out.println(dal1.get(i));

    DoubleArrayList dal2 = new DoubleArrayList();
    dal2.add(90.1); dal2.add(80.2); dal2.add(70.3); dal2.add(60.4); dal2.add(50.5);
    for (int i=0; i<dal2.size(); i++)
      System.out.println(dal2.get(i));

    DoubleArrayList dal3 = new DoubleArrayList();
    dal3.add(1.1); dal3.add(2.2); dal3.add(3.3); dal3.add(4.4); dal3.add(5.5);
    for (int i=0; i<dal3.size(); i++)
      System.out.println(dal3.get(i));
  }
}

// Expandable array list of doubles.

class DoubleArrayList {
  // Invariant: 0 <= size <= items.length
  private volatile double[] items = new double[2];
  private volatile int size = 0;

  // Number of items in the double list
  public synchronized int size() {
    return size;
  }

  // Return item number i, if any
  public synchronized double get(int i) {
    if (0 <= i && i < size) 
      return items[i];
    else 
      throw new IndexOutOfBoundsException(String.valueOf(i));
  }

  // Add item x to end of list
  public synchronized boolean add(double x) {
    if (size == items.length) {
      double[] newItems = new double[items.length * 2];
      for (int i=0; i<items.length; i++)
	newItems[i] = items[i];
      items = newItems;
    }
    items[size] = x;
    size++;
    return true;
  }

  // Replace item number i, if any, with x
  public synchronized double set(int i, double x) {
    if (0 <= i && i < size) {
      double old = items[i];
      items[i] = x;
      return old;
    } else 
      throw new IndexOutOfBoundsException(String.valueOf(i));
  }

  // The double list formatted as eg "[3.2, 4.7]"
  public synchronized String toString() {
    StringBuilder sb = new StringBuilder("[");
    for (int i=0; i<size; i++)
      sb.append(i > 0 ? ", " : "").append(items[i]);
    return sb.append("]").toString();
  }
}
