// For week 9
// sestoft@itu.dk * 2014-10-28

public class TestHSQueues {
  public static void main(String[] args) {


  }
}

// Thread-safe lock-based queue a la Herlihy & Shavit fig 3.1.  Same
// interface as Goetz's version of the Michael and Scott queue, except
// for the method names.

class LockBasedQueue<T> {
  private final T[] items;
  private int tail = 0, head = 0;
  
  public LockBasedQueue(int capacity) {
    this.items = makeArray(capacity);
  }
  
  @SuppressWarnings("unchecked")   
  private static <T> T[] makeArray(int capacity) {
    // Java's @$#@?!! type system requires this unsafe cast    
    return (T[])new Object[capacity];
  }

  public synchronized boolean enq(T item) { // at tail
    if (tail - head == items.length)
      return false;
    else {
      items[tail % items.length] = item;
      tail++;
      return true; 
    }
  }

  public synchronized T deq() {         // from head
    if (tail == head) 
      return null;
    else {
      T item = items[head % items.length];
      head++;
      return item;
    } 
  }
}


// Single-enqueuer single-dequeuer waitfree queue a la Herlihy &
// Shavit fig 3.2.  

class WaitFreeQueue<T> {
  private final T[] items;
  private volatile int tail = 0, head = 0;
  
  public WaitFreeQueue(int capacity) {
    this.items = makeArray(capacity);
  }
  
  @SuppressWarnings("unchecked")   
  private static <T> T[] makeArray(int capacity) {
    // Java's @$#@?!! type system requires this unsafe cast    
    return (T[])new Object[capacity];
  }

  public boolean enq(T item) { // at tail
    if (tail - head == items.length)
      return false;
    else {
      items[tail % items.length] = item;
      tail++;
      return true; 
    }
  }

  public T deq() {         // from head
    if (tail == head) 
      return null;
    else {
      T item = items[head % items.length];
      head++;
      return item;
    } 
  }
}
