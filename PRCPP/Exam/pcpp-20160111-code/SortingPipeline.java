// Pipelined sorting using P>=1 stages, each maintaining an internal
// collection of size S>=1.  Stage 1 contains the largest items, stage
// 2 the second largest, ..., stage P the smallest ones.  In each
// stage, the internal collection of items is organized as a minheap.
// When a stage receives an item x and its collection is not full, it
// inserts it in the heap.  If the collection is full and x is less
// than or equal to the collections's least item, it forwards the item
// to the next stage; otherwise forwards the collection's least item
// and inserts x into the collection instead.

// When there are itemCount items and stageCount stages, each stage
// must be able to hold at least ceil(itemCount/stageCount) items,
// which equals (itemCount-1)/stageCount+1.

// sestoft@itu.dk * 2016-01-10

import org.multiverse.api.StmUtils;
import org.multiverse.api.Txn;
import org.multiverse.api.references.TxnDouble;
import org.multiverse.api.references.TxnInteger;
import org.multiverse.api.references.TxnRef;

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.IntToDoubleFunction;

import static org.multiverse.api.StmUtils.*;

/** ------------------------------------------- **/
/** CLASS SortingPipeline       (le main...)    **/
/** ------------------------------------------- **/
public class SortingPipeline {
  public static void main(String[] args) throws InterruptedException {
    SystemInfo();
    final int count = 100_000;
    final int P = 4;
    final double[] arr = DoubleArray.randomPermutation(count);
    final BlockingDoubleQueue[] queues = new BlockingDoubleQueue[P+1];

    for (int i=0; i<queues.length; i++) {
      queues[i] = new WrappedArrayDoubleQueue();
      //queues[i] = new BlockingNDoubleQueue(50);
      //queues[i] = new StmBlockingNDoubleQueue(50);
    }

    sortPipeline(arr,P,queues);

    // Benchmarking
/*    System.out.println(Mark7("sortPipeline", i -> {
      try {
        return sortPipeline(arr, P, queues);
      } catch (InterruptedException e) {
        e.printStackTrace();
      }
    return 0;
    }));*/
  }

  private static int sortPipeline(double[] arr, int P, BlockingDoubleQueue[] queues) throws InterruptedException {

    int threadCount = P + 2;
    int stageCount = 1;
    Thread[] threads = new Thread[threadCount];
    int colSize = arr.length / P;

    // creating of P+2 threads
    for (int i=0; i < threadCount; i++) {

      // DoubleGenerator instance
      if (i==0) {
        threads[i] = new Thread(new DoubleGenerator(arr, arr.length, queues[i]));
        threads[i].start();
        //System.out.println(threads[i].getName() + " has started!");
      }

      // SortedChecker instance
      else if (i==threadCount-1){
        threads[i] = new Thread(new SortedChecker(arr.length, queues[i-1]));
        threads[i].start();
        //System.out.println(threads[i].getName() + " has started!");
      }

      // Sorting instance
      else {
        threads[i] = new Thread(new SortingStage(queues[i-1], queues[i], colSize, P, stageCount));
        threads[i].start();
        //System.out.println(threads[i].getName() + " has started!");
        stageCount++;
      }
    }

    // waiting for threads to complete ...
    try {
      for (int i=0; i<threadCount; i++) {
        threads[i].join();
        //System.out.println(threads[i].getName() + " joined!");
      }
    } catch (InterruptedException exn) {
      System.out.println("Something went wrong ...");
    }
    return 0;
  }

  /** ------------------------------------------- **/
  /** CLASS SortingStage                          **/
  /** ------------------------------------------- **/
  static class SortingStage implements Runnable {
    private final BlockingDoubleQueue input;
    private final BlockingDoubleQueue output;
    private final int colSize;
    private volatile int itemCount;
    private final double[] heap;
    private final int P;
    private final int I;

    public SortingStage(BlockingDoubleQueue input, BlockingDoubleQueue output, int colSize, int P, int I) {
      this.input = input;
      this.output = output;
      this.colSize = colSize;
      this.heap = new double[colSize];
      this.P = P;
      this.I = I;
      // the below is not a beauty ...
      this.itemCount =(heap.length*P) + (P - I) * colSize;

    }

    public void run() {

      int heapSize = 0;
      while (itemCount > 0) {

        double x = 0;
        try {
          x = input.take();
        } catch (InterruptedException e) {
          e.printStackTrace();
        }

        // heap not full, put "x" into it
        if (heapSize < heap.length) {
          heap[heapSize++] = x;
          DoubleArray.minheapSiftup(heap, heapSize-1, heapSize-1);
        }
        // "x" is small, forward
        else if (x <= heap[0]) {
          try {
            output.put(x);
          } catch (InterruptedException e) {
            e.printStackTrace();
          }
          itemCount--;
        }
        // forward least, replace with "x"
        else {
          double least = heap[0];
          heap[0] = x;
          DoubleArray.minheapSiftdown(heap, 0, heapSize-1);
          try {
            output.put(least);
          } catch (InterruptedException e) {
            e.printStackTrace();
          }
          itemCount--;
        }
      }
    }
  }

  /** ------------------------------------------- **/
  /** CLASS DoubleGenerator                       **/
  /** ------------------------------------------- **/
  static class DoubleGenerator implements Runnable {
    private final BlockingDoubleQueue output;
    private final double[] arr;  // The numbers to feed to output
    private final int infinites;

    public DoubleGenerator(double[] arr, int infinites, BlockingDoubleQueue output) {
      this.arr = arr;
      this.output = output;
      this.infinites = infinites;
    }

    public void run() { 
      for (int i=0; i<arr.length; i++)  // The numbers to sort
        try {
          output.put(arr[i]);
        } catch (InterruptedException e) {
          e.printStackTrace();
        }
      for (int i=0; i<infinites; i++)   // Infinite numbers for wash-out
        try {
          output.put(Double.POSITIVE_INFINITY);
        } catch (InterruptedException e) {
          e.printStackTrace();
        }
    }
  }

  /** ------------------------------------------- **/
  /** CLASS SortedChecker                         **/
  /** ------------------------------------------- **/
  static class SortedChecker implements Runnable {
    // If DEBUG is true, print the first 100 numbers received
    private final static boolean DEBUG = false;
    private final BlockingDoubleQueue input;
    private final int itemCount; // the number of items to check

    public SortedChecker(int itemCount, BlockingDoubleQueue input) {
      this.itemCount = itemCount;
      this.input = input;
    }

    public void run() { 
      int consumed = 0;
      double last = Double.NEGATIVE_INFINITY;
      while (consumed++ < itemCount) {
        double p = 0;
        try {
          p = input.take();
        } catch (InterruptedException e) {
          e.printStackTrace();
        }

        if (DEBUG && consumed <= 100) 
          System.out.print(p + " ");
        if (p <= last)
          System.out.printf("Elements out of order: %g before %g%n", last, p);
        last = p;
      }
      if (DEBUG)
        System.out.println();
    }
  }

  // --- Benchmarking infrastructure ---

  // NB: Modified to show milliseconds instead of nanoseconds

  public static double Mark7(String msg, IntToDoubleFunction f) {
    int n = 10, count = 1, totalCount = 0;
    double dummy = 0.0, runningTime = 0.0, st = 0.0, sst = 0.0;
    do { 
      count *= 2;
      st = sst = 0.0;
      for (int j=0; j<n; j++) {
        Timer t = new Timer();
        for (int i=0; i<count; i++)
          dummy += f.applyAsDouble(i);
        runningTime = t.check();
        double time = runningTime * 1e3 / count;
        st += time; 
        sst += time * time;
        totalCount += count;
      }
    } while (runningTime < 0.25 && count < Integer.MAX_VALUE/2);
    double mean = st/n, sdev = Math.sqrt((sst - mean*mean*n)/(n-1));
    System.out.printf("%-25s %15.1f ms %10.2f %10d%n", msg, mean, sdev, count);
    return dummy / totalCount;
  }

  public static void SystemInfo() {
    System.out.printf("# OS:   %s; %s; %s%n", 
                      System.getProperty("os.name"), 
                      System.getProperty("os.version"), 
                      System.getProperty("os.arch"));
    System.out.printf("# JVM:  %s; %s%n", 
                      System.getProperty("java.vendor"), 
                      System.getProperty("java.version"));
    // The processor identifier works only on MS Windows:
    System.out.printf("# CPU:  %s; %d \"cores\"%n", 
                      System.getenv("PROCESSOR_IDENTIFIER"),
                      Runtime.getRuntime().availableProcessors());
    java.util.Date now = new java.util.Date();
    System.out.printf("# Date: %s%n", 
      new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ").format(now));
  }

  // Crude wall clock timing utility, measuring time in seconds
   
  static class Timer {
    private long start, spent = 0;
    public Timer() { play(); }
    public double check() { return (System.nanoTime()-start+spent)/1e9; }
    public void pause() { spent += System.nanoTime()-start; }
    public void play() { start = System.nanoTime(); }
  }
}

// ----------------------------------------------------------------------

// Queue interface

/** ------------------------------------------- **/
/** Interface BlockingDoubleQueue               **/
/** ------------------------------------------- **/
interface BlockingDoubleQueue {
  double take() throws InterruptedException;
  void put(double item) throws InterruptedException;
  int size();
  boolean isEmpty();
  boolean isFull();
}

class WrappedArrayDoubleQueue implements BlockingDoubleQueue {

  final ArrayBlockingQueue<Double> myQueue = new ArrayBlockingQueue<Double>(50);

  @Override
  public double take() {
    try {
      return myQueue.take();
    } catch (InterruptedException e) {
      e.printStackTrace();
    }
    return 0;
  }

  @Override
  public void put(double item) {
    try {
      myQueue.put(item);
    } catch (InterruptedException e) {
      e.printStackTrace();
    }
  }

  @Override
  public int size() {
    return myQueue.size();
  }

  @Override
  public boolean isEmpty() {
    return false;
  }

  @Override
  public boolean isFull() {
    return false;
  }

}

class BlockingNDoubleQueue implements BlockingDoubleQueue {

  final double[] myQueue;
  volatile int head;
  volatile int tail;
  volatile int size;
  volatile int availableItems;
  volatile int availableSpaces;

  public BlockingNDoubleQueue(int capacity) {

    this.myQueue = new double[capacity];
    this.size = 0;
    this.tail = 0;
    this.head = 0;
    this.availableSpaces = capacity;
    this.availableItems = 0;
  }

  @Override
  public synchronized double take() throws InterruptedException {

    while (availableItems == 0) {
      wait();
    }

    availableItems--;
    double item = myQueue[head];
    head = (head + 1) % myQueue.length;
    availableSpaces++;

    notifyAll();

    return item;

  }

  @Override
  public synchronized void put(double item) throws InterruptedException {

    while (availableSpaces == 0) {
      wait();
    }

    availableSpaces--;
    myQueue[tail] = item;
    tail = (tail + 1) % myQueue.length;
    availableItems++;

    notifyAll();

  }

  @Override
  public synchronized int size() {
    return size;
  }

  @Override
  public boolean isEmpty() {
    return false;
  }

  @Override
  public boolean isFull() {
    return false;
  }
}

class StmBlockingNDoubleQueue implements BlockingDoubleQueue {

  private final TxnInteger availableItems, availableSpaces;
  private volatile double[] items;
  private final TxnInteger head, tail;

  public StmBlockingNDoubleQueue(int capacity) {

    this.availableItems = newTxnInteger(0);
    this.availableSpaces = newTxnInteger(capacity);
    this.items = new double[capacity];
    this.head = newTxnInteger(0);
    this.tail = newTxnInteger(0);

  }

  @Override
  public double take() {
    return atomic(() -> {
      if (availableItems.get() == 0) {
        retry();
        return null;    // unreachable
      } else {
        availableItems.decrement();
        double item = items[head.get()];
        head.set((head.get() + 1) % items.length);
        availableSpaces.increment();
        return item;
      }
    });
  }

  @Override
  public void put(double item) throws InterruptedException {

    atomic(() -> {
      if (availableSpaces.get() == 0)
        retry();
      else {
        availableSpaces.decrement();
        items[tail.get()] = item;
        tail.set((tail.get() + 1) % items.length);
        availableItems.increment();
      }
    });
  }

  @Override
  public int size() {
    return 0;
  }

  @Override
  public boolean isEmpty() {
    return atomic(() -> availableItems.get() == 0);
  }

  @Override
  public boolean isFull() {
    return atomic(() -> availableSpaces.get() == 0);
  }
}

// ----------------------------------------------------------------------

/** ------------------------------------------- **/
/** CLASS DoubleArray                           **/
/** ------------------------------------------- **/
class DoubleArray {
  public static double[] randomPermutation(int n) {
    double[] arr = fillDoubleArray(n);
    shuffle(arr);
    return arr;
  }

  private static double[] fillDoubleArray(int n) {
    double[] arr = new double[n];
    for (int i = 0; i < n; i++)
      arr[i] = i + 0.1;
    return arr;
  }

  private static final java.util.Random rnd = new java.util.Random();

  private static void shuffle(double[] arr) {
    for (int i = arr.length-1; i > 0; i--)
      swap(arr, i, rnd.nextInt(i+1));
  }

  // Swap arr[s] and arr[t]
  private static void swap(double[] arr, int s, int t) {
    double tmp = arr[s]; arr[s] = arr[t]; arr[t] = tmp;
  }

  // Minheap operations for parallel sort pipelines.  
  // Minheap invariant: 
  // If heap[0..k-1] is a minheap, then heap[(i-1)/2] <= heap[i] for
  // all indexes i=1..k-1.  Thus heap[0] is the smallest element.

  // Although stored in an array, the heap can be considered a tree
  // where each element heap[i] is a node and heap[(i-1)/2] is its
  // parent. Then heap[0] is the tree's root and a node heap[i] has
  // children heap[2*i+1] and heap[2*i+2] if these are in the heap.

  // In heap[0..k], move node heap[i] downwards by swapping it with
  // its smallest child until the heap invariant is reestablished.

  public static void minheapSiftdown(double[] heap, int i, int k) {
    int child = 2 * i + 1;                          
    if (child <= k) {
      if (child+1 <= k && heap[child] > heap[child+1])
        child++;                                  
      if (heap[i] > heap[child]) {
        swap(heap, i, child); 
        minheapSiftdown(heap, child, k); 
      }
    }
  }

  // In heap[0..k], move node heap[i] upwards by swapping with its
  // parent until the heap invariant is reestablished.
  public static void minheapSiftup(double[] heap, int i, int k) {
    if (0 < i) {
      int parent = (i - 1) / 2;
      if (heap[i] < heap[parent]) {
        swap(heap, i, parent); 
        minheapSiftup(heap, parent, k); 
      }
    }
  }
}
