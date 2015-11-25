// For week 8
// 2014-10-31

// Bounded queue with blocking operations based on Semaphore, and
// associated put-take test, both by Brian Goetz and Tim Peierls, some
// modifications by sestoft@itu.dk

import java.util.Random;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.Semaphore;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicInteger;

public class TestBoundedQueueTest extends Tests{
  public static void main(String[] args) throws Exception {
    sequentialTest(new SemaphoreBoundedQueue<Integer>(3));
    parallelTest(new SemaphoreBoundedQueue<Integer>(10));
    // sequentialTest(new LockingBoundedQueue<Integer>(3));
    // parallelTest(new LockingBoundedQueue<Integer>(10));
  }

  private static void sequentialTest(BoundedQueue<Integer> bq) throws Exception {
    System.out.printf("%nSequential test: %s", bq.getClass());    
    assertTrue(bq.isEmpty());
    assertTrue(!bq.isFull());
    bq.put(7); bq.put(9); bq.put(13); 
    assertTrue(!bq.isEmpty());
    assertTrue(bq.isFull());
    assertEquals(bq.take(), 7);
    assertEquals(bq.take(), 9);
    assertEquals(bq.take(), 13);
    assertTrue(bq.isEmpty());
    assertTrue(!bq.isFull());
    System.out.println("... passed");
  }

  private static void parallelTest(BoundedQueue<Integer> bq) throws Exception {
    System.out.printf("%nParallel test: %s", bq.getClass()); 
    final ExecutorService pool = Executors.newCachedThreadPool();
    new PutTakeTest(bq, 17, 100000).test(pool); 
    pool.shutdown();
    System.out.println("... passed");      
  }
}

class Tests {
  public static void assertEquals(int x, int y) throws Exception {
    if (x != y) 
      throw new Exception(String.format("ERROR: %d not equal to %d%n", x, y));
  }

  public static void assertTrue(boolean b) throws Exception {
    if (!b) 
      throw new Exception(String.format("ERROR: assertTrue"));
  }
}

interface BoundedQueue<T> {
  boolean isEmpty();
  boolean isFull();
  void put(T item) throws Exception;
  T take() throws Exception;
}

class SemaphoreBoundedQueue <T> implements BoundedQueue<T> {
  private final Semaphore availableItems, availableSpaces;
  private final T[] items;
  private int tail = 0, head = 0;
  
  public SemaphoreBoundedQueue(int capacity) {
    this.availableItems = new Semaphore(0);
    this.availableSpaces = new Semaphore(capacity);
    this.items = makeArray(capacity);
  }
  
  @SuppressWarnings("unchecked")   
  private static <T> T[] makeArray(int capacity) {
    // Java's @$#@?!! type system requires this unsafe cast    
    return (T[])new Object[capacity];
  }

  public boolean isEmpty() {
    return availableItems.availablePermits() == 0;
  }

  public boolean isFull() {
    return availableSpaces.availablePermits() == 0;
  }

  public void put(T item) throws InterruptedException { // at tail
    availableSpaces.acquire();
    doInsert(item);
    availableItems.release();
  }

  public T take() throws InterruptedException {         // from head
    availableItems.acquire();
    T item = doExtract();
    availableSpaces.release();
    return item;
  }
  
  private synchronized void doInsert(T item) {
    items[tail] = item;
    tail = (tail + 1) % items.length;
  }
  
  private synchronized T doExtract() {
    T item = items[head];
    items[head] = null;
    head = (head + 1) % items.length;
    return item;
  }
}

// Lock-based bounded queue with blocking operations

class LockingBoundedQueue <T> implements BoundedQueue<T> {
  private int availableItems, availableSpaces;
  private final T[] items;
  private int tail = 0, head = 0;
  
  public LockingBoundedQueue(int capacity) {
    this.availableItems = 0;
    this.availableSpaces = capacity;
    this.items = makeArray(capacity);
  }
  
  @SuppressWarnings("unchecked")   
  private static <T> T[] makeArray(int capacity) {
    // Java's @$#@?!! type system requires this unsafe cast    
    return (T[])new Object[capacity];
  }

  public synchronized boolean isEmpty() {
    return availableItems == 0;
  }

  public synchronized boolean isFull() {
    return availableSpaces == 0;
  }

  public synchronized void put(T item) throws InterruptedException {    // at tail
    while (isFull())
      wait();
    availableSpaces--;
    doInsert(item);
    availableItems++;
    notifyAll();
  }

  public synchronized T take() throws InterruptedException {            // from head
    while (isEmpty())
      wait();
    availableItems--;
    T item = doExtract();
    availableSpaces++;
    notifyAll();
    return item;
  }
  
  private synchronized void doInsert(T item) {
    items[tail] = item;
    tail = (tail + 1) % items.length;
  }
  
  private synchronized T doExtract() {
    T item = items[head];
    items[head] = null;
    head = (head + 1) % items.length;
    return item;
  }
}


/**
 * Producer-consumer test program for BoundedQueue
 *
 * @author Brian Goetz and Tim Peierls; some modifications by
 * sestoft@itu.dk
 */

class PutTakeTest extends Tests {
  // We could use one CyclicBarrier for both starting and stopping,
  // precisely because it is cyclic, but the code becomes clearer by
  // separating them:
  protected CyclicBarrier startBarrier, stopBarrier;
  protected final BoundedQueue<Integer> bq;
  protected final int nTrials, nPairs;
  protected final AtomicInteger putSum = new AtomicInteger(0);
  protected final AtomicInteger takeSum = new AtomicInteger(0);

  public PutTakeTest(BoundedQueue<Integer> bq, int npairs, int ntrials) {
    this.bq = bq;
    this.nTrials = ntrials;
    this.nPairs = npairs;
    this.startBarrier = new CyclicBarrier(npairs * 2 + 1);
    this.stopBarrier = new CyclicBarrier(npairs * 2 + 1);
  }
  
  void test(ExecutorService pool) {
    try {
      for (int i = 0; i < nPairs; i++) {
        pool.execute(new Producer());
        pool.execute(new Consumer());
      }      
      startBarrier.await(); // wait for all threads to be ready
      stopBarrier.await();  // wait for all threads to finish      
      assertTrue(bq.isEmpty());
      assertEquals(putSum.get(), takeSum.get());
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }

  class Producer implements Runnable {
    public void run() {
      try {
        Random random = new Random();
        int sum = 0;
        startBarrier.await();
        for (int i = nTrials; i > 0; --i) {
          int item = random.nextInt();
          bq.put(item);
          sum += item;
        }
        putSum.getAndAdd(sum);
        stopBarrier.await();
      } catch (Exception e) {
        throw new RuntimeException(e);
      }
    }
  }
  
  class Consumer implements Runnable {
    public void run() {
      try {
        startBarrier.await();
        int sum = 0;
        for (int i = nTrials; i > 0; --i) {
          sum += bq.take();
        }
        takeSum.getAndAdd(sum);
        stopBarrier.await();
      } catch (Exception e) {
        throw new RuntimeException(e);
      }
    }
  }
}
