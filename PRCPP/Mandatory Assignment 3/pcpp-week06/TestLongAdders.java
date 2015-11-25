// For week 8
// sestoft@itu.dk * 2014-10-22, 2015-09-25

// It is possible to obtain a modest speedup (2x or so) by using 16 or
// 32 AtomicLongs instead of one, and index by thread hashcode.  But
// the built-in LongAdder is 6-30 x faster still.  The overhead for
// getting the thread's hashcode is 3 ns and that may overshadow any
// real benefits, bu tmore likely there are other problems, such as
// false sharing in the cache, which is carefully avoided in the real
// LongAdder implementation.

// The false sharing can be removed to some extent on the 4 core (x 2
// hyperthreading) Intel i7 using the bizarre allocation idea shown
// in NewLongAdderPadded, but the effect of this on the 32 core AMD
// Opteron is modest.  In any case, technology-dependent allocation
// code such as that in NewLongAdderPadded should be hidden in standard
// libraries where it can be updated by experts as technology and JVM
// implementation techniques evolve. 

import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicLongArray;
import java.util.concurrent.atomic.LongAdder;
import java.util.concurrent.ThreadLocalRandom;
import java.util.function.IntToDoubleFunction;

public class TestLongAdders {
  private static final int threadCount = 32, iterations = 1_000_000;

  public static void main(String[] args) {
    SystemInfo();
    Mark7("current thread hashCode", 
          i -> Thread.currentThread().hashCode());
    Mark7("ThreadLocalRandom", 
          i -> ThreadLocalRandom.current().nextInt());
    Mark7("AtomicLong", 
          i -> exerciseAtomicLong());
    Mark7("LongAdder", 
          i -> exerciseLongAdder());
    Mark7("LongCounter", 
          i -> exerciseLongCounter());
    Mark7("NewLongAdder", 
          i -> exerciseNewLongAdder());
    Mark7("NewLongAdderPadded", 
          i -> exerciseNewLongAdderPadded());
  }

  // Timing of Java's AtomicLong
  private static double exerciseAtomicLong() {
    final AtomicLong adder = new AtomicLong();
    Thread[] threads = new Thread[threadCount];
    for (int t=0; t<threadCount; t++) {
      final int myThread = t;
      threads[t] = new Thread(() -> {
        for (int i=0; i<iterations; i++) 
          adder.getAndAdd(i);
      });
    }
    for (int t=0; t<threadCount; t++) 
      threads[t].start();
    try {
      for (int t=0; t<threadCount; t++) 
        threads[t].join();
    } catch (InterruptedException exn) { }
    return adder.get();
  }

  // Timing of Java 8's built-in LongAdder, supposedly highly scalable
  private static double exerciseLongAdder() {
    final LongAdder adder = new LongAdder();
    Thread[] threads = new Thread[threadCount];
    for (int t=0; t<threadCount; t++) {
      final int myThread = t;
      threads[t] = new Thread(() -> {
        for (int i=0; i<iterations; i++) 
          adder.add(i);
      });
    }
    for (int t=0; t<threadCount; t++) 
      threads[t].start();
    try {
      for (int t=0; t<threadCount; t++) 
        threads[t].join();
    } catch (InterruptedException exn) { }
    return adder.longValue();
  }

  // Timing of a simple long with synchronized add and get methods
  private static double exerciseLongCounter() {
    final LongCounter adder = new LongCounter();
    Thread[] threads = new Thread[threadCount];
    for (int t=0; t<threadCount; t++) {
      final int myThread = t;
      threads[t] = new Thread(() -> {
        for (int i=0; i<iterations; i++) 
          adder.add(i);
      });
    }
    for (int t=0; t<threadCount; t++) 
      threads[t].start();
    try {
      for (int t=0; t<threadCount; t++) 
        threads[t].join();
    } catch (InterruptedException exn) { }
    return adder.get();
  }

  // Timing of a striped long, with dense allocation of stripes
  private static double exerciseNewLongAdder() {
    final NewLongAdder adder = new NewLongAdder();
    Thread[] threads = new Thread[threadCount];
    for (int t=0; t<threadCount; t++) {
      final int myThread = t;
      threads[t] = new Thread(() -> {
        for (int i=0; i<iterations; i++) 
          adder.add(i);
      });
    }
    for (int t=0; t<threadCount; t++) 
      threads[t].start();
    try {
      for (int t=0; t<threadCount; t++) 
        threads[t].join();
    } catch (InterruptedException exn) { }
    return adder.longValue();
  }

  // Timing of a striped long, with scattered allocation of stripes
  private static double exerciseNewLongAdderPadded() {
    final NewLongAdderPadded adder = new NewLongAdderPadded();
    Thread[] threads = new Thread[threadCount];
    for (int t=0; t<threadCount; t++) {
      final int myThread = t;
      threads[t] = new Thread(() -> {
        for (int i=0; i<iterations; i++) 
          adder.add(i);
      });
    }
    for (int t=0; t<threadCount; t++) 
      threads[t].start();
    try {
      for (int t=0; t<threadCount; t++) 
        threads[t].join();
    } catch (InterruptedException exn) { }
    return adder.longValue();
  }

  // --- Benchmarking infrastructure ---

  private static class Timer {
    private long start, spent = 0;
    public Timer() { play(); }
    public double check() { return (System.nanoTime()-start+spent)/1e9; }
    public void pause() { spent += System.nanoTime()-start; }
    public void play() { start = System.nanoTime(); }
  }

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
        double time = runningTime * 1e6 / count; // microseconds
        st += time; 
        sst += time * time;
        totalCount += count;
      }
    } while (runningTime < 0.25 && count < Integer.MAX_VALUE/2);
    double mean = st/n, sdev = Math.sqrt((sst - mean*mean*n)/(n-1));
    System.out.printf("%-25s %15.1f us %10.2f %10d%n", msg, mean, sdev, count);
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
}

// ----------------------------------------------------------------------

// An atomic long that consists of a single private long field and
// synchronized methods, following Java monitor pattern.

class LongCounter {
  private long count = 0;
  public synchronized void add(int delta) {
    count += delta;
  }
  public synchronized long get() { 
    return count; 
  }
}

// ----------------------------------------------------------------------

// An atomic long that is composed of NSTRIPES AtomicLongs stored next
// to each other in an array.  Probably not a good idea except deep in
// the Java class libraries.  In any case, presumably a thread
// hashcode could be negative, so should use 
// Math.abs(Thread.currentThread().hashCode() % NSTRIPES) or 
// (Thread.currentThread().hashCode() & 0x7FFFFFFF) % NSTRIPES.

class NewLongAdder {
  private final static int NSTRIPES = 31;
  private final AtomicLongArray counters = new AtomicLongArray(NSTRIPES);

  public void add(long delta) {
    counters.addAndGet(Thread.currentThread().hashCode() % NSTRIPES, delta);
  }

  public long longValue() {
    long result = 0;
    for (int stripe=0; stripe<NSTRIPES; stripe++)
      result += counters.get(stripe);
    return result;
  }
}

// ----------------------------------------------------------------------

// An atomic long that is composed of NSTRIPES AtomicLongs
// (presumably) scattered in the heap because of the seemingly useless
// Object allocations.  Inspired by the innards of Java 8's LongAdder.

class NewLongAdderPadded {
  private final static int NSTRIPES = 31;
  private final AtomicLong[] counters;

  public NewLongAdderPadded() {
    this.counters = new AtomicLong[NSTRIPES];
    for (int stripe=0; stripe<NSTRIPES; stripe++) {
      // Believe it or not, this sometimes speeds up the code,
      // presumably because it avoids false sharing of cache lines:
      new Object(); new Object(); new Object(); new Object();
      counters[stripe] = new AtomicLong();
    }
  }

  public void add(long delta) {
    counters[Thread.currentThread().hashCode() % NSTRIPES].addAndGet(delta);
  }

  public long longValue() {
    long result = 0;
    for (int stripe=0; stripe<NSTRIPES; stripe++)
      result += counters[stripe].get();
    return result;
  }
}

