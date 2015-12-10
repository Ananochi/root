// For week 10
// sestoft@itu.dk * 2014-11-05, 2015-10-14

// Compile and run like this:
//   javac -cp ~/lib/multiverse-core-0.7.0.jar TestStmHistogram.java
//   java -cp ~/lib/multiverse-core-0.7.0.jar:. TestStmHistogram

// For the Multiverse library:

import org.multiverse.api.references.TxnInteger;

import java.util.concurrent.CyclicBarrier;

import static org.multiverse.api.StmUtils.atomic;
import static org.multiverse.api.StmUtils.newTxnInteger;

// Multiverse locking:

class TestStmHistogram {
    public static void main(String[] args) throws InterruptedException {
        countPrimeFactorsWithStmHistogram();
    }

    private static void countPrimeFactorsWithStmHistogram() throws InterruptedException {
        final Histogram histogram = new StmHistogram(30);
        final Histogram total = new StmHistogram(30);
        final int range = 4_000_000;
        final int threadCount = 10, perThread = range / threadCount;
        final CyclicBarrier startBarrier = new CyclicBarrier(threadCount + 1),
                stopBarrier = startBarrier;
        final Thread[] threads = new Thread[threadCount];
        for (int t = 0; t < threadCount; t++) {
            final int from = perThread * t,
                    to = (t + 1 == threadCount) ? range : perThread * (t + 1);
            threads[t] =
                    new Thread(() -> {
                        try {
                            startBarrier.await();
                        } catch (Exception exn) {
                        }
                        for (int p = from; p < to; p++)
                            histogram.increment(countFactors(p));
                        System.out.print("*");
                        try {
                            stopBarrier.await();
                        } catch (Exception exn) {
                        }
                    });
            threads[t].start();

        }

        try {
            startBarrier.await();
        } catch (Exception exn) {
        }
        try {
            stopBarrier.await();
        } catch (Exception exn) {
        }

        for (int k = 0; k<200; k++) {
            total.transferBins(histogram);
            Thread.sleep(30);
        }

        dump(histogram);
        dump(total);

        total.transferBins(total);

        dump(total);
        //int[] testGetBins = histogram.getBins();
        //int testGetAndClear = histogram.getAndClear(testGetBins.length -1);
        //System.out.println("Copying histogram to 'this' and emptying histogram");
        //histogramTEST.transferBins(histogram);
        //System.out.println("printing histogram");
        //dump(histogram);
        //System.out.println("printing this");
        //dump(histogramTEST);
    }

    public static void dump(Histogram histogram) {
        int totalCount = 0;
        for (int bin = 0; bin < histogram.getSpan(); bin++) {
            System.out.printf("%4d: %9d%n", bin, histogram.getCount(bin));
            totalCount += histogram.getCount(bin);
        }
        System.out.printf("      %9d%n", totalCount);
    }

    public static int countFactors(int p) {
        if (p < 2)
            return 0;
        int factorCount = 1, k = 2;
        while (p >= k * k) {
            if (p % k == 0) {
                factorCount++;
                p /= k;
            } else
                k++;
        }
        return factorCount;
    }
}

interface Histogram {
    void increment(int bin);

    int getCount(int bin);

    int getSpan();

    int[] getBins();

    int getAndClear(int bin);

    void transferBins(Histogram hist);
}

class StmHistogram implements Histogram {
    private final TxnInteger[] counts;

    public StmHistogram(int span) {
        counts = new TxnInteger[span];
        for (int i = 0;i<span;i++)
            counts[i] = newTxnInteger();
    }

    public void increment(int bin) {
        atomic(() -> {
            counts[bin].increment();
        });
    }

    public int getCount(int bin) {
        return atomic(() -> {
            return counts[bin].get();
        });
    }

    public int getSpan() {
        return atomic(() -> counts.length);
    }

    public int[] getBins() {
        return atomic(() -> {
            int[] bins = new int[counts.length];
            for (int i = 0; i < counts.length; i++) {
                bins[i] = counts[i].get();
            }
            return bins;
        });
    }

    public int getAndClear(int bin) {
        return atomic(() -> {
            return counts[bin].getAndSet(0);
        });
    }

    public void transferBins(Histogram hist) {
        for (int i = 0; i < hist.getSpan(); i++) {
            final int finalI = i;
            atomic(() -> {
                int counter = hist.getAndClear(finalI);
                while (counter != 0) {
                    this.increment(finalI);
                    counter--;
                }
            });
        }
    }
}
