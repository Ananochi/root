package com.company;

import java.util.ArrayList;
import java.util.List;

class Main2 {

    public static void main(String[] args) {
        MyAtomicIntegerWithAtomicInteger primeFactors = new MyAtomicIntegerWithAtomicInteger();
        int startOfRange = 0;
        int endOfRange = 499999;
        List<Thread> threadsToRun = new ArrayList<>();

        for (int i = 0; i < 10; i++) {
            final int finalStartOfRange = startOfRange;
            final int finalEndOfRange = endOfRange;
            threadsToRun.add(new Thread(() -> {
                primeFactors.addAndGet(countFactorInRange(finalStartOfRange, finalEndOfRange));
            }));
            startOfRange += 500000;
            endOfRange += 500000;
        }

        long startTime = System.nanoTime();

        long endTime = System.nanoTime();
        threadsToRun.forEach(java.lang.Thread::run);
        long duration = (endTime - startTime);

        System.out.println(primeFactors.get());
        System.out.println(duration);
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

    public static int countFactorInRange(int start, int end) {
        int primeFactors = 0;
        for (int i = start; i <= end; i++) {
            primeFactors += countFactors(i);
        }
        return primeFactors;
    }
}
 
