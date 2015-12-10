package com.company;

import java.util.ArrayList;
import java.util.List;

public class Main {

    public static void main(String[] args) {
        Histogram2 histogram2 = new Histogram2(4_999_999);
        int startOfRange = 0;
        int endOfRange = 499999;
        List<Thread> threadsToRun = new ArrayList<>();

        for (int i = 0; i < 10; i++) {
            final int finalStartOfRange = startOfRange;
            final int finalEndOfRange = endOfRange;
            threadsToRun.add(new Thread(() -> {
                for (int p = finalStartOfRange; p <= finalEndOfRange; p++) {
                    histogram2.increment(countFactors(p));
                }
            }));
            startOfRange += 500000;
            endOfRange += 500000;
        }

        long startTime = System.nanoTime();

        long endTime = System.nanoTime();
        threadsToRun.forEach(java.lang.Thread::run);
        long duration = (endTime - startTime);

        for (int i = 0; i < histogram2.getSpan(); i++) {
            System.out.println(String.format("%s:\t%s",i,histogram2.getCount(i)));
        }

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
}
