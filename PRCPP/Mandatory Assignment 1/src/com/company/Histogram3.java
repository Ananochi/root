package com.company;

import java.util.concurrent.atomic.AtomicInteger;

public class Histogram3 implements Histogram {
    private final int span;
    private final AtomicInteger[] counts;

    public Histogram3(int span) {
        this.span = span;
        this.counts = new AtomicInteger[span];
        for (int i = 0; i < span; i++) {
            counts[i] = new AtomicInteger();
        }

    }

    @Override
    public void increment(int bin) {
        counts[bin].incrementAndGet();
    }

    @Override
    public int getCount(int bin) {
        return counts[bin].get();
    }

    @Override
    public int getSpan() {
        return span;
    }

    @Override
    public int[] getBins() {
        AtomicInteger[] countsSource = this.counts.clone();
        int[] output = new int[countsSource.length];
        for (int i = 0; i < countsSource.length; i++) {
            output[i] = countsSource[i].get();
        }
        return output;
    }
}
