package com.company;

import java.util.concurrent.atomic.LongAdder;

public class Histogram5 implements Histogram {
    private final int span;
    private final LongAdder[] counts;

    public Histogram5(int span) {
        this.span = span;
        this.counts = new LongAdder[span];
        for (int i = 0; i < span; i++) {
            counts[i] = new LongAdder();
        }

    }

    @Override
    public synchronized void increment(int bin) {
        counts[bin].increment();
    }

    @Override
    public int getCount(int bin) {
        return counts[bin].intValue();
    }

    @Override
    public int getSpan() {
        return span;
    }

    @Override
    public int[] getBins() {
        LongAdder[] countsSource = this.counts.clone();
        int[] output = new int[countsSource.length];
        for (int i = 0; i < countsSource.length; i++) {
            output[i] = countsSource[i].intValue();
        }
        return output;
    }
}
