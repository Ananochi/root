package com.company;

import java.util.concurrent.atomic.AtomicIntegerArray;

public class Histogram4 implements Histogram {
    private final int span;
    private final AtomicIntegerArray counts;

    public Histogram4(int span) {
        this.span = span;
        this.counts = new AtomicIntegerArray(span);
       // for (int i = 0; i < span; i++) {
         //   counts[i] = new AtomicInteger();
        //}

    }

    @Override
    public void increment(int bin) {
        counts.incrementAndGet(bin);
    }

    @Override
    public int getCount(int bin) {
        return counts.get(bin);
    }

    @Override
    public int getSpan() {
        return span;
    }

    @Override
    public int[] getBins() {
        int[] output = new int[span];
        for (int i = 0; i < span; i++) {
            output[i] = counts.get(i);
        }
        return output;
    }
}
