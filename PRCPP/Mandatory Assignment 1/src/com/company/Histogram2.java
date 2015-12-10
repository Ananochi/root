package com.company;

public class Histogram2 implements Histogram {
    private final int span;
    private int[] counts;

    public Histogram2(int span) {
        this.span = span;
        this.counts = new int[span];
    }

    @Override
    public synchronized void increment(int bin) {
        counts[bin] = counts[bin] + 1;
    }

    @Override
    public synchronized int getCount(int bin) {
        return counts[bin];
    }

    @Override
    public int getSpan() {
        return counts.length;
    }
    
    @Override
    public int[] getBins() {
        int[] countsSource = this.counts.clone();
        int[] output = new int[countsSource.length];
        for (int i = 0; i < countsSource.length; i++) {
            output[i] = countsSource[i].get();
        }
        return output;
    }
} 
