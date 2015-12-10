package com.company;

public class MyAtomicInteger {
    private int AtomicInteger = 0;

    public synchronized int addAndGet(int amount){
        AtomicInteger = AtomicInteger + amount;
        return AtomicInteger;
    }

    public synchronized int get(){
        return AtomicInteger;
    }
}
 
