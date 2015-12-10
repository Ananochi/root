package com.company;

import java.util.concurrent.atomic.AtomicInteger;

public class MyAtomicIntegerWithAtomicInteger {
    private AtomicInteger atomicInteger;

    public MyAtomicIntegerWithAtomicInteger() {
        atomicInteger = new AtomicInteger();
    }

    public int addAndGet(int amount){
        return atomicInteger.addAndGet(amount);
    }

    public int get(){
        return atomicInteger.get();
    }

}
