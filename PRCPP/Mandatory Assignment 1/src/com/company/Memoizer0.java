package com.company;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class Memoizer0 <A, V> {
    private final ConcurrentMap<A,V> cache = new ConcurrentHashMap<A,V>();
    private final Computable<A,V> c;

    public Memoizer(Computable<A,V> c) {
        this.c = c;
    }
    public V compute(final A arg) throws InterruptedException {
            return cache.computeIfAbsent(arg, arg -> new V(f(k)));
        }
    }
}