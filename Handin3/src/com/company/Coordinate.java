package com.company;

import java.util.Comparator;

/**
 * Created by B006572 on 14-09-2015.
 */
public class Coordinate implements Comparator<Coordinate> {

    private Double val;
    private int id;
    Coordinate(){}

    Coordinate(Double v, int i) {

        super();
        this.val = v;
        this.id = i;

    }

    public void setVal(Double val) {
        this.val = val;
    }

    public Double getVal() {

        return val;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    @Override
    public int compare(Coordinate c1, Coordinate c2) {
        if (c1.getVal() < c2.getVal()) return -1;
        if (c1.getVal() > c2.getVal()) return 1;
        return 0;
    }
}
