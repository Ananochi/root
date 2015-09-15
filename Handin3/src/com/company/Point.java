package com.company;

import java.util.Comparator;

/**
 * Created by B006572 on 13-09-2015.
 */
public class Point{

    private Coordinate x;
    private Coordinate y;

    public Point(Coordinate x, Coordinate y) {

        super();
        this.x = x;
        this.y = y;

    }

    public void setX(Coordinate x) {
        this.x = x;
    }

    public void setY(Coordinate y) {
        this.y = y;
    }

    public Coordinate getX() {

        return x;
    }

    public Coordinate getY() {

        return y;
    }

    static Comparator<Point> xRank = new Comparator<Point>() {
        @Override
        public int compare(Point c1, Point c2) {
            if (c1.getX().getVal() < c2.getX().getVal()) return -1;
            if (c1.getX().getVal() > c2.getX().getVal()) return 1;
            return 0;
        }
    };

    static Comparator<Point> yRank = new Comparator<Point>() {
        @Override
        public int compare(Point c1, Point c2) {
            if (c1.getY().getVal() < c2.getY().getVal()) return -1;
            if (c1.getY().getVal() > c2.getY().getVal()) return 1;
            return 0;
        }
    };
}
