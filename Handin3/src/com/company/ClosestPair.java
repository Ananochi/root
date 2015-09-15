package com.company;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Created by B006572 on 11-09-2015.
 */
public class ClosestPair {

    // Main algorithm method
    public static Double ClosestPairRec(List<Point> px, List<Point> py) {

        // if |P| <= 3
        // Chosing whether to look at either px or py,
        // shouldent make a difference, since points are the same.
        if (px.size() <= 3) {
            return brutePairs(px);
        }

        // TODO - Runtime of Java 'SubList' - look it up!
        // TODO - The construction of Qx, Qy, Rx and Ry should run in O(n) time!

        // Contruct Qx, Qy, Rx, Ry
        List<List<Point>> QR = constructQR(px, py);
        List<Point> qx = QR.get(0);
        List<Point> qy = QR.get(2);
        List<Point> rx = QR.get(1);
        List<Point> ry = QR.get(3);

        Double q0q1 = ClosestPairRec(qx, qy);
        Double r0r1 = ClosestPairRec(rx, ry);

        Double delta = Math.min(q0q1, r0r1);
        Coordinate xstar = px.get(px.size()-1).getX();
        List<Point> s = new ArrayList<>();

        // TODO - does the construction of Py run in O(n) time?
        // TODO - it uses the sort at the end!??

        // getting values in P within distance delta of L (the vertical line contructed by 'xstar')
        for (Point p : px) {

            if (p.getX().getVal() <= xstar.getVal() && p.getX().getVal() <= xstar.getVal()+delta) {

                // the point lies within
                s.add(p);
            }
        }

        // creating Sy
        Collections.sort(s, Point.yRank);

        // for each point s in Sy, compute distance from s to each of next 15 points in Sy
        int index = 0;
        Double lowest = null;

        // TODO - this is O(n) time? Doubt it :(

        for (Point p : s) {
            for (int i=index+1; i<index+15; i++) {
                if (i==s.size()) {break;}
                Point test = s.get(i);
                Double val = eucDist(p,test);

                if (lowest == null || val <= lowest) {
                    lowest=val;
                }
            }
            index++;
        }

        if (lowest<delta) {return lowest;}
        return delta;

    }

    // Constructs Q and R from Px and Py
    // Returns List<Qx, Rx, Qy, Ry>
    public static List<List<Point>> constructQR(List<Point> px, List<Point> py) {

        List<List<Point>> output = new ArrayList<>();

        int xSize = (int) Math.ceil(px.size() / 2.00);
        int ySize = (int) Math.ceil(py.size() / 2.00);

        output.add(px.subList(0, xSize)); // QX
        output.add(px.subList(xSize,px.size())); // RX
        output.add(py.subList(0, ySize)); // QY
        output.add(py.subList(ySize, py.size())); // RY

        return output;
    }

    // calculates the distance between 3 points - the brute way!
    // return this as Double
    public static Double brutePairs(List<Point> points) {

        List<Point> brutePairs = points;

        // calc: p0->p1, p0->p2, p1->p2
        if (brutePairs.size() == 2) {
            Double p0p1 = eucDist(brutePairs.get(0), brutePairs.get(1));
            return p0p1;
        }

        if (brutePairs.size() == 3) {
            Double p0p1 = eucDist(brutePairs.get(0), brutePairs.get(1));
            Double p0p2 = eucDist(brutePairs.get(0), brutePairs.get(2));
            Double p1p2 = eucDist(brutePairs.get(1), brutePairs.get(2));
            Double result = Math.min(Math.min(p0p1, p0p2), p1p2);
            return result;
        }
        // theres only 1 point - no distance!
        return 0.00;
    }

    // Standard Euclidian distance between two points
    // Returns a double
    public static Double eucDist(Point p0, Point p1) {

        Double x1 = p0.getX().getVal();
        Double x2 = p1.getX().getVal();
        Double y1 = p0.getY().getVal();
        Double y2 = p1.getY().getVal();

        Double euc = ((x2 - x1) * (x2 - x1)) + ((y2 - y1) * (y2 - y1));

        return Math.sqrt(euc);
    }
}
