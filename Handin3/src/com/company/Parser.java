package com.company;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Created by B006572 on 11-09-2015.
 */
public class Parser {

    public static List<List<Point>> ToPoints(String[] file) {

        List<List<Point>> results = new ArrayList<>();
        List<Point> xSorted = new ArrayList<>();
        List<Point> ySorted = new ArrayList<>();

        Double x;
        Double y;
        int id;

        for (int i=0; i<file.length; i++) {

            String str = file[i];

            if (Character.isDigit(str.charAt(0))) {

                //str = str.replaceAll("^[+-]?\\d*\\.?\\d*$", " ");
                String[] vals = str.split("\\s+");

                // getting the x,y pairs as doubles and the id for later use
                x = Double.parseDouble(vals[1]);
                y = Double.parseDouble(vals[2]);
                id = Integer.parseInt(vals[0]);

                xSorted.add(new Point(new Coordinate(x, id), new Coordinate(y, id)));
                ySorted.add(new Point(new Coordinate(x, id), new Coordinate(y, id)));
            }
        }

        Collections.sort(xSorted, Point.xRank);
        Collections.sort(ySorted, Point.yRank);

        results.add(0, xSorted);
        results.add(1, ySorted);

        // add sorted list to output list
        return results;
    }
}