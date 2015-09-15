package com.company;

import com.sun.org.apache.xpath.internal.SourceTree;

import java.io.FileReader;
import java.io.IOException;
import java.util.List;

public class Main {

    public static void main(String[] args) throws IOException {

        // readfile
        String[] inputFile = com.company.FileReader.ReadFile(args[0]);
        // create lists Px & Py
        List<List<Point>> points = Parser.ToPoints(inputFile);
        // compute closest pair of points
        Double output = ClosestPair.ClosestPairRec(points.get(0), points.get(1));

        //print output to console
        System.out.println(args[0] + ": " + points.get(0).size() + " " + output);

    }
}
