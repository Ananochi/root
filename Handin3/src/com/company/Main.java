package com.company;

import java.io.File;
import java.io.IOException;
import java.util.List;

public class Main {

    public static void main(String[] args) throws IOException {

        // get the input the directory
        File[] files = new File(args[0]).listFiles();

        for (File f : files) {

            // readfiles from directory - args[0]
            String[] inputFile = FileReader.ReadFile(f.getPath()); //String[] inputFile = FileReader.ReadFile(".\\data-cp\\bier127-tsp.txt");

            // create lists Px & Py
            List<List<Point>> points = Parser.ToPoints(inputFile);
            // compute closest pair of points
            Double output = ClosestPair.ClosestPairRec(points.get(0), points.get(1));

            //print output to console
            System.out.println(f.getPath() + ": " + points.get(0).size() + " " + output);
        }

    }
}
