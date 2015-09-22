package com.company;

import java.util.ArrayList;
import java.util.Map;

/**
 * Created by B006572 on 21-09-2015.
 */
public class Algorithm {

    public static Integer Algorithm(String seq1, String seq2, Map<String, Integer> costMap) {

        String sequenceA = seq1;
        String sequenceB = seq2;

        int gap = costMap.get("A*");

        // Array A[0...m, 0...n]
        int[][] opt = new int[sequenceA.length() +1][sequenceB.length() +1];

        // Initialize A[i,0]=idelta for each i
        for (int i = 1; i <= sequenceA.length(); i++) {
            opt[i][0] = opt[i - 1][0] + gap;
        }

        // Initialize A[j,0]=idelta for each j
        for (int j = 1; j <= sequenceB.length(); j++) {
            opt[0][j] = opt[0][j - 1] + gap;
        }

        // main for loop
        for (int i = 1; i <= sequenceA.length(); i++) {
            for (int j = 1; j <= sequenceB.length(); j++) {

                String str1 = Character.toString(sequenceA.charAt(i - 1));
                String str2 = Character.toString(sequenceB.charAt(j - 1));
                Integer cost = costMap.get(str1 + str2);

                int scoreDiag = opt[i - 1][j - 1] + cost;
                int scoreLeft = opt[i][j - 1] + gap;
                int scoreUp = opt[i - 1][j] + gap;

                // we take the minimum
                opt[i][j] = Math.max(Math.max(scoreDiag, scoreLeft), scoreUp);
            }
        }

/*        for (int i = 1; i <= sequenceA.length(); i++) {
            for (int j = 1; j <= sequenceB.length(); j++)
                System.out.print(opt[i][j] + "\t");
            System.out.println();
        }*/

        return opt[sequenceA.length()][sequenceB.length()];
    }
}
