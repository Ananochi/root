package com.company;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by B006572 on 21-09-2015.
 */
public class Parser {

    public static List<Protein> Parser(String[] input) {

        String temp = "";
        int elemCount = 0;
        List<Protein> output = new ArrayList<>();

        // creating the Proteins
        for (int i=0; i<input.length; i++) {

            // we found a new Protein
            if (input[i].startsWith(">")) {

                // setting the protein String
                if (elemCount != 0) {
                    output.get(elemCount-1).setProtein(temp);
                    temp = "";
                }

                String[] vals = input[i].split("\\s+");
                Protein element = new Protein(vals[0].substring(1), "");
                output.add(element);
                elemCount++;
            }

            else {
                temp+=input[i];
            }
        }
        // setting the last element aswell ...
        output.get(elemCount-1).setProtein(temp);
        return output;
    }
}
