package com.company;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

/**
 * Created by B006572 on 21-09-2015.
 */
public class CostParser {

    private Scanner sc;
    private Map<String, Integer> costsMap = new HashMap();

    public CostParser(String costsFile) {
        try {
            sc = new Scanner(new File(costsFile));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public Map<String, Integer> getCosts() {
        sc.useDelimiter("\\r");
        while (sc.hasNext("[#].*"))
            sc.next();
        sc.nextLine();
        String proteinsList = sc.nextLine().replaceAll(" ", "");

        int row, col;
        for (row = 0; row < proteinsList.length(); row++)
        {
            String[] costsLine = sc.nextLine().trim().replaceAll("  ", " ").substring(2).split(" ");
            for (col = 0; col < proteinsList.length(); col++)
            {
                costsMap.put(proteinsList.substring(row, row + 1) + proteinsList.substring(col, col + 1),
                        Integer.parseInt(costsLine[col]));
            }
        }
        return costsMap;
    }
}
