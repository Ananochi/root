package com.company;

import java.io.IOException;
import java.util.List;
import java.util.Map;

public class Main {

    public static void main(String[] args) throws IOException {

        String[] input = FileReader.ReadFile(args[0]);
        Map<String, Integer> costs = new CostParser("C:\\Users\\b006572\\Documents\\GitHub\\root\\Handin4\\gorilla_data\\BLOSUM62.txt").getCosts();
        List<Protein> proteins = Parser.Parser(input);

        for (int i=0; i<proteins.size()-1; i++){
            for (int j=i+1; j<proteins.size(); j++) {

                Integer result = Algorithm.Algorithm(proteins.get(i).getProtein(), proteins.get(j).getProtein(), costs);
                System.out.println(proteins.get(i).getName() + "--" + proteins.get(j).getName() + ": " + result);
            }
        }
    }
}
