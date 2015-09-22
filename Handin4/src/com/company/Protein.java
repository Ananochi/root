package com.company;

/**
 * Created by B006572 on 21-09-2015.
 */
public class Protein {

    private String name;
    private String protein;

    public Protein(String name, String protein) {

        super();
        this.name = name;
        this.protein = protein;

    }

    public void setName(String name) {
        this.name = name;
    }

    public void setProtein(String protein) {
        this.protein = protein;
    }

    public String getName() {

        return name;
    }

    public String getProtein() {
        return protein;
    }
}
