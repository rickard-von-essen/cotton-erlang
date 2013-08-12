package net.sf.cotton.records;

import java.io.Serializable;

public class Pair implements Serializable {
    private String first;
    private String second;


    public String getFirst() {
        return first;
    }

    public void setFirst(String first) {
        this.first = first;
    }

    public String getSecond() {
        return second;
    }

    public void setSecond(String second) {
        this.second = second;
    }
}
