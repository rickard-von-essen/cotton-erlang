package net.sf.cotton;

import net.sf.cotton.records.Pair;

public class TestServiceImpl implements TestService {

    public int add(int a, int b) {
        return a + b;
    }

    public String concatenate(Pair p) {
        return p.getFirst() + p.getSecond();
    }

    public Pair split(String s) {
        Pair pair = new Pair();
        int length = s.length();
        pair.setFirst(s.substring(0,length/2));
        pair.setSecond(s.substring(length/2, length));
        return pair;
    }

}
