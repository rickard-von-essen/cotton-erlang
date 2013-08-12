package net.sf.cotton;

import net.sf.cotton.records.Pair;

public interface TestService {

    int add(int a, int b);

    String concatenate(Pair p);

    Pair split(String s);
}
