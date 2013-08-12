package net.sf.cotton;

import junit.framework.TestCase;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.context.ApplicationContext;

/**
 * Unit test for simple App.
 */
public class HessianTest extends TestCase {

    HessianServer server;

    @Override
    protected void setUp() throws Exception {
        server = new HessianServer();
        server.start();
    }

    @Override
    protected void tearDown() throws Exception {
        server.stop();
    }

    /**
     * Rigourous Test :-)
     */
    public void testHessian() {
        ApplicationContext ctx = new ClassPathXmlApplicationContext("spring.xml");
        TestService testService = (TestService) ctx.getBean("testService");
        int result = testService.add(2,3);
        assertEquals(5, result);
    }
}
