package net.sf.cotton;

import com.caucho.hessian.server.HessianServlet;
import org.mortbay.jetty.Server;
import org.mortbay.jetty.servlet.Context;
import org.mortbay.jetty.servlet.ServletHolder;

/**
 * Hello world!
 *
 */
public class HessianServer {


    final Server server = new Server(2345);

    protected void start() throws Exception {

        Context root = new Context(server,"/",Context.ALL);

        HessianServlet servlet = new HessianServlet();
        servlet.setHomeAPI(TestService.class);
        servlet.setHome(new TestServiceImpl());

        root.addServlet(new ServletHolder(servlet),"/");

        server.start();

        new Thread() {

            public void run() {
                try {
                    server.join();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
        }.start();

        //Thread.sleep(9999999999999L);

    }

    protected void stop() throws Exception {
        server.stop();
    }

    public static void main( String[] args ) throws Exception {
        new HessianServer().start();
    }
}
