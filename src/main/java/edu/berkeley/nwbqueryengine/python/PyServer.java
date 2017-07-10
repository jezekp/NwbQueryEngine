package edu.berkeley.nwbqueryengine.python;

import edu.berkeley.nwbqueryengine.Main;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import py4j.GatewayServer;

/**
 * Created by petr-jezek on 10.7.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public class PyServer {

    private Log logger = LogFactory.getLog(getClass());

    public void start() {
        GatewayServer gatewayServer = new GatewayServer(new PythonFacade());
        gatewayServer.start();
        logger.info("Python gateway server started...");
    }
}
