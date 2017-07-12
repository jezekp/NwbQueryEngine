package edu.berkeley.nwbqueryengine.python;

import edu.berkeley.nwbqueryengine.io.Facade;
import edu.berkeley.nwbqueryengine.io.FileFacade;
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

    public void start(Facade facade) {
        GatewayServer gatewayServer = new GatewayServer(facade);
        gatewayServer.start();
        logger.info("Python gateway server started...");
    }
}
