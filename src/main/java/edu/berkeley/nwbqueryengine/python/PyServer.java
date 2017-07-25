package edu.berkeley.nwbqueryengine.python;

import edu.berkeley.nwbqueryengine.Properties;
import edu.berkeley.nwbqueryengine.io.Facade;
import edu.berkeley.nwbqueryengine.io.FileFacade;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import py4j.GatewayServer;

import java.net.InetAddress;
import java.net.UnknownHostException;

/**
 * Created by petr-jezek on 10.7.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public class PyServer {

    private Log logger = LogFactory.getLog(getClass());

    public void start(Facade facade) {
        try {
            InetAddress address = InetAddress.getByName(Properties.get().getHostIp());
            GatewayServer gatewayServer = new GatewayServer(
                    facade,
                    GatewayServer.DEFAULT_PORT,
                    GatewayServer.DEFAULT_PYTHON_PORT,
                    address,
                    address,
                    GatewayServer.DEFAULT_CONNECT_TIMEOUT,
                    GatewayServer.DEFAULT_READ_TIMEOUT,
                    null
            );
            gatewayServer.start();
            int port = gatewayServer.getListeningPort();
            logger.info("Python gateway server started... " + address.getCanonicalHostName() + " " + address.getHostAddress() + ":" + port);
        } catch (UnknownHostException e) {
            logger.error(e);
        }
    }
}
