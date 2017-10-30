package edu.berkeley.nwbqueryengine;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * Created by petr-jezek on 20.7.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public class Properties {

    private static Log logger = LogFactory.getLog(Properties.class);

    private Properties() {

    }

    private static Properties properties = null;
    private static String hostIp;


    public static Properties get() {
        if (properties == null) {
            properties = new Properties();
            java.util.Properties p = System.getProperties();
            hostIp = p.getProperty("host.ip");
            logger.info("Reading properties:");
            logger.info("Host IP: " + hostIp);
        }
        return properties;
    }

    public String getHostIp() {
        return get().getHostIp();
    }
}
