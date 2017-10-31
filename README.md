# NwbQueryEngine
This is a prototype of a query engine used for NeurodataWithoutBorders format

Getting started:
==

Prerequisites
--
- For running only [Installed JRE 8](http://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html)

- For programmers 
1. [Installed JDK 8](http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html)
2. [Installed Maven](https://maven.apache.org/download.cgi)
3. Installed your favorite IDE


Running
==
- compile with mvn clean package -Djava.library.path=src/main/resources/

- run java -Djava.library.path=src/main/resources/ -jar target/nwbqueryengine-1.0-SNAPSHOT-jar-with-dependencies.jar file [query](doc/queries.md)

e.g: java -Djava.library.path=src/main/resources/ -jar target/nwbqueryengine-1.0-SNAPSHOT-jar-with-dependencies.jar /home/user/file.nwb "processing=(electrode_idx>30)"

Python support:
==

- install [py4j](https://www.py4j.org/install.html) by pip install py4j
- run server java -Djava.library.path=PATH_TO_RESOURCES_DIR -jar target/nwbqueryengine-1.0-SNAPSHOT-jar-with-dependencies.jar pyserver
- For running on a remote host parameter -Dhost.ip=remote-host-ip must be used


- run [python code](doc/example.py) such as:
```python
 >>> from py4j.java_gateway import JavaGateway
 >>> from py4j.java_gateway import GatewayParameters
 >>> gateway = JavaGateway() # for localhost
 >>> gateway = JavaGateway(gateway_parameters=GatewayParameters(address='remote host ip')) # or for remote host
 >>> res = gateway.executeQuery("file or dir with nwb files", "query")
 >>> for x in res:
 ...     print (x)

```

Maven
==
        <dependency>
            <groupId>edu.berkeley</groupId>
            <artifactId>nwbqueryengine</artifactId>
            <version>1.0-SNAPSHOT</version>
        </dependency>
        
        <repository>
            <id>snapshots</id>
            <url>http://eeg.kiv.zcu.cz:8081/nexus/content/repositories/snapshots/</url>
        </repository>
