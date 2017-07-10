# NwbQueryEngine
This is a prototype of a query engine used for NeurodataWithoutBorders dataSet format

Getting started:
-

- run java -Djava.library.path=PATH_TO_RESOURCES_DIR -jar NwbQueryEngine.jar

Python support:
- 

- install py4j by pip install py4j (https://www.py4j.org/install.html)
- run server java -Djava.library.path=PATH_TO_RESOURCES_DIR -jar NwbQueryEngine.jar pyserver

- run python code such as:
```python
 >>> from py4j.java_gateway import JavaGateway
 >>> gateway = JavaGateway()
 >>> random = gateway.jvm.java.util.Random()
 >>> addition_app = gateway.entry_point 
 >>> gateway.executeQuery("file or dir with nwb files", "query")
```
