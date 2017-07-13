# NwbQueryEngine
This is a prototype of a query engine used for NeurodataWithoutBorders format

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
 >>> res = gateway.executeQuery("file or dir with nwb files", "query")
 >>> res[0].getDataset()
 >>> res[0].getValue()
```
