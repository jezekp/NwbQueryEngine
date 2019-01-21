from py4j.java_gateway import JavaGateway

gateway = JavaGateway() # for localhost


with open('/home/petr-jezek/data/nwb_datasets/newDatasets/alm-1/datafiles/nwb_files/data_structure_ANM210861_20130701.nwb', mode='rb') as file: # b is important -> binary
            fileContent = file.read()

b = bytearray(fileContent)
res = gateway.executeQuery(b, "epochs='start_time>200 & stop_time<400 | stop_time>1600'")

for x in res:
    print (x)
