from pyspark.ml.clustering import KMeans
dataset = spark.read.format("libsvm").load("sample.txt")
kmeans = KMeans().setK(2).setSeed(1)
model = kmeans.fit(dataset)
wssse = model.computeCost(dataset)
print("Within Set Sum of Squared Errors = " + str(wssse))
centers = model.clusterCenters()
print("Cluster Centers: ")
for center in centers:
    print(center)

