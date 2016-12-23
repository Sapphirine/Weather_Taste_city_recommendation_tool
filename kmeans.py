from pyspark.ml.clustering import KMeans
dataset = spark.read.format("libsvm").load("spark_pca.txt")
kmeans = KMeans().setK(10).setSeed(10)
model = kmeans.fit(dataset)
wssse = model.predict(dataset)
print("Within Set Sum of Squared Errors = " + str(wssse))
centers = model.clusterCenters()
print("Cluster Centers: ")
for center in centers:
    print(center)
