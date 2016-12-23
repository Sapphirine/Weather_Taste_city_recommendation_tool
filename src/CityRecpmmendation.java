package kk126203.test;

import java.io.File;
import java.util.List;

import org.apache.mahout.cf.taste.impl.model.file.FileDataModel;
import org.apache.mahout.cf.taste.impl.neighborhood.ThresholdUserNeighborhood;
import org.apache.mahout.cf.taste.impl.recommender.GenericUserBasedRecommender;
import org.apache.mahout.cf.taste.impl.similarity.PearsonCorrelationSimilarity;
import org.apache.mahout.cf.taste.model.DataModel;
import org.apache.mahout.cf.taste.neighborhood.UserNeighborhood;
import org.apache.mahout.cf.taste.recommender.RecommendedItem;
import org.apache.mahout.cf.taste.recommender.UserBasedRecommender;
import org.apache.mahout.cf.taste.similarity.UserSimilarity;

public class CityRecpmmendation
{
    public static void main( String[] args ) throws Exception
    {
    	DataModel m = new FileDataModel(new File("data/dataset.csv"));
    	UserSimilarity s = new PearsonCorrelationSimilarity(m);
    	UserNeighborhood n = new ThresholdUserNeighborhood(0.1, s, m);
    	UserBasedRecommender r = new GenericUserBasedRecommender(m, n, s);
    	List<RecommendedItem> re = r.recommend(5, 1);
    	for (RecommendedItem string : re) 
    	{
    	  System.out.println("For the city that fits you would be");
    	  System.out.println(string);
    	}
    }
}
