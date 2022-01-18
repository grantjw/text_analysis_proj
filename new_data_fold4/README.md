# Overview
* Create a new dataset (that contains variables generated from Sentiment Analysis, merged with fips code and designed media market) for further analysis 
* "df_merge_New_issue65" creates the new dataset by merging the sentiment datasets and state; city; zip; dma datasets:

1) sentiment_df_AFINN.Rdata
2) sentiment_df_Hu & Liu.Rdata
3) sentiment_df_AFINN_neg_count.Rdata
4) state - stfips.csv
5) ctyfips - stfips - zip - cd - dma.csv
6) cd - zip.csv

* This outputs  "merge_df_New_issue65.Rdata" which is a dataset that merges raw text, sentiment scores, and different fips code with DMA for further analysis. 
* With this data, we have two csv files "Wesleyan TV ad sentiment by county.csv" and "Wesleyan TV ad sentiment by region.csv" 

