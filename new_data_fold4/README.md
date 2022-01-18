# Overview
* Create a new dataset (that contains variables generated from Sentiment Analysis, merged with fips code and designed media market) for further analysis 
* "df_merge_New_issue65" creates the new dataset by merging the sentiment datasets and state; city; zip; dma datasets:

sentiment_df_AFINN.Rdata
sentiment_df_Hu & Liu.Rdata
* sentiment_df_AFINN_neg_count.Rdata
* state - stfips.csv
* ctyfips - stfips - zip - cd - dma.csv
* cd - zip.csv
* This outputs  "merge_df_New_issue65.Rdata" which is a dataset that merges raw text, sentiment scores, and different fips code with DMA for further analysis. 
