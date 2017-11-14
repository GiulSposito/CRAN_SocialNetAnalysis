install.packages("patentsview")

library(patentsview)
library(jsonlite)

keywords <- c("machine learning","deep learning", "neural network","artificial intelligence","statistical learning", "data mining", "predictive model")
fields <- c("")

qry <- '{"inventor_last_name":["Whitney","Hopper"]}'
qry <- '{"_contains":{"inventor_last_name":"Whitney"}}'
qry <- '{"_text_phrase":{"patent_title":"deep learning"}}'
qry <- '{"_or":[{"_text_phrase":{"patent_title":"deep learning"}},{"_text_phrase":{"patent_title":"machine learning"}}]}'


qry <- qry_funs$and(
  qry_funs$gte(patent_year=2000),
  qry_funs$or(
    qry_funs$text_phrase(patent_title=keywords),  
    qry_funs$text_phrase(patent_abstract=keywords)
  )
)



res <- search_pv(query = qry, fields = NULL)
View(res$query_results)



