install.packages("patentsview")

library(patentsview)


keywords <- c("machine learning","deep learning", "neural network","artificial intelligence","statistical learning", "data mining", "predictive model")
fields = c("patent_abstract", "patent_average_processing_time",
           "inventor_first_name", "inventor_total_num_patents")

fields <- get_fields(endpoint = "patents", groups = c("patents", "inventors"))

qry <- '{"inventor_last_name":["Whitney","Hopper"]}'
qry <- '{"_contains":{"inventor_last_name":"Whitney"}}'
qry <- '{"_text_phrase":{"patent_title":"deep learning"}}'
qry <- '{"_or":[{"_text_phrase":{"patent_title":"deep learning"}},{"_text_phrase":{"patent_title":"machine learning"}}]}'


qry <- qry_funs$and(
  qry_funs$gte(patent_year=1990),
  qry_funs$or(
    qry_funs$text_phrase(patent_title=keywords),  
    qry_funs$text_phrase(patent_abstract=keywords)
  )
)

res <- search_pv(query = qry, fields = fields)
res

View(res$query_results)

View(res$data$patents)

View(fieldsdf[fieldsdf$can_query=="y", ])

str()

