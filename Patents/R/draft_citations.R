library(patentsview)
library(tidyr)
library(dplyr)

keywords <- c("machine learning","deep learning", "neural network","artificial intelligence",
              "statistical learning", "data mining", "predictive model")

fields = c("patent_id","patent_title", "patent_abstract", "patent_num_us_application_citations",
           "inventor_id", "inventor_first_name", "inventor_total_num_patents",
           "assignee_id", "assignee_organization",
           "uspc_mainclass_title", "uspc_subclass_title")


qry <- qry_funs$and(
  qry_funs$gte(patent_year=1990),  # patents do ano 1990 para frente (Year patent was granted)
  qry_funs$or(
    qry_funs$text_phrase(patent_title=keywords),   # com (pelo menos) uma das keywords no titulo
    qry_funs$text_phrase(patent_abstract=keywords) # com (pelo menos) uma das keywords no abstract
  )
)


res <- search_pv(query = qry, fields = fields)
res
View(res$data$patents)

patents <- res$data$patents %>%
  select( patent_id, patent_title, patent_abstract, patent_num_us_application_citations)

inventors <- res$data$patents %>%
  select( patent_id, inventors ) %>%
  tidyr::unnest(inventors)

assignees <- res$data$patents %>%
  select( patent_id, assignees )
  tidyr::unnest(assignees) %>%
  View()

category <- res$data$patents %>% 
  select( patent_id, uspcs ) %>%
  tidyr::unnest(uspcs) 






fieldsdf %>%
  filter( grepl("app", field, fixed = TRUE) ) %>%
  select( -endpoint, -can_query, -common_name ) %>%
  distinct() %>%
  arrange( group, field)  %>%
  View()

