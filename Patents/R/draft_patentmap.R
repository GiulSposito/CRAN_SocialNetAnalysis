library(patentsview)

keywords <- c("machine learning","deep learning", "neural network","artificial intelligence","statistical learning", "data mining", "predictive model")

# query
query <- qry_funs$and(
  qry_funs$gte(patent_year=1990),
  qry_funs$or(
    qry_funs$text_phrase(patent_title=keywords),  
    qry_funs$text_phrase(patent_abstract=keywords)
  )
)

# Create a list of fields:
fields <- c(
  c("patent_number", "patent_year"),
  get_fields(endpoint = "patents", groups = c("assignees", "cpcs"))
)

# Send HTTP request to API's server:
pv_res <- search_pv(query = query, fields = fields, all_pages = TRUE)

install.packages("leaflet")
install.packages("htmltools")

library(leaflet)
library(htmltools)
library(dplyr)
library(tidyr)

data <-
  pv_res$data$patents %>%
  unnest(assignees) %>%
  select(assignee_id, assignee_organization, patent_number,
         assignee_longitude, assignee_latitude) %>%
  group_by_at(vars(-matches("pat"))) %>%
  mutate(num_pats = n()) %>%
  ungroup() %>%
  select(-patent_number) %>%
  distinct() %>%
  mutate(popup = paste0("<font color='Black'>",
                        htmlEscape(assignee_organization), "<br><br>Patents:",
                        num_pats, "</font>")) %>%
  mutate_at(vars(matches("_l")), as.numeric) %>%
  filter(!is.na(assignee_id))

leaflet(data) %>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
  addCircleMarkers(lng = ~assignee_longitude, lat = ~assignee_latitude,
                   popup = ~popup, ~sqrt(num_pats), color = "yellow")


library(ggplot2)
library(RColorBrewer)

data <-
  pv_res$data$patents %>%
  unnest(cpcs) %>%
  filter(cpc_subgroup_id != "H04L63/02") %>% # remove patents categorized into only top-level category of H04L63/02
  mutate(
    title = case_when(
      grepl("filtering", .$cpc_subgroup_title, ignore.case = T) ~
        "Filtering policies",
      .$cpc_subgroup_id %in% c("H04L63/0209", "H04L63/0218") ~
        "Architectural arrangements",
      grepl("Firewall traversal", .$cpc_subgroup_title, ignore.case = T) ~
        "Firewall traversal",
      TRUE ~
        .$cpc_subgroup_title
    )
  ) %>%
  mutate(title = gsub(".*(?=-)-", "", title, perl = TRUE)) %>%
  group_by(title, patent_year) %>%
  count() %>%
  ungroup() %>%
  mutate(patent_year = as.numeric(patent_year))

ggplot(data = data) +
  geom_smooth(aes(x = patent_year, y = n, colour = title), se = FALSE) +
  scale_x_continuous("\nPublication year", limits = c(2007, 2016),
                     breaks = 2007:2016) +
  scale_y_continuous("Patents\n", limits = c(0, 700)) +
  scale_colour_manual("", values = brewer.pal(5, "Set2")) +
  theme_bw() + # theme inspired by https://hrbrmstr.github.io/hrbrthemes/
  theme(panel.border = element_blank(), axis.ticks = element_blank())
