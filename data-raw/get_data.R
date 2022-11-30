library(patentsview)
library(dplyr)
library(devtools)

# Rewrite of the https://docs.ropensci.org/patentsview/articles/top-assignees.html
# vignette, using the new version of the patentsview R package that hits the
# new version of the API.  Here we write the data to a file rather than 
# instantiating a datatable

# To actually run this you would need to request an API key here
# https://patentsview.org/apis/keyrequest and set an environmental variable
# PATENTSVIEW_API_KEY to its value

# With the recent API change that limits the overall result set size, we now look
# for databases (plural) where we used to use the singular database for this vignette.
# We also exclude patents without an assignee organization.

# We first need to write a query, we'll use the package's DSL. Our query will look for 
# "databases" in either the patent title or abstract. Note, this isn't a terribly 
# good way to ID our patents, but it will work for the purpose of demonstration. 
# Users who are interested in writing higher-quality queries could consult the 
# large body of research that has been done in field of patent document retrieval.

query <- with_qfuns(
  and(
     neq("assignees_at_grant.organization" = ""),
     or(
       text_phrase(patent_abstract = "databases"),
       contains(patent_title = "databases")
     )
   )
)

# Our query connverted to the API's query language
#> {"_and":[{"_neq":{"assignees_at_grant.organization":""}},{"_or":[{"_text_phrase":{"patent_abstract":"databases"}},{"_contains":{"patent_title":"databases"}}]}]}

# Create a list of the fields we'll need for the analysis (The API 
# lets you specify which fields you want returned.)

fields <- c(
  "patent_number", "patent_date","patent_year",
  "patent_num_us_patents_cited",
  "assignees_at_grant.organization",  # fulltext field now if used in q:
  "assignees_at_grant.assignee_id"  # the assignee fields come back in a nested object
)

# Send HTTPS requests to the PatentsView API to get the data, paging through the
# result set as needed, sleeping as necessary if we get throttled.  
pv_out <- search_pv(query, fields = fields, all_pages = TRUE, per_page = 1000)

# Now let’s identify who the top assignees are based on how many patents they have in 
# our data set. We’ll also calculate how many total patents these assignees have and what 
# fraction of their total patents relate to databases.

# Unnest the data frames that are stored in the assignee list column
dl <- unnest_pv_data(pv_out$data, "patent_number")

# dl
#> List of 2
#>  $ assignees_at_grant:'data.frame':  7150 obs. of  3 variables:
#>   ..$ patent_number: chr [1:7150] "4751635" ...
#>   ..$ assignee     : chr [1:7150] "https://search.patentsview.org/api/v1/assi"..
#>   ..$ organization : chr [1:7150] "Bell Communications Research, Inc." ...
#>  $ patents           :'data.frame':  7598 obs. of  4 variables:
#>   ..$ patent_number              : chr [1:7598] "4751635" ...
#>   ..$ patent_date                : chr [1:7598] "1988-06-14" ...
#>   ..$ patent_year                : int [1:7598] 1988 1988 ...
#>   ..$ patent_num_us_patents_cited: int [1:7598] 3 18 ...

# We shouldn't need the filter (we queried for non empty string orginazations) but
# some are NA somehow

# We don't get the assignee_total_num_patents back from the patents endpoint any longer.
# We'll have to make a call to the assignee endpoint once we know who the top 75
# assignees are.

# Requested assignees_at_grant.assignee_id comes back in the assignees_at_grant object 
# with key "assignee" and value like https://search.patentsview.org/api/v1/assignee/35/
# We want to parse out the assignee_ids (35 in the example value)
# str_extract(assignee, "(\\d+)(?=/$)")

# Create a data frame with the top 75 assignees:
top_asgns <-
  dl$assignees_at_grant %>%
    filter(!is.na(organization)) %>% # some patents are assigned to an inventor (not an org)
    mutate(assignee_id = sub(".*/(\\d+)/$", "\\1", assignee)) %>%
    group_by(organization, assignee_id) %>% 
    summarise(db_pats = n()) %>% 
    ungroup() %>%
    arrange(desc(db_pats)) %>%
    slice(1:75)

# Now that we have the assignee_id's we can call the assignee endpoint to get the
# total number of patents for each of our top_asgns 

# Specify the fields we want returned from the API call
assignee_fields <- c(
  "assignee_id", "organization", "num_patents" 
)

# DSL to query for assignees matching the assignee_ids of our top 75 assignees
assignee_query =  qry_funs$eq(assignee_id = as.integer(top_asgns$assignee_id))

# we'll post to the API, the query is a pretty large string (an "or" of 75 assignee_ids)
assignee_out <- search_pv(assignee_query , fields = assignee_fields, all_pages = TRUE, 
   per_page = 1000, endpoint = "assignees", method = "POST")

assignee_counts <- unnest_pv_data(assignee_out$data, "assignee_id")

# assignee_counts
#> List of 1
#>  $ assignees:'data.frame':   75 obs. of  3 variables:
#>   ..$ assignee_id : int [1:75] 3 6 ...
#>   ..$ organization: chr [1:75] "Xerox Corporation" ...
#>   ..$ num_patents : int [1:75] 23495 80931 ...

# Here we redo top_asgns now that we have all the fields we need.
# We join in the total counts and mutate in the percentages 
top_asgns <- dl$assignees_at_grant %>%
   inner_join(assignee_counts$assignees) %>%
   rename(ttl_pats = num_patents) %>%
   group_by(organization, ttl_pats) %>%
   summarise(db_pats = n()) %>% 
   mutate(frac_db_pats = round(db_pats / ttl_pats, 3)) %>%
   select(c(1, 3, 2, 4))  %>%
   arrange(desc(db_pats))

# Here we stop short of creating the datatable, as the vignette does.  
# Instead, we'll write the blended patentsview data to a file for our shiny app's use

# write a csv for curious humans to read
write.csv(top_asgns, "data-raw/top_asgns.csv", row.names = FALSE)
use_data(top_asgns, internal = FALSE, overwrite = TRUE)
use_data(top_asgns, internal = TRUE, overwrite = TRUE)

