library(devtools)
install_github("ropensci/rentrez")

library(rentrez)

# search and find the paper's pubmed id:
paper <- entrez_search(db="pubmed", term="Donor AND Human AND Milk AND Effects AND  Storage AND Heat AND Treatment AND  Oxidative AND Stress AND Markers")
cites = entrez_link(dbfrom = "pubmed", db = "all", id = paper$ids)





pubmed_get_friends = function(pubmed_id, edge_type){
  # this is analogous to:
  # rtweet::get_friends(userID)

  # check if edge_type is valid...
  valid_types <- c(
    'pubmed_pubmed',
    'pubmed_pubmed_alsoviewed',
    'pubmed_pubmed_citedin',
    'pubmed_pubmed_refs',
    'pubmed_pubmed_reviews'
  )
  # Check if all elements of edge_type are in valid_types
  if (!all(edge_type %in% valid_types)) {
    invalid_entries <- edge_type[!edge_type %in% valid_types]
    stop("Invalid edge_type entries: ", paste(invalid_entries, collapse = ", "), ".")
  }
  max_attempts <- 3
  success <- FALSE

  for(i in 1:max_attempts){
    tryCatch({
      cites <- entrez_link(dbfrom = "pubmed", db = "all", id = pubmed_id)
      success <- TRUE
      break
    }, error = function(e){
      if(i == max_attempts){
        print(pubmed_id)
        stop("Maximum attempts reached. Could not execute the code.")
      }
      warning(sprintf("Attempt %d failed. Trying again...", i))
    })
  }

  if (!success) {
    stop("Failed to retrieve pubmed_id = ", pubmed_id, " after 3 attempts.")
  }

  # cites = entrez_link(dbfrom = "pubmed", db = "all", id = pubmed_id)


  all_edges_to_return = cites$links[edge_type] %>% unlist %>% unique
  all_edges_to_return

}


test_id = "29624432"
test_id = "29867837"

cites3 = entrez_link(dbfrom = "pubmed", db = "all", id = test_id)
str(cites3)
