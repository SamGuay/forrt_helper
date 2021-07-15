#####                     FORRT Glossary Helper                       #####
# (c) FORRT and collaborators
# Script developped by Samuel Guay to parse 
# Google Documents into markdown / Hugo 
# V0.0.1        
# Load packages -----------------------------------------------------------

# if it says package not installed:
# run renv::restore() in the console to install all packages
options(warn = -1, echo = F)
suppressMessages({ 
  library(rvest)
  library(magrittr)
  library(purrr)
  library(stringr)
  # library(xml2)
  library(jsonlite)
  })

# Set constants -----------------------------------------------------------
args <- commandArgs()
URL <- args[7]
# A-L URL: https://docs.google.com/document/d/e/2PACX-1vQ7uYU9S6wST4943lEYdRkaHtiNqbnOjpRct7VJM4n2Uh9MkmeAEkThUeKKVnIvgcP5K0LUICSCHHLl/pub
# M-Z URL: https://docs.google.com/document/d/e/2PACX-1vRgRWIdA80Dr072GDUKeE71xEgkXnrfxeoBB-9WdwPM13GoLm5mMLIx2DDiH7DSTt2rlMPZ4SUVCqS3/pub
# URL <- "https://docs.google.com/document/d/e/2PACX-1vRKw2Gq8mnB72wJIigfsuGnJaKASdFm6FnCCmC0K2cf8RBit2BzXDHcXJkbODgjwfrcBhELq0o3Tmu9/pub"
# Import HTML and extract relevant bits -----------------------------------

cat(paste0("\n\nImporting HTML from ", URL, "\n\n"))

raw_html <- read_html(URL) %>% html_elements("#contents")

cat("Now extracting relevant bits \n\n")
raw_titles <- raw_html %>% html_elements("h4")
# remove last random h4 (needed to import the last term, but not needed)
raw_titles <- head(raw_titles, -1)
# "//h4[3] | //h4[4]/preceding-sibling::*[preceding-sibling::h4[3]]" is a 
# working XPATH expression to grab an H4 (`//h4[i]`) and everything (`*`) until the
# next H4 between 2 h4 title (//h4[i+1]/preceding-sibling::*[preceding-sibling::h4[i]])
# both numbers at extremities needs to be the same and the `*` can be replaced by
# any relevant tags (eg `p` for paragraphs only)
# inspired by: https://stackoverflow.com/a/10864540 & https://devhints.io/xpath &
# https://stackoverflow.com/a/7838880
# had to use XPATH because CSS selectors in Google docs are weird AF and
# not static, so this should be safer. Also, no child, only siblings here (:
# TODO: Fix the last word to be picked (not working now since i+1 doesn't exist for the last word)
#       potential solution: change last number with [last()] ?
#       "Temporary" solution: Add a random h4 at the end of the last word.

# prepare XPATH expression to be _glued_
xpath_exp <- "//h4[{i}] | //h4[{i+1}]/preceding-sibling::p[preceding-sibling::h4[{i}]]"

# Create a vector with an xpath expression for each word + def to be extracted
xpath_glued <- glue::glue(xpath_exp,
                          i = seq_along(raw_titles))

# Extract each word + related content into their own list
raw_terms <- map(xpath_glued, ~html_elements(x = raw_html, xpath = .x)) %>%
  map(html_text, trim = T) %>%
  compact() %>%
  # rename list by their Title
  set_names(raw_titles %>%
            html_text() %>%
            sub(pattern = "\\s*\\[complete\\]", replacement = "") %>%
            # for some unknown reason, sub (or trimws()) wouldnt remove all
            # weirdly encoded whitespace, so had to use stringr::str_squish()
            # thanks to https://stackoverflow.com/a/49311193 for confirming it.
            stringr::str_squish())

# Tibbling terms ----------------------------------------------------------
cat("Currently wrangling and tidying the data \n\n")
# This function was created to turn logical value (when str_subset returns False) into NA and also to convert empty string "" into NA, otherwise pass the temp_var into the tibble. Useful in the for loop below 
input_safely <- function(temp_var) ifelse(length(temp_var) != 0,
                                          ifelse(temp_var == "", NA, temp_var),
                                          NA)

# Using var = character() instead of var = "" to avoid adding an empty row
# during the creation
terms <- tibble::tibble(word = character(),
                       definition = character(),
                       related_terms = character(),
                       references = character(),
                       alt_definition = character(),
                       alt_related_terms = character(),
                       # reference = "", # TODO: to be deleted in 2.0 (view reference: above). The template on the GDoc has duplicate reference keywords as of 2021-07-07.
                       drafted_by = character(),
                       reviewed_by = character())

for (term in raw_terms) { 
  temp_word <- term[1] %>% 
      sub(pattern = "\\s*\\[complete\\]", replacement = "") %>%
      # for some unknown reason, sub (or trimws()) wouldnt remove all
      # weirdly encoded whitespace, so had to use stringr::str_squish()
      # thanks to https://stackoverflow.com/a/49311193 for confirming it.
      stringr::str_squish()
  
  temp_def <- term[2] %>%
    sub(pattern = "^Definition[s():\\s]+", replacement = "") %>%
    stringr::str_squish()
  
  temp_rel_terms <- term %>%
    stringr::str_subset(pattern = "^Related term[(\\s\\ss)]*:") %>%
    sub(pattern = "\\s*Related term[s():\\s]+", replacement = "") %>%
    stringr::str_squish() %>% as.character()
  
  temp_references <- term %>%
    stringr::str_subset(pattern = "Reference[s():\\s]*") %>%
    sub(pattern = "Reference[s():\\s]+", replacement = "") %>%
    stringr::str_squish()
  
  temp_alt_def <- term %>% stringr::str_subset(pattern = "Alternative definition[s():\\s]*") %>%
    stringr::str_replace(pattern = "Alternative definition[s():\\s]*\\(*[if applicable)]*\\s*", replacement = "") %>%
    stringr::str_squish() %>%
    as.character() # needed when it's empty.. dunno why?
  
  temp_alt_terms <- term %>% stringr::str_subset(pattern = "Related term[s] to alternative") %>%
    stringr::str_replace(pattern = "\\s*Related terms? to alternative definition[s():\\s]*\\(*[if applicable)]*\\s*", replacement = "") %>%
    stringr::str_squish() #%>%
    # as.character() # needed when it's empty.. dunno why?
  
  temp_drafted <- term %>%
    stringr::str_subset(pattern = "[Dd]rafted") %>%
    sub(pattern = "\\s*[Ooriginally]*\\s*[Dd]rafted\\s[Bb]y[s():\\s]*", replacement = "") %>%
    stringr::str_squish()
  
  temp_reviewed <- term %>%
    stringr::str_subset(pattern = "[Rr]eviewed [(or Edited) by]*:") %>%
    sub(pattern = "\\s*[Rr]eviewed\\s*[(]or\\s*[Ee]dited[)]\\s*[Bb]y[s():\\s]*", replacement = "") %>%
    stringr::str_squish()
  
  terms <- terms %>% 
    tibble::add_row(word = input_safely(temp_word),
                   definition = input_safely(temp_def),
                   related_terms = input_safely(temp_rel_terms),
                   references = input_safely(temp_references),
                   alt_definition = input_safely(temp_alt_def),
                   alt_related_terms = input_safely(temp_alt_terms),
                   drafted_by = input_safely(temp_drafted),
                   reviewed_by = input_safely(temp_reviewed))
  }

#filter out first rows that shouldnt be there (Term placeholder/template + empty term)
terms <- terms %>% dplyr::filter(!word %in% c("Term placeholder", "Word placeholder", "Word Placeholder"))
terms <- terms %>% tidyr::drop_na(word)
number_of_terms <- nrow(dplyr::distinct(terms, word))
cat(paste0("There are ", number_of_terms, " extracted so far. \n\n"))
# Tidy the terms tibble ------------------------------------------------

# strip random words in term title

terms <-  terms %>% dplyr::mutate(dplyr::across(word, str_replace, "\\**almost done\\**|\\**almost complete\\**|\\#*review needed\\#*", ""))

# When working with tibbles/database, it's easier to work with separate() or separate_rows() than str_split() given the nature of the data.
# cant find out how to fix the Incompatible lengths error if I put everything in the same separate_rows() call, so piping party it is!
# Dunno why \\s* doesn't 0 or MORE whitespace.. seems to be stuck at 1.. 
# TODO: should a , be added as a separator?
separator <- "\\s*\\s*\\s*[;]\\s*\\s*\\s*" # select whitespace before and after ; or , (thus trim everything)
terms_tidy <- terms %>%
  tidyr::separate_rows(related_terms, sep = separator) %>%
  tidyr::separate_rows(references, sep = separator) %>%
  tidyr::separate_rows(alt_related_terms, sep = separator) %>%
  tidyr::separate_rows(drafted_by, sep = separator) %>%
  tidyr::separate_rows(reviewed_by, sep = separator)

terms_tidy <- terms_tidy %>% 
  dplyr::mutate(dplyr::across(where(is.character), str_trim))

# cat(paste0("Tidied data are now being exported as csv \n\n"))
# write.csv(terms_tidy, "a-k.csv")

# Setup JSON --------------------------------------------------------------
cat(paste0("Now onto generating an .md file for each term \n\n"))
# obj = var to be split and separator = same sep determined above to tidy the data.
split_attack <- function(obj) str_split(obj, pattern = separator)
terms_json <- terms %>%
  dplyr::mutate(related_terms = str_split(related_terms, separator),
                references = str_split(references, separator),
                alt_related_terms = str_split(alt_related_terms, separator),
                reviewed_by = str_split(reviewed_by, separator),
                drafted_by = str_split(drafted_by, separator))

terms_json <- terms_json %>% dplyr::rename(title = word)

# Generate .md pages ------------------------------------------------------

for (row in 1:nrow(terms_json)) {
  temp_name = str_replace_all(pattern = "[^a-zA-Z0-9]", replacement = "-", string = terms_json[row,1])
  temp_name = str_replace_all(pattern = "--", replacement = "-", string = temp_name)
  temp_name = str_replace_all(pattern = "-$", replacement = "", string = temp_name)
  temp_name = str_replace_all(pattern = "^-", replacement = "", string = temp_name)
  temp_name = str_trunc(string = temp_name, width = 35, side = "right", ellipsis = "")
  # path = paste0(args[6], "/", tolower(temp_name), ".md")
  # add auto_unbox = T to toJSON() call to remove empty list to be created (but loosing the parameter name)
  path = paste0(args[6], "/", tolower(temp_name), ".md")
  
# auto_unbox = T doesnt work as I wished it would, so had to make this funky gymnastic to make it work.
  temp_json <- unbox(fromJSON(toJSON(terms_json[row,]))) 
  write_json(path = path, x = temp_json, pretty = T)
}

cat(paste0("Done, thank you for reading the comments. Hugo, now is your turn to shine. \n\n"))
