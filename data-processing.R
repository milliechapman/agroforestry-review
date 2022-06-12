# Load the package required to read JSON files (a few different options to test out!)
library(rjson)
library(tidyverse)
library(tidyjson)
library(jsonlite)

#These are search terms
search <- read_csv("data/codebook_new.csv")$subterm
  
n_articles <- 24669
df <- data.frame(matrix(nrow = n_articles,ncol = (length(search)+3)))
colnames(df) <- c("title","yr","total_words",search)

data <- "data/6bd3f05f-5304-8353-44a7-351ddd024b2e-jsonl.jsonl.gz"
lines <- readLines(data, n=-1L)

nums <- c(1:1325, 1327:5855, 5857:7668,7701:8199, 8202:18377, 
          18382:19851,19855:20388,
          20392:22553)

library(corpus)
z<-term_stats(lines[1:24669], drop = stopwords_en, drop_punct = TRUE)
write_csv(z, "data/output/all-words.csv")


# search unigrams
for (i in nums) {
  s <- lapply(lines[i], fromJSON)
  #df$title[i] <- unlist(s[[1]]$title)[1]
  df$yr[i] <- unlist(s[[1]]$publicationYear)[1]
  #df$journal[i] <- unlist(s[[1]]$isPartOf)[1]
  grams <- as.data.frame(unlist(s[[1]]$unigramCount))
  df$total_words[i] <- length(grams$`unlist(s[[1]]$unigramCount)`)
  terms <- rownames_to_column(grams,"term")
  colnames(terms) <- c("term","count")
  for (j in 1:length(search)) {
    n <- 3+j
    matches <- which(terms$term==search[j])
    if (length(matches) > 0) {
      df[i,n] <- terms$count[matches]
    }
    else {
      df[i,n] <- 0
    }
  }
}

write_csv(df, "data/output/search-descriptive2.csv")


## reports


n_articles <- 647
df <- data.frame(matrix(nrow = n_articles,ncol = (length(search)+3)))
colnames(df) <- c("title","yr","total_words",search)
data <- "data/reports-jsonl.jsonl.gz"
lines <- readLines(data, n=-1L)

nums <- c(1:647)

#library(corpus)
#z<-term_stats(lines[1:24669], drop = stopwords_en, drop_punct = TRUE)

# search unigrams
for (i in nums) {
  s <- lapply(lines[i], fromJSON)
  #df$title[i] <- unlist(s[[1]]$title)[1]
  df$yr[i] <- unlist(s[[1]]$publicationYear)[1]
  #df$journal[i] <- unlist(s[[1]]$isPartOf)[1]
  grams <- as.data.frame(unlist(s[[1]]$unigramCount))
  df$total_words[i] <- length(grams$`unlist(s[[1]]$unigramCount)`)
  terms <- rownames_to_column(grams,"term")
  colnames(terms) <- c("term","count")
  for (j in 1:length(search)) {
    n <- 3+j
    matches <- which(terms$term==search[j])
    if (length(matches) > 0) {
      df[i,n] <- terms$count[matches]
    }
    else {
      df[i,n] <- 0
    }
  }
}

write_csv(df, "data/output/search-descriptive-reports2.csv")


search <- c("climate mitigation", "carbon sequestration", "climate adaptiation")
# search bigrams
for (i in 1:n_articles) {
  s <- lapply(lines[i], fromJSON)
  #df$title[i] <- unlist(s[[1]]$title)[1]
  df$yr[i] <- unlist(s[[1]]$publicationYear)[1]
  #df$journal[i] <- unlist(s[[1]]$isPartOf)[1]
  bigrams <- as.data.frame(unlist(s[[1]]$bigramCount))
  terms <- rownames_to_column(bigrams,"term")
  colnames(terms) <- c("term","count")
  for (j in 1:length(search)) {
    n <- 3+j
    matches <- which(terms$term==search[j])
    if (length(matches) > 0) {
      df[i,n] <- df[i,n] + terms$count[matches]
    }
    else {
      df[i,n] <- df[i,n]
    }
  }
}

write_csv(df, "data/output/newsearch_unigrams.csv")

# summary <- df %>%
#   group_by(yr) %>%
#   summarise_at(vars(conservation:Moore), sum, na.rm = TRUE)
