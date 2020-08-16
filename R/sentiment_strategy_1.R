# If a stock experienced a large change in price this week
# Did that stock see more news written about it in the lead up to the changes
# Also did the sentiment in the headlines leading up to the price change
# Predict the direction of the stock (Postive Sentiment = Increase in Price)

library(riingo)
library(sentiStock)
library(PerformanceAnalytics)
library(dplyr)
library(readr)
library(quantmod)
library(sentometrics)
library(purrr)

# Stock Screen on Tiingo (Sentometrics 1)
# 1 Week Return - +10-35%
# Price - $10-25 ( (Something you can afford)
# Volume - 100K to infinity - (Need to be able to trade in and out quick )
winners <- read_csv("Sentometrics_1_15082020.csv")

# Stock Screen on Tiingo (Sentometrics 2)
# 1 Week Return  -10-35%
# Price - $10-25 ( (Something you can afford)
# Volume - 100K to infinity - (Need to be able to trade in and out quick )
losers <- read_csv("Sentometrics_2_15082020.csv")

# Use the entire universe of NYSE Tickers
all_tickers <- supported_tickers()
nyse_only <- all_tickers %>% filter(exchange == "NYSE") %>% na.omit()
all_nyse_news <- do.call(rbind, lapply(nyse_only$ticker, get_all_news, 5))

# Get the last 3 months news on all of the names
winners_raw_news <- do.call(rbind, lapply(winners$Ticker, get_all_news, 90))
losers_raw_news <- do.call(rbind, lapply(losers$Ticker, get_all_news, 90))

# When did the major price change take place on these tickers?
# Get the data
allsymbols <- c(winners$Ticker, losers$Ticker)
prices_df <- riingo_prices(allsymbols, start_date = Sys.Date()-90, end_date = Sys.Date())
# Shrink the prices df down to what you need
prices_df <- prices_df %>%
  select(ticker, date, adjClose, volume)

# Bins the number of news articles on each ticker  by week
# Start with splitting the news dataframe up by ticker
winners_news_list <- split(winners_raw_news, winners_raw_news$ticker)

# Clean up the news so that you can get it into sento_corpus
clean_up_tiingo_news <- function(news){
  news %>%
  select(id, publishedDate, description) %>%
  rename(date = publishedDate, texts = description) %>%
  distinct()}
winners_cleaned_news <- lapply(winners_news_list, clean_up_tiingo_news)
# Turn it into a sentocorpus
winners_list_sento_corpus <- lapply(winners_cleaned_news, sento_corpus)
# Compute the text summaries
winners_corpus_summaries <- lapply(winners_list_sento_corpus, corpus_summarize, by = "week")
# Extract just the stats
winners_corpus_stats <- lapply(winners_corpus_summaries, function(x){x$stats})

# Calculate the percentage change in number of documents
winners_corpus_stats <- lapply(winners_corpus_stats,
                               function(x){x %>% mutate(pct_chg_docs = ROC(documents)) %>%
                                                 select(date, documents, meanTokens, pct_chg_docs)})

# Turn the stats into a large dataframe
corpus_stats_df <- bind_rows(winners_corpus_stats,.id = "ticker")

# Join the weekly price change data for each stock
# Document level Exploratory Data Analysis - Cleaned up Data Set
document_count_eda <- corpus_stats_df %>%
  left_join(prices_df, by = c("ticker", "date")) %>%
  group_by(ticker) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(pct_chg_price = (adjClose/lag(adjClose)-1)) %>%
  na.omit()
# Lets see as a whole with the winners can we predict price change with change in the
# Number of articles being written about it
model <- lm(pct_chg_price ~ pct_chg_docs, data = document_count_eda)
summary(model)


# Not really sure what we found here however time to move on to prediction with sentiment
library(ggplot2)
ggplot(document_count_eda,
       aes(x = log(adjClose),
           y = log(documents)))+
  geom_point()+
  geom_smooth(method = loess, method.args = list(family = "symmetric"))

# Replicate Predicting the VIX

library("quanteda")
library("stm")
library("lexicon")

# Fit a topic model
# You need to clean the data further to take out the ticker and few a other things
dfm_list <- lapply(winners_list_sento_corpus,
    function(x){dfm_func <- dfm(x, tolower = TRUE,
    remove_punct = TRUE, remove_numbers = TRUE, remove = stopwords("en")) %>%
    dfm_remove(min_nchar = 3)
    # dfm_trim(min_termfreq = 0.95, termfreq_type = "quantile") %>%
    # dfm_trim(max_docfreq = 0.10, docfreq_type = "prop")
    dfm_subset(dfm_func, ntoken(dfm_func) >0)})

topicModel_list <- lapply(dfm_list, function(x){stm::stm(x, K = 5, verbose = FALSE)})
top_terms <- lapply(topicModel_list, function(x){t(stm::labelTopics(x, n = 10)[["prob"]])})

keywords <- lapply(top_terms,
                   function(x){z <- lapply(1:ncol(x), function(i) x[, i])
                                names(z) <- paste0("TOPIC_", 1:length(z))
                                z})

winners_list_sento_corpus_newfeat <- mapply(function(x,y){add_features(x, keywords = y, do.binary = FALSE, do.regex = FALSE)},
                                            winners_list_sento_corpus, keywords)


# Prepare Sentiment Lexicons

lexiconsIn <- c(
  sentometrics::list_lexicons[c("LM_en", "HENRY_en", "GI_en")],
  list(
    NRC = lexicon::hash_sentiment_nrc,
    HULIU = lexicon::hash_sentiment_huliu,
    SENTIWORD = lexicon::hash_sentiment_sentiword,
    JOCKERS = lexicon::hash_sentiment_jockers,
    SENTICNET = lexicon::hash_sentiment_senticnet,
    SOCAL = lexicon::hash_sentiment_socal_google
  )
)
lex <- sento_lexicons(lexiconsIn = lexiconsIn,
                      valenceIn = sentometrics::list_valence_shifters[["en"]])

# Define the sentiment index aggregation specifications
ctrAggPred <- ctr_agg(
  howWithin = "proportionalPol",
  howDocs = "equal_weight",
  howTime = "beta", by = "day", fill = "latest", lag = 1, aBeta = 1:3, bBeta = 1:2
)

# Aggregate the corpus into textual sentiment time series
sentmeaspred <- lapply(winners_list_sento_corpus_newfeat,
                       sento_measures,
                       lexicons = lex, ctr = ctrAggPred)

# Get the price data from quantmod
data.env <- new.env()
getSymbols(winners$Ticker, env = data.env, src= "tiingo",  api = "181f4c56b03d7fa96e17339ab1d94ae27b4035a1",
           from = Sys.Date()-90, to = Sys.Date())

prices_df <- riingo_prices(winners$Ticker, start_date = Sys.Date()-90, end_date = Sys.Date())
prices_list <- split(prices_df, prices_df$ticker)

sentMeasIn <- mapply(function(x,y){subset(x, date %in% y$date)}, sentmeaspred, prices_list)
