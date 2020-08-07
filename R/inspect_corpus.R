tslnews <- sentiStock::get_all_news("TSLA", 90)
library(magrittr)

clean_and_create_corpus <- function(data, text_type){
  news_from_source <- data %>%
    dplyr::select(id, publishedDate, paste0(text_type)) %>%
    dplyr::rename(date = publishedDate, texts = description)
    news_sento <- sentometrics::sento_corpus(news_from_source)
}

tsla <- clean_and_create_corpus(tslnews, "description")

# Fit the topipc Model

dfm <- dfm(tsla, tolower = TRUE,
               remove_punct = TRUE, remove_numbers = TRUE, remove = stopwords("en")) %>%
      dfm_remove(min_nchar = 3) %>%
      # Trim a dfm using frequency threshold-based feature selection
      dfm_trim(min_termfreq = 0.95, termfreq_type = "quantile") %>%
      dfm_trim(max_docfreq = 0.10, docfreq_type = "prop")
dfm <- dfm_subset(dfm, ntoken(dfm) > 0)

# Extract 5 Topics
topicModel <- stm::stm(dfm, K = , verbose = FALSE)
topTerms <- t(stm::labelTopics(topicModel, n = 30)[["prob"]])

# Take a look at the top 5 terms
topTerms[, 1:5]

    # corpus_summarize(tsla)
# lexicons <- sento_lexicons(list_lexicons[c("GI_en", "LM_en", "HENRY_en")])
# compute_sentiment(tsla, lexicons, )
#
# ?ctr_agg()
# # use_package("sentometrics")
# # library(lexicon)
# #
# # tsla <- get_all_news("TSLA", 90)
# # library(sentometrics)
# # library(dplyr)
#
# # corpus <- sento_corpus(tsla_sento_corpus_headlines)
# #
# #
# # s <- compute_sentiment(corpus, lexicons, how = "counts")
# #
# # ctr <- ctr_agg(howDocs = "proportional", howTime = "equal_weight", by = "day", lag = 1)
# # measures <- aggregate(s, ctr)
# #
# # plot(measures)
# #
# #
# # ctr <- ctr_agg(howWithin = "counts",
# #                howDocs = "proportional",
# #                howTime = c("linear", "equal_weight"), by = "day", lag = 2)
# #
# #
# # # Get the stock price and compare it now
# # library(quantmod)
# #
# # library(riingo)
# # riingo::riingo_get_token()
# #
# # getSymbols("TSLA", src = "tiingo", from = "2020-04-25", api = riingo::riingo_get_token())
# #
# # library(TTR)
# #
# # TSLA_ROC <- ROC(TSLA)
# # measures_ROC <- ROC(measuresGlobal)
# # measures_ROC <- data.frame(date = index(measures_ROC), coredata(measures_ROC))
# # measures <- sento_measures(corpus, lexicons, ctr)
# #
# # measuresGlobal <- aggregate(measures, do.global = TRUE)
# #
# # TSLA_df <- data.frame(date = index(TSLA_ROC), coredata(TSLA))
# #
# #
# # measuresGlobal_2 <- TSLA_ROC %>%
# #   left_join(measuresGlobal,by = c("date")) %>%
# #   select(date, TSLA.Open, TSLA.Volume,global)
# #
# # library("data.table")
# # library(ggplot2)
# #
# # ggplot(melt(measuresGlobal_2, id.vars = "date")) +
# #   aes(x = date, y = value, color = variable) +
# #   geom_line() +
# #   scale_x_date(name = "Date", date_labels = "%m-%Y") +
# #   scale_y_continuous(name = "Sentiment") +
# #   theme_bw() +
# #   sentometrics:::plot_theme(legendPos = "top")
# #
# #
# # ggplot()
