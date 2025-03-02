#' Function to gather industry specific sentiments

#' @export
#' @rdname industry_sentiment

get_industry_sentiment <- function(de,industry,retweets_min,tweet_length){

  components_de <- de %>%  filter(Symbol == "ADS.DE"|Symbol == "ALV.DE" |
                                               Symbol == "DBK.DE" | Symbol == "DHER.DE")
  components_de <- components_de %>% filter(sector == industry)
  symbols <- c(components_de[["Symbol"]])
  df_total = data.frame()
  for (s in symbols) {

    load_data <- eval(parse(text = paste(s,'()', sep='')))

    if(tweet_length == "yes"){
    load_data <- load_data %>% filter(retweets_count > as.numeric(retweets_min))

    }else{
      load_data <- load_data %>% filter((retweets_count > as.numeric(retweets_min))&
                                        (tweet_length > 81))
    }
    senti_stock <- aggregate_sentiment(load_data)

    df_total <- rbind(df_total,senti_stock)
  }
  filtered_df <- df_total %>% group_by(date,language) %>%
    summarise_at(vars("sentiment_weight_retweet", "sentiment_weight_likes",
                      "sentiment_weight_length","sentiment_mean","tweets_used_daily"), mean)
}


#' @export
#' @rdname industry_sentiment

get_industry_sentiment_nofiltering <- function(de,industry){

  components_de <- de %>%  filter(Symbol == "ADS.DE"|Symbol == "ALV.DE" |
                                    Symbol == "DBK.DE" | Symbol == "DHER.DE")
  components_de <- components_de %>% filter(sector == industry)
  symbols <- c(components_de[["Symbol"]])
  df_total = data.frame()
  for (s in symbols) {

    load_data <- eval(parse(text = paste(s,'()', sep='')))
    senti_stock <- aggregate_sentiment(load_data)

    df_total <- rbind(df_total,senti_stock)
  }
  filtered_df <- df_total %>% group_by(date,language) %>%
    summarise_at(vars("sentiment_weight_retweet", "sentiment_weight_likes",
                      "sentiment_weight_length","sentiment_mean"), mean)
}
