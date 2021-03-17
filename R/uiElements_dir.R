#'dirPanel
#'@export
#'@rdname dir_panel
dir_setter_panel <- function(){
  tabPanel("Select Working Directory",
           tags$br(),
     fluidRow(column(10, offset = 1,
                     tags$b(tags$h3("Welcome to our app!")),
                     tags$p("
                            In this app we aim to enable you to analyse tweets and see if the information
                            contained within them can be used to explain and predict stock movements. "),
                     tags$b(tags$h4("Tweets")),
                        tags$p("    For this we scraped Twitter for a period from
                            2018-11-30 until yesterday (the app is daily updated). The tweets were searched on a daily basis, meaning we
                            made sure to have a roughly equivalent number of tweets per day. Our app focuses on the main stock indices in DAX
                            and the Dow Jones Industrial (DJI) and its components. In order to analyse the effect of the tweets we scraped
                            tweets using no search term as well as using company specific search terms. For most companies, the search terms
                            included their name as well as various twitter handles. For companies such as Apple or Caterpillar only twitter
                            handles were used in order to avoid scraping wrong tweets. For the company tweets we set the upper limit of tweets
                            to receive to 5000 and for the unfiltered searches to roughly 100,000 per day. All searches were performed for English
                            as well as German tweets."),
                    tags$b(tags$h4("Sentiment")),
                         tags$p("   In order to estimate the effects of tweets on stocks we computed the sentiment of each tweet
                            and aggregated it on a daily level by taking the mean. As we expect that the length, number of retweets and number
                            of likes of a tweets may influence the importance of a sentiment we also compute weighted means. The sentiment
                            ranges from values from -1 to 1. "),
                    tags$b(tags$h4("App structure")),
                         "
                                In the first tab 'Twitter' you can get an overview of the tweets. In 'Descriptives'
                            you can plot time series for the sentiment but also other metrics like the number of retweets.
                            You can also get an idea of the distribution of specific metrics over a selected period. \n
                            Within the “Descriptive” tab you can also analyse word and bigram frequencies to get an idea of
                            what people are tweeting. In the tab “Going deeper” we allow you to take a look at the raw tweets.
                            Here you can also plot network graphs of the tweets. Note that this computation is relatively expensive
                            and can thus only be performed for very short periods. However, it offers you the opportunity to dig deeper
                            into what people have been tweeting on a given day.",
                            tags$br(),
                            "In the comparison tab you can get a first visual idea of the connection between tweets and stocks.
                            Here you can visualize the time series
                            of all stocks within the DAX and DJI together with twitter metrics such as the sentiment for the same period.",
                     tags$br(),
                            "In the model tab we offer you various models to analyse the effects of sentiment on stock returns. We first start
                            with Granger Causality and basic regression analysis but also offer forecasting methods such as Vector Autoregressions
                            (VAR) and XGBoost. For a better description of the models go to the models tab. \
                            We now ask you to select the folder where you store the data that should be analysed.",





           )),
           tags$br(),
           tags$hr(),
           fluidRow(column(4,

                           tags$p(),
                           tags$p("Please choose the directory containing the folder containig \n
               the data called 'Data'."),
               shinyFiles::shinyDirButton("directory", "Select folder", "Please select a folder"
               ),
               ## in case dir path chooser not working enter manually
               tags$br(),
               tags$br(),
               tags$p("In the case that choosing a path through the 'Select Folder' button \n
             is not possible we recommentd updating RStudio. If that does not help you can
             also enter your path manually"),
             textInput("dir_path_man", ""),
             actionButton("dir_path_man_btn", "Set path"),
             tags$head(tags$style("#path_checker_man{color: red;

                                 }"
             )
             ),
             textOutput("path_checker_man")

           ),
           column(8,
                  tags$p("Welcom to our shiny app. Please select the folder containing the \n
                         data in order to continue."),
                  tags$h4("Selected folder"),

             textOutput("directorypath"),
             tags$hr())
           ),


           fluidRow(column(12, align = "center",
                           tags$hr(),
                           tags$p(),
                           imageOutput("twitter_logo")
           )
           )

  )

}
