library(shiny)

# Code borrowed from: https://stackoverflow.com/questions/31415301/shiny-responds-to-enter
# This generates "keyPressed" when <CR> is pressed
js <- '
$(document).on("keyup", function(e) {
  if(e.keyCode == 13){
    Shiny.onInputChange("keyPressed", Math.random());
  }
});
'

shinyUI(
  pageWithSidebar(
    headerPanel("I will Guess the Next Word ..."),
    sidebarPanel(
      h3("Text to be 'auto-completed':"),
      textInput("inputText", NULL,value="I think, therefore I ..."),
      # actionButton("keyPressed", "Submit"),
      # tags$script(js),
      hr(),
      tags$b("Instructions:"),
      p("Type the text to be auto-completed."),
      p("I will try to guess the next word, based on an 'n-gram frequency model' - how often combinations of words extracted from blogs, news and tweets occur ...")
    ),
    mainPanel(
      htmlOutput("returnWords")
      # leafletOutput("theMap"),
      # em("Click on the circles for details ..."),
      # hr(),
      # h3("This is what I found:"),
      # tags$b("From:"),
      # textOutput("foundFrom"),
      # textOutput("fromTime"),
      # tags$b("To:"),
      # textOutput("foundTo"),
      # textOutput("toTime"),
      # htmlOutput("timeDifferenceHTML"),
      # textOutput("distanceTraveled"),
      # hr(),
      # tags$footer("Timezone data provided by geonames.org"),
      # tags$footer("Location data provided by openstreetmap.org")
    )
  )
)