library(shiny)

source("./Knesser-Ney.R", local=TRUE)
modelFile <- "nGramModel.RData"
if(file.exists(modelFile)) load(modelFile)

###################
shinyServer( function(input, output) {
        
        output$returnWords <- renderText({
                paste(
                        '<h2>','My guess is:','</h2>',
                        '<p style="text-align:center"><i>', input$inputText,'</i> <font size="+3" color="blue">',words()[1],'</font> </p>',
                        '<h3>', 'Other possibilities:', '</h3>',
                        '<p style="text-align:center"><i>', input$inputText,'</i> <font size="+1" color="red">',words()[2],'</font> </p>',
                        '<p style="text-align:center"><i>', input$inputText,'</i> <font size="+1" color="red">',words()[3],'</font> </p>',
                        '<p style="text-align:center"><i>', input$inputText,'</i> <font size="+1" color="red">',words()[4],'</font> </p>',
                        '<p style="text-align:center"><i>', input$inputText,'</i> <font size="+1" color="red">',words()[5],'</font> </p>'
                )
        })
        
        words <- reactive({
                runTheModel(input$inputText)
        })
        
        # words <- eventReactive(input$keyPressed,{
        #         runTheModel(input$inputText)
        # }, ignoreNULL = FALSE)
        # 
        # output$theMap <- renderLeaflet({
        #         mapObject()[["myMap"]]
        # })
        # 
        # output$foundFrom <- renderText({
        #         mapObject()[["mapData"]][1,"found"]
        # })
        # output$foundTo <- renderText({
        #         mapObject()[["mapData"]][2,"found"]
        # })
        # 
        # autoInvalidate <- reactiveTimer(1000)
        # 
        # currentTime <- reactive({
        #         autoInvalidate()
        #         now()
        # })
        # 
        # output$fromTime <- renderText({
        #         as.character(with_tz(currentTime(),mapObject()[["mapData"]][1,"TZone"]))
        # })
        # 
        # output$toTime <- renderText({
        #         as.character(with_tz(currentTime(),mapObject()[["mapData"]][2,"TZone"]))
        # })
        # 
        # output$timeDifferenceHTML <- renderText({
        #         paste("<b>Adjust your watch",abs(mapObject()[["mapData"]][2,"tDiff"]),ifelse(mapObject()[["mapData"]][2,"tDiff"]<0,"hours back","hours forward"),"at your destination</b>")
        # })
        # 
        # output$distanceTraveled <- renderText({
        #         paste("You will travel",round(mapObject()[["mapData"]][2,"distance"],1),"kilometers")
        # })
})