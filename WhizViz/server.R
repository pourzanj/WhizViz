#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(purrr)
library(ggplot2)
library(plotly)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$xvar <- renderUI({
    #get DfName as string, use it to get actual Df, then
    #get the names of only numeric columns, because we can't
    #plot factors
    DfName <- parse(text = input$xtype)
    NumColNames <- eval(DfName) %>% purrr::keep(is.numeric) %>% colnames
    
    #remember we have to add back in column names that were removed
    #and added to the MasterList (see ImportCsv.R)
    if(input$xtype == "ClinicalWv") NumColNames <- c(MasterColumnsClinicalNum, NumColNames)
    if(input$xtype == "NeuroPsychWv") NumColNames <- c(MasterColumnsNeuroPsychNum, NumColNames)

    selectInput("xvarSelected", "X-axis variable", NumColNames)
  })
  
  output$yvar <- renderUI({
    #get DfName as string, use it to get actual Df, then
    #get the names of only numeric columns, because we can't
    #plot factors
    DfName <- parse(text = input$ytype)
    NumColNames <- eval(DfName) %>% purrr::keep(is.numeric) %>% colnames
    
    #remember we have to add back in column names that were removed
    #and added to the MasterList (see ImportCsv.R)
    if(input$ytype == "ClinicalWv") NumColNames <- c(MasterColumnsClinicalNum, NumColNames)
    if(input$ytype == "NeuroPsychWv") NumColNames <- c(MasterColumnsNeuroPsychNum, NumColNames)
    
    selectInput("yvarSelected", "Y-axis variable", NumColNames)
  })
  
  output$numMissing <- renderPrint({
    DfNameX <- parse(text = input$xtype)
    DfNameY <- parse(text = input$ytype)
    df <- full_join(eval(DfNameX), eval(DfNameY)) %>% left_join(MasterListWv)
    print(paste("Total Obs.: ", nrow(df), " Missing Obs. ",
                sum(is.na(df[, input$xvarSelected]) | is.na(df[, input$yvarSelected]))))
  })
  
  # renderPlotly() also understands ggplot2 objects!
  output$plot <- renderPlotly({
    DfNameX <- parse(text = input$xtype)
    DfNameY <- parse(text = input$ytype)
    
    #joining join the selected data frames with the MasterList. Only need to join the Y
    #data frame if it's different than the X
    df <- MasterListWv %>% left_join(eval(DfNameX), by = c("Code", "VisitNumber"))
    if(input$xtype != input$ytype) df <- MasterListWv %>% left_join(eval(DfNameY), by = c("Code", "VisitNumber"))
    
    #to apply VisitNumber filter we have to convert VisitNumber from int to factor
    df <- df %>% mutate(VisitNumber = factor(VisitNumber))
    
    #apply selected filters
    df <- df %>%
      filter(
        VisitNumber %in% input$showVisits | ifelse(is.na(VisitNumber), input$showVisitsNa, FALSE),
        EstimatedClassification %in% input$showDx | ifelse(is.na(EstimatedClassification), input$showDxNa, FALSE),
        between(EstimatedAbAbby, input$showAb[1], input$showAb[2]) | ifelse(is.na(EstimatedAbAbby), input$showAbNa, FALSE),
        between(EstimatedTauAbby, input$showTau[1], input$showTau[2]) | ifelse(is.na(EstimatedTauAbby), input$showTauNa, FALSE),
        Sex %in% input$showSex | ifelse(is.na(Sex), input$showSexNa, FALSE),
        between(Age.at.Sample, input$showAge[1], input$showAge[2]) | ifelse(is.na(Age.at.Sample), input$showAgeNa, FALSE),
        ifelse(is.na(Hypertension), input$showHypertensionNa,
          ifelse(Hypertension, "Yes" %in% input$showHypertension, "No" %in% input$showHypertension)
        ),
        BmiClassification %in% input$showBmi | ifelse(is.na(BmiClassification), input$showBmiNa, FALSE),
        Education %in% input$showEducation | ifelse(is.na(Education), input$showEducationNa, FALSE),
        ApoE %in% input$showApoe | ifelse(is.na(ApoE), input$showApoeNa, FALSE)
      ) %>%
      #refactor to take out empty factors once we've filtered
      mutate(VisitNumber = factor(VisitNumber)) %>%
      mutate(EstimatedClassification = factor(EstimatedClassification)) %>%
      mutate(Education = factor(Education)) %>%
      mutate(ApoE = factor(ApoE))
    
    #logic to color and/or add shapes to points
    if(input$colorPoints) {
      if(input$shapePoints) p <- ggplot(df, aes_string(x = input$xvarSelected, y = input$yvarSelected, color = input$colorBy, shape = input$shapeBy))
      else p <- ggplot(df, aes_string(x = input$xvarSelected, y = input$yvarSelected, color = input$colorBy))
    }
    else { 
      if(input$shapePoints) p <- ggplot(df, aes_string(x = input$xvarSelected, y = input$yvarSelected, shape = input$shapeBy))
      else p <- ggplot(df, aes_string(x = input$xvarSelected, y = input$yvarSelected))
    }
    
    #geom point with text will allow us to hover over patient to get MasterList info
    p <- p + geom_point(aes(text = paste("Code:", Code, "-", VisitNumber,
                                  "</br>Estimated Classification:", EstimatedClassification,
                                  "</br>CSF Estimated AB Abby:", EstimatedAbAbby,
                                  "</br>CSF Estimated Tau Abby:", EstimatedTauAbby,
                                  "</br>Sex:", Sex,
                                  "</br>Age at Samples:", Age.at.Sample,
                                  "</br>Education:", Education,
                                  "</br>Bmi Classification:", BmiClassification,
                                  "</br>Hypertension:", Hypertension,
                                  "</br>Traumatic Brain Injury:", Traumatic.Brain.Injury,
                                  "</br>Other Medical Conditions:", Other.Medical.Conditions,
                                  "</br>`Meds and Supplements`:", "Meds.and.Supplements",
                                  "</br>`STROOP INTERFERENCE RAW`:", STROOP.INTERFERENCE.RAW,
                                  "</br>`STROOP INTERFERENCE Z-SCORE`:", STROOP.INTERFERENCE.Z.SCORE)))

    if("x" %in% input$axesscale) p <- p + scale_x_log10()
    if("y" %in% input$axesscale) p <- p + scale_y_log10()
    
    if("lin" %in% input$regression) p <- p + geom_smooth(method = "lm")
    if("smooth" %in% input$regression) p <- p + geom_smooth()
    
    if(input$facetX) {
     p <- p + facet_wrap(c(input$facetXBy))
    }
    
    ggplotly(p)
  })
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })
  
})
