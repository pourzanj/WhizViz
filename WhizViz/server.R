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
    selectInput("xvarSelected", "X-axis variable", NumColNames)
  })
  
  output$yvar <- renderUI({
    #get DfName as string, use it to get actual Df, then
    #get the names of only numeric columns, because we can't
    #plot factors
    DfName <- parse(text = input$ytype)
    NumColNames <- eval(DfName) %>% purrr::keep(is.numeric) %>% colnames
    selectInput("yvarSelected", "Y-axis variable", NumColNames)
  })
  
  output$numMissing <- renderPrint({
    DfNameX <- parse(text = input$xtype)
    DfNameY <- parse(text = input$ytype)
    df <- full_join(eval(DfNameX), eval(DfNameY)) %>% left_join(General)
    print(paste("Total Obs.: ", nrow(df), " Missing Obs. ",
                sum(is.na(df[, input$xvarSelected]) | is.na(df[, input$yvarSelected]))))
  })
  
  # renderPlotly() also understands ggplot2 objects!
  output$plot <- renderPlotly({
    DfNameX <- parse(text = input$xtype)
    DfNameY <- parse(text = input$ytype)
    
    #joining the same df with itself changes the names which we don't want
    if(input$xtype == input$ytype) df <- General %>% left_join(eval(DfNameX), by = c("Code", "VisitNumber"))
    else df <- General %>% left_join(full_join(eval(DfNameX), eval(DfNameY), by = c("Code", "VisitNumber")), by = c("Code", "VisitNumber"), suffix = c("", ".y"))
    df <- df %>%
      filter(
        VisitNumber %in% input$showVisits | ifelse(is.na(VisitNumber), input$showVisitsNa, FALSE),
        EstimatedClassification_ %in% input$showDx | ifelse(is.na(EstimatedClassification_), input$showDxNa, FALSE),
        between(AbAbby, input$showAb[1], input$showAb[2]) | ifelse(is.na(AbAbby), input$showAbNa, FALSE),
        between(TauAbby, input$showTau[1], input$showTau[2]) | ifelse(is.na(TauAbby), input$showTauNa, FALSE),
        Sex_ %in% input$showSex | ifelse(is.na(Sex_), input$showSexNa, FALSE),
        between(Age_, input$showAge[1], input$showTau[2]) | ifelse(is.na(Age_), input$showAgeNa, FALSE),
        ifelse(is.na(Hypertension_), input$showHypertensionNa,
          ifelse(Hypertension_, "Yes" %in% input$showHypertension, "No" %in% input$showHypertension)
        ),
        BmiClassification %in% input$showBmi | ifelse(is.na(BmiClassification), input$showBmiNa, FALSE),
        Education_ %in% input$showEducation | ifelse(is.na(Education_), input$showEducationNa, FALSE),
        ApoE_ %in% input$showApoe | ifelse(is.na(ApoE_), input$showApoeNa, FALSE)
      ) %>%
      #refactor to take out empty factors once we've filtered
      mutate(VisitNumber = factor(VisitNumber)) %>%
      mutate(ClinicalDx = factor(ClinicalDx)) %>%
      mutate(Education_ = factor(Education_)) %>%
      mutate(ApoE_ = factor(ApoE_))
    
    # p <- ggplot(df, aes_string(x = input$xvarSelected, y = input$yvarSelected)) +
    #   geom_point(aes(text = paste("Code:", Code, "-", VisitNumber,
    #                               "</br>Classification:", `Clinical Dx`)))
    
    #logic to color and/or add shapes to points
    if(input$colorPoints) {
      if(input$shapePoints) p <- ggplot(df, aes_string(x = input$xvarSelected, y = input$yvarSelected, color = input$colorBy, shape = input$shapeBy))
      else p <- ggplot(df, aes_string(x = input$xvarSelected, y = input$yvarSelected, color = input$colorBy))
    }
    else { 
      if(input$shapePoints) p <- ggplot(df, aes_string(x = input$xvarSelected, y = input$yvarSelected, shape = input$shapeBy))
      else p <- ggplot(df, aes_string(x = input$xvarSelected, y = input$yvarSelected))
    }
    
    p <- p + geom_point(aes(text = paste("Code:", Code, "-", VisitNumber,
                                  "</br>Classification:", ClinicalDx,
                                  "</br>CSF AB Zlokovic:", CsfAb42_Zlokovic,
                                  "</br>CSF Tau Zlokovic:", CsfTau_Zlokovic,
                                  "</br>CSF AB Abby:", AbAbby,
                                  "</br>CSF Tau Abby:", TauAbby,
                                  "</br>Clinical Dx:", ClinicalDx,
                                  "</br>Sex:", Sex_,
                                  "</br>Age at Samples:", Age_,
                                  "</br>Education:", Education_,
                                  "</br>Bmi Classification:", BmiClassification,
                                  "</br>Hypertension:", Hypertension_,
                                  "</br>Traumatic Brain Injury:", TBI_,
                                  "</br>Other Medical Conditions:", Other_Medical_Conditions_ ,
                                  "</br>`Meds and Supplements`:", MedsAndSupplements,
                                  "</br>`STROOP INTERFERENCE RAW`:", StroopRaw,
                                  "</br>`STROOP INTERFERENCE Z-SCORE`:", StroopZ,
                                  "</br>ChangeInIntraCranialVolume:", ChangeInCranialVol,
                                  "</br>ChangeInGrayMatterVolume:", ChangeInGrayVol,
                                  "</br>HypertensionDx:", Hypertension_)))

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
