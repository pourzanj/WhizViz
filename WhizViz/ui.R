#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("WhizViz"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    sidebarPanel(
      selectInput("xtype", "X-axis Data Type",
                  c("Clinical" = "ClinicalWv", "Neuro Psych" = "NeuroPsychWv", "Volumetrics" = "VolumetricsWv",
                    "Urine FFA" = "UrineFfaWv", "Urine DCA" = "UrineDcaWv", "Urine TFA" = "UrineTfaWv",
                    "CSF FFA" = "CsfFfaWv", "CSF FFA (As Percent)" = "CsfFfaPercentWv",
                    "CSF SF" = "CsfSfWv", "CSF SF (As Percent)" = "CsfSfPercentWv",
                    "CSF NP" = "CsfNpWv", "CSF NP (As Percent)" = "CsfNpPercentWv")),
      
      uiOutput("xvar"),

      selectInput("ytype", "Y-axis Data Type",
                  c("Clinical" = "ClinicalWv", "Neuro Psych" = "NeuroPsychWv", "Volumetrics" = "VolumetricsWv",
                    "Urine FFA" = "UrineFfaWv", "Urine DCA" = "UrineDcaWv", "Urine TFA" = "UrineTfaWv",
                    "CSF FFA" = "CsfFfaWv", "CSF FFA (As Percent)" = "CsfFfaPercentWv",
                    "CSF SF" = "CsfSfWv", "CSF SF (As Percent)" = "CsfSfPercentWv",
                    "CSF NP" = "CsfNpWv", "CSF NP (As Percent)" = "CsfNpPercentWv")),
      
      uiOutput("yvar"),
      
      verbatimTextOutput("numMissing"),
      
      checkboxGroupInput("axesscale", "Axes Scale:", choices = c("X Log Scale" = "x",
                                                                "Y Log Scale" = "y")),
      
      checkboxGroupInput("regression", "Regression Fit:", choices = c("Linear" = "lin",
                                                                 "Smoother" = "smooth")),
      
      #########################
      h2("Filters"),
      selectizeInput('showVisits', 'Visits:', choices = levels(factor(MasterListWv$VisitNumber)),
                     selected = levels(factor(MasterListWv$VisitNumber)), multiple = TRUE),
      checkboxInput("showVisitsNa", "Include Missing", TRUE),

      selectizeInput('showDx', 'Classification:', choices = levels(MasterListWv$EstimatedClassification),
                     selected = levels(MasterListWv$EstimatedClassification), multiple = TRUE),
      checkboxInput("showDxNa", "Include Missing", TRUE),

      sliderInput("showAb", "CSF Estimated AB (Abby)", min(MasterListWv$EstimatedAbAbby, na.rm = TRUE),
                  max(MasterListWv$EstimatedAbAbby, na.rm = TRUE), value = c(min(MasterListWv$EstimatedAbAbby, na.rm = TRUE),
                                                                                           max(MasterListWv$EstimatedAbAbby, na.rm = TRUE))),
      #Note estimated CSF values should not be missing
      #checkboxInput("showAbNa", "Include Missing", TRUE),

      sliderInput("showTau", "CSF Estimated Tau (Abby)", min(MasterListWv$EstimatedTauAbby, na.rm = TRUE),
                  max(MasterListWv$EstimatedTauAbby, na.rm = TRUE), value = c(min(MasterListWv$EstimatedTauAbby, na.rm = TRUE),
                                                                                       max(MasterListWv$EstimatedTauAbby, na.rm = TRUE))),
      #checkboxInput("showTauNa", "Include Missing", TRUE),

      selectizeInput('showSex', 'Sex:', choices = levels(MasterListWv$Sex),
                     selected = levels(MasterListWv$Sex), multiple = TRUE),
      checkboxInput("showSexNa", "Include Missing", TRUE),

      sliderInput("showAge", "Age", min(MasterListWv$Age.at.Sample, na.rm = TRUE),
                  max(MasterListWv$Age.at.Sample, na.rm = TRUE), value = c(min(MasterListWv$Age.at.Sample, na.rm = TRUE),
                                                                                       max(MasterListWv$Age.at.Sample, na.rm = TRUE))),
      checkboxInput("showAgeNa", "Include Missing", TRUE),

      selectizeInput('showHypertension', 'Hypertension:', choices = c("Yes", "No"),
                     selected = c("Yes", "No"), multiple = TRUE),
      checkboxInput("showHypertensionNa", "Include Missing", TRUE),

      selectizeInput('showBmi', 'BMI:', choices = levels(MasterListWv$BmiClassification),
                     selected = levels(MasterListWv$BmiClassification), multiple = TRUE),
      checkboxInput("showBmiNa", "Include Missing", TRUE),

      selectizeInput('showEducation', 'Education:', choices = levels(MasterListWv$Education),
                     selected = levels(MasterListWv$Education), multiple = TRUE),
      checkboxInput("showEducationNa", "Include Missing", TRUE),

      selectizeInput('showApoe', 'ApoE:', choices = levels(MasterListWv$ApoE), selected = levels(MasterListWv$ApoE), multiple = TRUE),
      checkboxInput("showApoeNa", "Include Missing", TRUE),
      
      
      #########################
      h2("Color"),
      checkboxInput("colorPoints", "Color Points", value = FALSE),
      selectizeInput("colorBy", "Variable to Color By", MasterListFactorNames),
      
      h2("Shape By"),
      checkboxInput("shapePoints", "Shape Points", value = FALSE),
      selectizeInput("shapeBy", "Variable to Color By", MasterListFactorNames),
      
      h2("Facet By"),
      checkboxInput("facetX", "Facet", value = FALSE),
      selectizeInput("facetXBy", "Variable to Facet X By", MasterListFactorNames)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("plot", height = "800px")
    )
  )
))
