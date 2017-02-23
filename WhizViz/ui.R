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
                  c("Clinical" = "ClinicalWv", "CSF FFA" = "CsfFfaWv",
                    "CSF TFA" = "CsfTfaWv", "Neuro Psych" = "NeuroPsychWv",
                    "Urine DCA" = "UrineDcaWv", "Urine FFA" = "UrineFfaWv",
                    "Urine TFA" = "UrineTfaWv", "Volumetrics" = "Volumetrics")),
      
      uiOutput("xvar"),

      selectInput("ytype", "Y-axis Data Type",
                  c("Clinical" = "ClinicalWv", "CSF FFA" = "CsfFfaWv",
                    "CSF TFA" = "CsfTfaWv", "Neuro Psych" = "NeuroPsychWv",
                    "Urine DCA" = "UrineDcaWv", "Urine FFA" = "UrineFfaWv",
                    "Urine TFA" = "UrineTfaWv", "Volumetrics" = "Volumetrics")),
      
      uiOutput("yvar"),
      
      verbatimTextOutput("numMissing"),
      
      checkboxGroupInput("axesscale", "Axes Scale:", choices = c("X Log Scale" = "x",
                                                                "Y Log Scale" = "y")),
      
      checkboxGroupInput("regression", "Regression Fit:", choices = c("Linear" = "lin",
                                                                 "Smoother" = "smooth")),
      
      #########################
      h2("Filters"),
      selectizeInput('showVisits', 'Visits:', choices = levels(General$VisitNumber),
                     selected = levels(General$VisitNumber), multiple = TRUE),
      checkboxInput("showVisitsNa", "Include Missing", TRUE),
      
      selectizeInput('showDx', 'Classification:', choices = levels(General$EstimatedClassification_),
                     selected = levels(General$EstimatedClassification_), multiple = TRUE),
      checkboxInput("showDxNa", "Include Missing", TRUE),
      
      sliderInput("showAb", "CSF AB (Abby)", min(General$AbAbby, na.rm = TRUE),
                  max(General$AbAbby, na.rm = TRUE), value = c(min(General$AbAbby, na.rm = TRUE), 
                                                                                           max(General$AbAbby, na.rm = TRUE))),
      checkboxInput("showAbNa", "Include Missing", TRUE),
      
      sliderInput("showTau", "CSF Tau (Abby)", min(General$TauAbby, na.rm = TRUE),
                  max(General$TauAbby, na.rm = TRUE), value = c(min(General$TauAbby, na.rm = TRUE), 
                                                                                       max(General$TauAbby, na.rm = TRUE))),
      checkboxInput("showTauNa", "Include Missing", TRUE),
      
      selectizeInput('showSex', 'Sex:', choices = levels(General$Sex_),
                     selected = levels(General$Sex_), multiple = TRUE),
      checkboxInput("showSexNa", "Include Missing", TRUE),
      
      sliderInput("showAge", "Age", min(General$Age_, na.rm = TRUE),
                  max(General$Age_, na.rm = TRUE), value = c(min(General$Age_, na.rm = TRUE), 
                                                                                       max(General$Age_, na.rm = TRUE))),
      checkboxInput("showAgeNa", "Include Missing", TRUE),
      
      selectizeInput('showHypertension', 'Hypertension:', choices = c("Yes", "No"),
                     selected = c("Yes", "No"), multiple = TRUE),
      checkboxInput("showHypertensionNa", "Include Missing", TRUE),
      
      selectizeInput('showBmi', 'BMI:', choices = levels(General$BmiClassification),
                     selected = levels(General$BmiClassification), multiple = TRUE),
      checkboxInput("showBmiNa", "Include Missing", TRUE),
      
      selectizeInput('showEducation', 'Education:', choices = levels(General$Education_),
                     selected = levels(General$Education_), multiple = TRUE),
      checkboxInput("showEducationNa", "Include Missing", TRUE),
      
      selectizeInput('showApoe', 'ApoE:', choices = levels(General$ApoE_), selected = levels(General$ApoE_), multiple = TRUE),
      checkboxInput("showApoeNa", "Include Missing", TRUE),
      
      
      #########################
      h2("Color"),
      checkboxInput("colorPoints", "Color Points", value = FALSE),
      selectizeInput("colorBy", "Variable to Color By", c("VisitNumber", "EstimatedClassification_", "AgeLevels", "Sex_", "Education_", "ApoE_", "BmiClassification", "AbLevels", "TauLevels", "Hypertension_")),
      
      h2("Shape By"),
      checkboxInput("shapePoints", "Shape Points", value = FALSE),
      selectizeInput("shapeBy", "Variable to Color By", c("VisitNumber", "EstimatedClassification_", "AgeLevels", "Sex_", "Education_", "ApoE_", "BmiClassification", "AbLevels", "TauLevels", "Hypertension_")),
      
      h2("Facet By"),
      checkboxInput("facetX", "Facet", value = FALSE),
      selectizeInput("facetXBy", "Variable to Facet X By", c("VisitNumber", "EstimatedClassification_", "AgeLevels", "Sex_", "Education_", "ApoE_", "BmiClassification", "AbLevels", "TauLevels", "Hypertension_"))
      #checkboxInput("facetY", "Facet on Y-Axis", value = FALSE),
      #selectizeInput("facetYBy", "Variable to Facet Y By", c("VisitNumber", "`Clinical Dx`", "AgeLevels", "Sex", "Education", "ApoE", "BmiClassification", "AbLevels", "TauLevels", "HypertensionDx"))
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("plot", height = "800px")
    )
  )
))
