library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "yellow",
  # Application title
  dashboardHeader(title = "Interactive Flow Chart"),

  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    radioButtons(
      "q1",
      h4("Were subjects receiving the intervention being studied and/or sampled in such a way that those on one side of the mean were overrepresented (e.g., only including subjects with BMIs above some level)?"),
      choices = c("Yes", "No")
    ),
    actionButton("gomap", "Submit Question 1"),
    uiOutput("ui1"),
    conditionalPanel(
      condition = "input.gomap != false",
      actionButton("gomap2", "Submit Question 2"),
      uiOutput("ui2"),
      conditionalPanel(
        condition = "input.gomap2 != false",
        actionButton("gomap3", "Submit Question 3"),
        uiOutput("ui3"),
        conditionalPanel(
          condition = "input.gomap3 != false",
          actionButton("gomap4", "Submit Question 4"),
          uiOutput("ui4"),
          conditionalPanel(
            condition = "input.gomap4 != false",
            actionButton("gomap5", "Submit Question 5"),
            uiOutput("ui5")
          )
        )
      )
    )
  )
)




# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # gomap = reactiveVal(FALSE)
  
  observeEvent(input$gomap, {
    if(input$q1=="Yes"){
      output$ui1 <- renderUI({
        radioButtons(
          "q2",
          label = h4("Was a control group (ideally randomized) included that was sampled in roughly the same way as the intervention group and had roughly the same baseline distribution of the outcome variables?" ),
          choices = c("Yes", "No")
        )
      })
    } else{
      output$ui1 <- renderUI({
        radioButtons(
          "q2",
          label = h4("Are analyses being conducted and/or conclusions being drawn on subsets of the intervention group defined by baseline distribution of the outcome variables? " ),
          choices = c("Yes", "No")
        )
      })
    }
  })
  
  observeEvent(input$gomap2, {
    if(input$q1=="Yes" & input$q2=="Yes"){
      output$ui2 <- renderUI({
        radioButtons(
          "q3",
          label = h4("Are conclusions being drawn as a function of a significance test of the differences in outcomes between the group receiving the intervention and the control group (or subsets thereof)? " ),
          choices = c("Yes", "No")
        )
      })
    } else if(input$q1=="Yes" & input$q2=="No") {
      output$ui2 <- renderUI({
        radioButtons(
          "q3",
          label = h4("Are conclusions being drawn by testing for the significance in change from baseline in the intervention group (or subsets of the intervention group)? " ),
          choices = c("Yes", "No")
        )
      })
    } else if(input$q1=="No" & input$q2=="Yes"){
      output$ui2 <- renderUI({
        radioButtons(
          "q3",
          label = h4("Was a control group (ideally randomized) included that was sampled in roughly the same way as the intervention group and had roughly the same baseline distribution of the outcome variables" ),
          choices = c("Yes", "No")
        )
      })
    } else if(input$q1=="No" & input$q2=="No"){
      output$ui2 <- renderUI({
        label = h4("RTM is not obviously a problem.", tags$a(href="www.rstudio.com", "Link to Flowchart"))
      })
    }
  })
  
  observeEvent(input$gomap3, {
    if(input$q1=="Yes" & input$q2=="Yes" & input$q3=="Yes"){
      output$ui3 <- renderUI({
        h4("RTM is not obviously a problem", tags$a(href="www.rstudio.com", "Link to Flowchart"))
      })
    } else if(input$q1=="Yes" & input$q2=="Yes" & input$q3=="No") {
      output$ui3 <- renderUI({
        radioButtons(
          "q4",
          label = h4("Are conclusions being drawn by conducting a significance test of change from baseline in the intervention group and a separate significance test of change from baseline in the control group (or subsets thereof) and declaring an effect if the result is significant in the intervention group, but not in the control group?" ),
          choices = c("Yes", "No")
        )
      })
    } else if(input$q1=="Yes" & input$q2=="No" & input$q3=="Yes") {
      output$ui3 <- renderUI({
        h4("RTM is a problem and could be creating spurious conclusions.", tags$a(href="www.rstudio.com", "Link to Flowchart"))
      })
    } else if(input$q1=="Yes" & input$q2=="No" & input$q3=="No") {
      output$ui3 <- renderUI({
        h4("Unclear", tags$a(href="www.rstudio.com", "Link to Flowchart"))
      })
    } else if(input$q1=="No" & input$q2=="Yes" & input$q3=="Yes") {
      output$ui3 <- renderUI({
        radioButtons(
          "q4",
          label = h4("Are conclusions being drawn as a function of a significance test of the differences in outcomes between the group receiving the intervention and the control group (or subsets thereof)?" ),
          choices = c("Yes", "No")
        )
      })
    } else if(input$q1=="No" & input$q2=="Yes" & input$q3=="No") {
      output$ui3 <- renderUI({
        radioButtons(
          "q4",
          label = h4("Are conclusions being drawn by testing for the significance in change from baseline in the intervention group (or subsets of the intervention group)? " ),
          choices = c("Yes", "No")
        )
      })
    }  
  })
  
  observeEvent(input$gomap4, {
    if(input$q1=="Yes" & input$q2=="Yes" & input$q3=="No" & input$q4=="Yes"){
      output$ui4 <- renderUI({
        h4("Authors are making the so-called DINS error (George et al., 2016*) which could be exacerbated by RTM.", tags$a(href="www.rstudio.com", "Link to Flowchart"))
      })
    } else if(input$q1=="Yes" & input$q2=="Yes" & input$q3=="No" & input$q4=="No"){
      output$ui4 <- renderUI({
        h4("Unclear", tags$a(href="www.rstudio.com", "Link to Flowchart"))
      })
    } else if(input$q1=="No" & input$q2=="Yes" & input$q3=="Yes" & input$q4=="Yes"){
      output$ui4 <- renderUI({
        h4("RTM is not obviously a problem.", tags$a(href="www.rstudio.com", "Link to Flowchart"))
      })
    } else if(input$q1=="No" & input$q2=="Yes" & input$q3=="Yes" & input$q4=="No"){
      output$ui4 <- renderUI({
        radioButtons(
          "q5",
          label = h4("Are conclusions being drawn by conducting a significance test of change from baseline in the intervention group and a separate significance test of change from baseline in the control group (or subsets thereof) and declaring an effect if the result is significant in the intervention group, but not in the control group?" ),
          choices = c("Yes", "No")
        )
      }) 
    } else if(input$q1=="No" & input$q2=="Yes" & input$q3=="No" & input$q4=="Yes"){
      output$ui4 <- renderUI({
        h4("RTM is a problem and could be creating spurious conclusions.", tags$a(href="www.rstudio.com", "Link to Flowchart"))
      })
    } else if(input$q1=="No" & input$q2=="Yes" & input$q3=="No" & input$q4=="No"){
      output$ui4 <- renderUI({
        h4("Unclear", tags$a(href="www.rstudio.com", "Link to Flowchart"))
      })
    } else if(input$q4=="Complete"){
      output$ui4 <- renderUI({
        label = h4("No Further Questions" )
      })
    }
  })
  
  observeEvent(input$gomap5, {
    if(input$q4=="Complete"){
      output$ui5 <- renderUI({
        label = h4("No Further Questions" )
      })
    } else if(input$q1=="No" & input$q2=="Yes" & input$q3=="Yes" & input$q4=="No" & input$q5=="Yes"){
      output$ui5 <- renderUI({
        label = h4("Authors are making the so-called DINS error (George et al., 2016*) which could be exacerbated by RTM.", tags$a(href="www.rstudio.com", "Link to Flowchart"))
      }) 
    } else if(input$q1=="No" & input$q2=="Yes" & input$q3=="Yes" & input$q4=="No" & input$q5=="No"){
      output$ui5 <- renderUI({
        label = h4("Unclear", tags$a(href="www.rstudio.com", "Link to Flowchart"))
      }) ## Here are all the null cases
    }   
  })  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

