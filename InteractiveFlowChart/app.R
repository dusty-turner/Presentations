library(shiny)
library(shinydashboard)
library(magick)

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "yellow",
                     
  # Application title
  dashboardHeader(title = "Does my study have a Regression to the Mean effect?", titleWidth = 600),
    dashboardSidebar( 
    tags$style(HTML("
      .main-sidebar{
        width: 20%; height: 100%; overflow-y: auto;
      }
    ")),
        tags$head(tags$style(
          type="text/css",
          "#image img {max-width: 100%; width: 100%; height: auto}"
        )),
    radioButtons(
      "q1",
      h4("Were subjects receiving the intervention being studied and/or sampled in such a way that those on one side of the mean were overrepresented (e.g., only including subjects with BMIs above some level)?"),
      choices = c("Yes", "No")
    ),
    actionButton("gomap", "Submit1"),
    uiOutput("ui1"),
    conditionalPanel(
      # condition = "output.factorflag == true",
      condition = "input.gomap != false",
      actionButton("gomap2", "Submit"),
      uiOutput("ui2"),
      conditionalPanel(
        condition = "output.factorflag == true",
        # condition = "input.gomap2 != false",
        actionButton("gomap3", "Submit"),
        uiOutput("ui3"),
        conditionalPanel(
          condition = "output.factorflag1 == true",
          # condition = "input.gomap3 != false",
          actionButton("gomap4", "Submit"),
          uiOutput("ui4"),
          conditionalPanel(
            condition = "output.factorflag2 == true",
            # condition = "input.gomap4 != false",
            actionButton("gomap5", "Submit"),
            uiOutput("ui5")
          )
        )
      )
    )
  ),
  # dashboardBody( 
  # dashboardBody(style = "overflow-y: auto;", 
  dashboardBody(style = "overflow: scroll;",
  # dashboardBody(style = "max-height: 2000vh; overflow-y: auto;",
  # dashboardBody(style = "max-height: 100vh;",

    
    tags$head(tags$style(HTML(' .main-sidebar{ width: 20%; } .content-wrapper, .main-footer, .right-side { margin-left: 20%; } '))),
    # tags$head(tags$style(HTML(' .main-sidebar{ width: 300px; } .content-wrapper, .main-footer, .right-side { margin-left: 300px; } '))),
    imageOutput("img1"),
    downloadButton("downloadImage", "Download"),
    
    tags$div(class = "footer", checked = NA,
             tags$p("This app was designed by Dusty Turner at the United States Military Academy"),
             tags$a(href = "https://twitter.com/DTDusty", "@dtdusty")
    )
    # tags$footer("This app was designed by Dusty Turner at the United States Military Academy")
    
    
  )
)




# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # gomap = reactiveVal(FALSE)
  
  imageLoc <- reactiveVal("https://raw.githubusercontent.com/dusty-turner/Presentations/master/Flow%20Chart.png")
  imageVal <- reactive({
    image_convert(image_read(imageLoc()), "jpeg")
  })
  
  updatedImageLoc <- reactive({
    ## retrieve the imageVal
    image <- imageVal()
    # image <- output$img1
    tmpfile <- image %>%
    image_write(tempfile(fileext='jpg'), format = 'jpg')
    tmpfile
  })
  
  output$downloadImage <- downloadHandler(
    filename = "Modified_image.jpeg",
    contentType = "image/jpeg",
    content = function(file) {
      ## copy the file from the updated image location to the final download location
      file.copy(updatedImageLoc(), file)
    })
  
    
  output$img1 = renderImage({   #This is where the image is set 
    list(src = "Flow Chart.png")# height = 240, width = 300)
  }, deleteFile = FALSE)
  
  observeEvent(input$gomap, {
    if(input$q1=="Yes"){
      output$ui1 <- renderUI({
        radioButtons(
          "q2",
          label = h4("Was a control group (ideally randomized) included that was sampled in roughly the same way as the intervention group and had roughly the same baseline distribution of the outcome variables?" ),
          choices = c("Yes", "No")
        )
      })
      output$img1 = renderImage({   #This is where the image is set 
        list(src = "Right Yes.png")# height = 240, width = 300)
      }, deleteFile = FALSE)
    } else{
      output$ui1 <- renderUI({
        radioButtons(
          "q2",
          label = h4("Are analyses being conducted and/or conclusions being drawn on subsets of the intervention group defined by baseline distribution of the outcome variables? " ),
          choices = c("Yes", "No")
        )
        
      })
      output$img1 = renderImage({   #This is where the image is set 
        list(src = "Left No.png")# height = 240, width = 300)
      }, deleteFile = FALSE)
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
      output$factorflag <- reactive(TRUE)
      outputOptions(output, "factorflag", suspendWhenHidden = FALSE)
      output$img1 = renderImage({   #This is where the image is set 
        list(src = "Right Yes Yes.png")# height = 240, width = 300)
      }, deleteFile = FALSE)
    } else if(input$q1=="Yes" & input$q2=="No") {
      output$ui2 <- renderUI({
        radioButtons(
          "q3",
          label = h4("Are conclusions being drawn by testing for the significance in change from baseline in the intervention group (or subsets of the intervention group)? " ),
          choices = c("Yes", "No")
        )
      })
      output$factorflag <- reactive(TRUE)
      outputOptions(output, "factorflag", suspendWhenHidden = FALSE)
      output$img1 = renderImage({   #This is where the image is set 
        list(src = "Right Yes No.png")# height = 240, width = 300)
      }, deleteFile = FALSE)
    } else if(input$q1=="No" & input$q2=="Yes"){
      output$ui2 <- renderUI({
        radioButtons(
          "q3",
          label = h4("Was a control group (ideally randomized) included that was sampled in roughly the same way as the intervention group and had roughly the same baseline distribution of the outcome variables" ),
          choices = c("Yes", "No")
        )
      })
      output$factorflag <- reactive(TRUE)
      outputOptions(output, "factorflag", suspendWhenHidden = FALSE)
      output$img1 = renderImage({   #This is where the image is set 
        list(src = "Left No Yes.png")# height = 240, width = 300)
      }, deleteFile = FALSE)
    } else if(input$q1=="No" & input$q2=="No"){
      output$ui2 <- renderUI({
        label = h4("RTM is not obviously a problem.")
      })
      output$img1 = renderImage({   #This is where the image is set 
        list(src = "Left No No.png")# height = 240, width = 300)
      }, deleteFile = FALSE)
      output$factorflag <- reactive(FALSE)
      outputOptions(output, "factorflag", suspendWhenHidden = FALSE)
    }
  })
  
  observeEvent(input$gomap3, {
    if(input$q1=="Yes" & input$q2=="Yes" & input$q3=="Yes"){
      output$ui3 <- renderUI({
        h4("RTM is not obviously a problem")
      })
      output$factorflag1 <- reactive(FALSE)
      outputOptions(output, "factorflag1", suspendWhenHidden = FALSE)
      output$img1 = renderImage({   #This is where the image is set 
        list(src = "Right Yes Yes Yes.png")# height = 240, width = 300)
      }, deleteFile = FALSE)
    } else if(input$q1=="Yes" & input$q2=="Yes" & input$q3=="No") {
      output$ui3 <- renderUI({
        radioButtons(
          "q4",
          label = h4("Are conclusions being drawn by conducting a significance test of change from baseline in the intervention group and a separate significance test of change from baseline in the control group (or subsets thereof) and declaring an effect if the result is significant in the intervention group, but not in the control group?" ),
          choices = c("Yes", "No")
        )
      })
      output$factorflag1 <- reactive(TRUE)
      outputOptions(output, "factorflag1", suspendWhenHidden = FALSE)
      output$img1 = renderImage({   #This is where the image is set 
        list(src = "Right Yes Yes No.png")# height = 240, width = 300)
      }, deleteFile = FALSE)
    } else if(input$q1=="Yes" & input$q2=="No" & input$q3=="Yes") {
      output$ui3 <- renderUI({
        h4("RTM is a problem and could be creating spurious conclusions.")
      })
      output$factorflag1 <- reactive(FALSE)
      outputOptions(output, "factorflag1", suspendWhenHidden = FALSE)
      output$img1 = renderImage({   #This is where the image is set 
        list(src = "Right Yes No Yes.png")# height = 240, width = 300)
      }, deleteFile = FALSE)
    } else if(input$q1=="Yes" & input$q2=="No" & input$q3=="No") {
      output$ui3 <- renderUI({
        h4("Unclear")
      })
      output$factorflag1 <- reactive(FALSE)
      outputOptions(output, "factorflag1", suspendWhenHidden = FALSE)
      output$img1 = renderImage({   #This is where the image is set 
        list(src = "Right Yes No No.png")# height = 240, width = 300)
      }, deleteFile = FALSE)
    } else if(input$q1=="No" & input$q2=="Yes" & input$q3=="Yes") {
      output$ui3 <- renderUI({
        radioButtons(
          "q4",
          label = h4("Are conclusions being drawn as a function of a significance test of the differences in outcomes between the group receiving the intervention and the control group (or subsets thereof)?" ),
          choices = c("Yes", "No")
        )
      })
      output$factorflag1 <- reactive(TRUE)
      outputOptions(output, "factorflag1", suspendWhenHidden = FALSE)
      output$img1 = renderImage({   #This is where the image is set 
        list(src = "Left No Yes Yes.png")# height = 240, width = 300)
      }, deleteFile = FALSE)
    } else if(input$q1=="No" & input$q2=="Yes" & input$q3=="No") {
      output$ui3 <- renderUI({
        radioButtons(
          "q4",
          label = h4("Are conclusions being drawn by testing for the significance in change from baseline in the intervention group (or subsets of the intervention group)? " ),
          choices = c("Yes", "No")
        )
      })
      output$factorflag1 <- reactive(TRUE)
      outputOptions(output, "factorflag1", suspendWhenHidden = FALSE)
      output$img1 = renderImage({   #This is where the image is set 
        list(src = "Left No Yes No.png")# height = 240, width = 300)
      }, deleteFile = FALSE)
    }  
  })
  
  observeEvent(input$gomap4, {
    if(input$q1=="Yes" & input$q2=="Yes" & input$q3=="No" & input$q4=="Yes"){
      output$ui4 <- renderUI({
        h4("Authors are making the so-called DINS error (George et al., 2016*) which could be exacerbated by RTM.")
      })
      output$factorflag2 <- reactive(FALSE)
      outputOptions(output, "factorflag2", suspendWhenHidden = FALSE)
      output$img1 = renderImage({   #This is where the image is set 
        list(src = "Right Yes Yes No Yes.png")# height = 240, width = 300)
      }, deleteFile = FALSE)
    } else if(input$q1=="Yes" & input$q2=="Yes" & input$q3=="No" & input$q4=="No"){
      output$ui4 <- renderUI({
        h4("Unclear")
      })
      output$factorflag2 <- reactive(FALSE)
      outputOptions(output, "factorflag2", suspendWhenHidden = FALSE)
      output$img1 = renderImage({   #This is where the image is set 
        list(src = "Right Yes Yes No No.png")# height = 240, width = 300)
      }, deleteFile = FALSE)
    } else if(input$q1=="No" & input$q2=="Yes" & input$q3=="Yes" & input$q4=="Yes"){
      output$ui4 <- renderUI({
        h4("RTM is not obviously a problem.")
      })
      output$factorflag2 <- reactive(FALSE)
      outputOptions(output, "factorflag2", suspendWhenHidden = FALSE)
      output$img1 = renderImage({   #This is where the image is set 
        list(src = "Left No Yes Yes Yes.png")# height = 240, width = 300)
      }, deleteFile = FALSE)
    } else if(input$q1=="No" & input$q2=="Yes" & input$q3=="Yes" & input$q4=="No"){
      output$ui4 <- renderUI({
        radioButtons(
          "q5",
          label = h4("Are conclusions being drawn by conducting a significance test of change from baseline in the intervention group and a separate significance test of change from baseline in the control group (or subsets thereof) and declaring an effect if the result is significant in the intervention group, but not in the control group?" ),
          choices = c("Yes", "No")
        )
      }) 
      output$factorflag2 <- reactive(TRUE)
      outputOptions(output, "factorflag2", suspendWhenHidden = TRUE)
      output$img1 = renderImage({   #This is where the image is set 
        list(src = "Left No Yes Yes No.png")# height = 240, width = 300)
      }, deleteFile = FALSE)
    } else if(input$q1=="No" & input$q2=="Yes" & input$q3=="No" & input$q4=="Yes"){
      output$ui4 <- renderUI({
        h4("RTM is a problem and could be creating spurious conclusions.")
      })
      output$img1 = renderImage({   #This is where the image is set 
        list(src = "Left No Yes No Yes.png")# height = 240, width = 300)
      }, deleteFile = FALSE)
    } else if(input$q1=="No" & input$q2=="Yes" & input$q3=="No" & input$q4=="No"){
      output$ui4 <- renderUI({
        h4("Unclear")
      })
      output$factorflag2 <- reactive(FALSE)
      outputOptions(output, "factorflag2", suspendWhenHidden = FALSE)
      output$img1 = renderImage({   #This is where the image is set 
        list(src = "Left No Yes No No.png")# height = 240, width = 300)
      }, deleteFile = FALSE)
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
        label = h4("Authors are making the so-called DINS error (George et al., 2016*) which could be exacerbated by RTM.")
      }) 
      output$img1 = renderImage({   #This is where the image is set 
        list(src = "Left No Yes Yes No Yes.png")# height = 240, width = 300)
      }, deleteFile = FALSE)
    } else if(input$q1=="No" & input$q2=="Yes" & input$q3=="Yes" & input$q4=="No" & input$q5=="No"){
      output$ui5 <- renderUI({
        label = h4("Unclear")
      }) ## Here are all the null cases
      output$img1 = renderImage({   #This is where the image is set 
        list(src = "Left No Yes Yes No No.png")# height = 240, width = 300)
      }, deleteFile = FALSE)
    }   
  })  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

