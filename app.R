## app.R ##
library(shinydashboard)
library(shiny)
library(DT)
library(readxl)
library(visNetwork)
library(xlsx)
library(timevis)
library(networkD3)

nodes <- read_excel("./data/Nodes.xlsx")
edges <- read_excel("./data/Edges.xlsx")
Extremist_List <- read_excel("./data/Extremist_List.xlsx", sheet= 'Sheet1')
Extremist <- read_excel("./data/Network_Database.xlsx", sheet= 'Extremist_Profile')
Event <- read_excel("./data/Network_Database.xlsx", sheet= 'Violent_Event_Profile')
Organization <- read_excel("./data/Network_Database.xlsx", sheet= 'Organizational_Profile')
or<-data.frame(Organization)
ex<-data.frame(Extremist[,1:40])
ev<-data.frame(Event)
#Data : Organizational Timeline
templateWC <-function(stage, team1) {
  if(stage=="Neo JMB"){
    sprintf(
      '<table style="background-color: #7af76a;"><tbody>
      <tr><td colspan="2" style ="color:white;" ><b>%s</b></td></tr>
      <tr>
      <td>%s</td>
      </tr>
      <tr>
      </tbody></table>',
      stage, team1
    )
  }
  
  
  else{
    sprintf(
      '<table style="background-color: #5ef2f2;"><tbody>
      <tr><td colspan="2" style="color:white;" ><b>%s</b></td></tr>
      <tr>
      <td>%s</td>
      </tr>
      <tr>
      </tbody></table>',
      stage, team1
    )
  }
} 

OrgTL <- read_excel("./data/OrgTimeline.xlsx")
data_OrgTL <-data.frame(
  id=OrgTL$ID,
  content =c(
    templateWC("Neo JMB","Gulsan Holey Artisan attack"),
    templateWC("Neo JMB","Sholakia Eid-gadh bomb blast"),
    templateWC("Neo JMB","Japanese citizen Kunio Hoshi murder"),
    templateWC("Neo JMB","Hindu priest Jagneshwar Roy murder"),
    templateWC("Neo JMB","RU Prof. Rezaul Karim Siddiquee murder"),
    templateWC("Neo JMB","Homeopathy Dr. Sanwar Hossain murder"),
    templateWC("Neo JMB","Hindu Ashram inmate Nityaranjan Pandey murder"),
    templateWC("Neo JMB","Pir/Sufi Mohammad Shahidullah murder"),
    templateWC("Neo JMB","Retired Sergeant Rustom Hawlader murder"),
    templateWC("Neo JMB","Businessman Debesh Chandra Pramanik murder"),
    templateWC("Neo JMB","Veteran Christian businessman murder"),
    templateWC("Neo JMB","Hindhu Priest Ananada Gopal Ganguly murder"),
    templateWC("Neo JMB","Hindu college teacher Ripon Chakraborty murder"),
    templateWC("Neo JMB","Hindu temple worker Shyamananda Das murder"),
    templateWC("ABT","Blogger Asif Mohiuddin's attempted murder"),
    templateWC("ABT","Blogger Ahmed Rajib Haider's murder"),
    templateWC("ABT","Ashulia bank robbery"),
    templateWC("ABT","Blogger Avijit Roy's murder"),
    templateWC("ABT","Blogger Oyasiqur Rahman Babu's murder"),
    templateWC("ABT","Blogger Ananta Bijoy Das's murder"),
    templateWC("ABT","RU Prof. AKM Shafiul Islam's murder")
  ),
  start=OrgTL$Date
  )


ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Data for Peace"),
  
  ## Sidebar content
  dashboardSidebar(
      sidebarMenu(
      menuItem(HTML("<h5><font color ='white'><b>Extremist Profiles</b></font></h5>"), tabName = "extremist"),
      menuItem(HTML("<h5><font color ='white'><b>Violent Event Profiles</b></font></h5>"), tabName = "incident"),
      menuItem(HTML("<h5><font color ='white'><b>Organaizational Profiles</b></font></h5>"), tabName = "organaization"),
      menuItem(HTML("<h5><font color ='white'><b>Network Graph</b></font></h5>"), tabName = "net"),
      menuItem(HTML("<h5><font color ='white'><b>Organizational Timeline</b></font></h5>"), tabName = "OrgTL")
    )
  ),
  #Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "extremist",
              HTML("<h1><font color ='black'><center><b>Analysis: Extremist Profiles</b> </center></font></h1>"),
              fluidRow(
                column(1),
                column(10,
                         tabsetPanel(
                         tabPanel("Tab1", DT::dataTableOutput('tbl_a')),
                         tabPanel("Tab2", sankeyNetworkOutput("plot"))
                       )
                       
                       ),
                column(1)
                
              )
      ),
      
      # Second tab content
      tabItem(tabName = "incident",
              HTML("<h1><font color ='black'><center><b>Violent Event Profiles</b> </center></font></h1>"),
              fluidRow(
                column(1),
                column(10,
                       tabsetPanel(
                              tabPanel("Tab1", DT::dataTableOutput('tbl_b')),
                              tabPanel("Tab2", "Tab content 2")
                       )
                       
                ),
                column(1)
              )
      ),
      # Third tab content
      tabItem(tabName = "organaization",
              HTML("<h1><font color ='black'><center><b>Organaizational Profiles</b> </center></font></h1>"),
              fluidRow(
                column(1),
                column(10,
                       tabsetPanel(
                              tabPanel("Tab1", DT::dataTableOutput('tbl_c')),
                              tabPanel("Tab2", "Tab content 2")
                       )
                       
                ),
                column(1)
                
              )
      ),
      #Tab : Network graph
      tabItem(tabName = "net",
              HTML("<h1><font color ='black'><center><b>Events, Extremists & Outfits : Network Analysis</b> </center></font></h1>"),
              
              fluidRow(
                
                column(1),
                column(10,
                       visNetworkOutput("network")),
                column(1)
                      
                )
      ),
      #Tab : Orgnanizational Timeline
      tabItem(tabName = "OrgTL",
              HTML("<h1><font color ='black'><center><b>Organizational Timeline</b></center></font></h1>"),
              
              fluidRow(
                
                column(1),
                column(10, 
                       timevisOutput("mytime"),
                       actionButton("btn", "Reset")),
                column(1)
                      
                )
      
      
      
      
      
      
    )
  )
)
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)

  output$tbl_c = DT::renderDT(or,extensions=c('Buttons','Responsive'),
                                options = list(
                                dom = 'Bfrtip',
                                buttons = list('copy', 'print', list(
                                  extend = 'collection',
                                  buttons = c('csv', 'excel', 'pdf'),
                                  text = 'Download'
                                )),
          
                                searchHighlight = TRUE,
                                pageLength=1,
                                scrollX= TRUE)
                              )
                              
                              
  output$tbl_a = DT::renderDT(ex,
                              extensions=c('Buttons','Responsive'),
                              options = list(
                                
                                autoWidth = TRUE,
                                columnDefs = list(list(className = 'dt-center',targets=c(1:7)),list(width ='250px',targets = list(1,7))),
                                dom = 'Bfrtip',
                                buttons = list('copy', 'print', list(
                                  extend = 'collection',
                                  buttons = c('csv', 'excel', 'pdf'),
                                  text = 'Download'
                                )),
                                
                                searchHighlight = TRUE,
                                pageLength=5,
                                scrollX= TRUE)
                              )
  output$tbl_b = DT::renderDT({datatable(ev,
                                         extensions=c('Buttons','Responsive'),
                                         options = list(
                                           dom = 'Bfrtip',
                                           buttons = list('copy', 'print', list(
                                             extend = 'collection',
                                             buttons = c('csv', 'excel', 'pdf'),
                                             text = 'Download'
                                           )),
                                           
                                           searchHighlight = TRUE,
                                           pageLength=4,
                                           scrollX= TRUE)
                                         ) %>% formatDate('Date',method = "toDateString")})
  output$downloadExt <- downloadHandler(
    filename = function(){
      paste("Extremist Profile","csv",sep = ".")
    },
    content = function(file) {
      write.csv(ex,file)
    }
  )
  
  output$downloadEve <- downloadHandler(
    filename = function(){
      paste("Event Profile","csv",sep = ".")
    },
    content = function(file) {
      write.csv(ev,file)
    }
  )
  
  output$downloadOrg <- downloadHandler(
    filename = function(){
      paste("Organaizational Profile","csv",sep = ".")
    },
    content = function(file) {
      write.csv(or,file)
    }
  )
  
  
  output$network <- renderVisNetwork({
    nodes$label <- nodes$type.label
    nodes$shadow <- TRUE
    nodes$title <-paste0("<p><b>",nodes$media,"</b><br>",nodes$Status,"<br>",nodes$Status1,"<br>",nodes$Status2,"<br>",nodes$Status3,"<br>",nodes$Status4,"</p>")
    nodes$borderWidth <- 2 
    nodes$color.background <- c("slategrey", "tomato", "red")[nodes$media.type]
    nodes$color.border <- "black"
    nodes$color.highlight.background <- "orange"
    nodes$color.highlight.border <- "darkred"
    visNetwork(nodes, edges ,width="100%", height="500px")%>% visOptions(highlightNearest = list(enabled = TRUE, degree = 3),selectedBy = "type.label")%>%visInteraction(navigationButtons = TRUE,zoomView = TRUE)
  })
  
  #Backend code: Extremist Organizational Timeline
  
  output$mytime <- renderTimevis(timevis(data_OrgTL,zoomFactor = 0.01,width = "1300px", height = "500px",fit = TRUE,showZoom = TRUE))
  observeEvent(input$btn, {
    addItem("mytime", list(id = "item1", content = "M", start = "2016")) %>%
      centerItem("item1")
  })
  
  
  #Backend Code Start: Extremist Sankey Relational Chart
  
  
 node = data.frame("name" = 
                       c("Educational Background", # Node 0
                         "Bangla Medium", # Node 1
                         "English Medium", # Node 2
                         "Madrasha Medium",
                         "Introvert",
                         "Extrovert",
                         "Shirt,Pants",
                         "Panjabi, Pajama",
                         "Bearded face",
                         "Clean Shaven face"))# Node 3
  link = as.data.frame(matrix(c(
    0, 1, 10, # Each row represents a link. The first number
    0, 2, 20, # represents the node being conntected from. 
    0, 3, 30, # the second number represents the node connected to.
    1, 4, 10,
    4, 6, 14,
    4, 7, 10,
    1, 5, 20,
    5, 6, 12,
    2, 4, 15,
    2, 5, 17,
    3, 4, 6,
    3, 5, 13,
    7, 8, 15,
    6, 8, 3,
    6, 9, 5
    
  ),# The third number is the value of the node
  byrow = TRUE, ncol = 3))
  names(link) = c("source", "target", "value")
  output$plot <- renderSankeyNetwork({
  sankeyNetwork(Links = link, Nodes = node,
                Source = "source", Target = "target",
                Value = "value", NodeID = "name",
                fontSize= 12, nodeWidth = 30)
  })
  
  #Backend Code End: Extremist Sankey Relational Chart
}

shinyApp(ui, server)