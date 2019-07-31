library(shiny)
library(ggplot2)
library(tree)
library(plotly)
library(gbm)


  #read the data
  cancerData<-read_csv("C:\\Users\\X-Gong\\Documents\\ST558\\Project3\\Five Year Cancer Survival Rates in USA.csv",col_types = cols(
    `Survival Rate` = col_double(),
    Year = col_double(),
    Race = col_factor(),
    Gender = col_factor(),
    `Cancer Type` = col_factor()
  ))

  #subset data for PCA
  
  #prepare the data 
  #cancerData$Race<-ifelse(cancerData$Race=="All races",1,ifelse((cancerData$Race=="Black"),2,3))
  #cancerData$Gender<-ifelse(cancerData$Gender=="Total",1,ifelse(cancerData$Gender=="males",2,3))
  
  # There are 17 cancer types in total and in the case, we just pick five most common cancer types: 
  #"Skin","Lung and bronchus","Prostate","Breast","Colon and rectum"
  subsetcancer<- cancerData %>% filter(`Cancer Type` %in% c("Skin","Lung and bronchus","Prostate","Breast","Colon and rectum"))
  subsetcancer$`Cancer Type`<- ifelse(subsetcancer$`Cancer Type`=="Skin",1,
                                      ifelse(subsetcancer$`Cancer Type`=="Lung and bronchus",2,
                                             ifelse(subsetcancer$`Cancer Type`=="Prostate",3,
                                                    ifelse(subsetcancer$`Cancer Type`=="Breast",4,5))))
  
shinyServer(function(input, output,session) {
  
  #filter missing values
  cancerData<- cancerData %>% filter(!is.na(`Survival Rate`))
  
  #numerical summaries
  output$num <- renderTable({
  
     if(input$Vars == "Cancer Type") {
        y<- aggregate(Survival.Rate ~ Cancer.Type,data =cancerData, FUN= mean)
        colnames(y)<-c("Cancer Type","Survival Rate")
        y}
        else{x<-aggregate(formula(paste0("Survival.Rate ~ ", input$Vars)), data =cancerData, FUN= mean)
            colnames(x)<-c(input$Vars,"Survival Rate")
             x}
  })
  
  #plotly function
  plotInput <- function(){
    
    if(input$Vars == "Cancer Type") {
      y<- aggregate(Survival.Rate ~ Cancer.Type,data =cancerData, FUN= mean)
      y }
    else{x<-aggregate(formula(paste0("Survival.Rate ~ ", input$Vars)), data =cancerData, FUN= mean)
    x}
    
    
    if (input$Vars == "Cancer Type"){
       p1 <-ggplot(y,aes(x=Cancer.Type,y=Survival.Rate))+
        geom_bar(stat = "identity",color="darkblue", fill="lightblue") +
         geom_hline(yintercept=.5, linetype="dashed", color = "red")+
          labs(x="Cancer Type",y ="Mean Survival Rate") +ylim(0,1)+
           theme(text = element_text(size=10),
            axis.text.x = element_text(angle=45, hjust=1)) 
       ggplotly(p1)
         
    }
      else { p2 <-ggplot(x,aes_string(x=input$Vars,y="Survival.Rate"))+
             geom_bar(stat = "identity",color="darkblue", fill="lightblue")+
              geom_hline(yintercept=.5, linetype="dashed", color = "red")+
              labs(y="Mean Survival Rate") +ylim(0,1)
               ggplotly(p2)} 
  } 
  
  #plotly 
   output$hist <- renderPlotly({
    print(plotInput ())
  })
 
  #plot function
   plotInput2 <- function(){
     
     if(input$Vars == "Cancer Type") {
       y<- aggregate(Survival.Rate ~ Cancer.Type,data =cancerData, FUN= mean)
       y }
     else{x<-aggregate(formula(paste0("Survival.Rate ~ ", input$Vars)), data =cancerData, FUN= mean)
     x}
     
     
     if (input$Vars == "Cancer Type"){
       ggplot(y,aes(x=Cancer.Type,y=Survival.Rate))+
         geom_bar(stat = "identity",color="darkblue", fill="lightblue") +
         geom_hline(yintercept=.5, linetype="dashed", color = "red")+
         labs(x="Cancer Type",y ="Mean Survival Rate") +ylim(0,1)+
         theme(text = element_text(size=10),
               axis.text.x = element_text(angle=45, hjust=1)) 
      
       
     }
     else {ggplot(x,aes_string(x=input$Vars,y="Survival.Rate"))+
       geom_bar(stat = "identity",color="darkblue", fill="lightblue")+
       geom_hline(yintercept=.5, linetype="dashed", color = "red")+
       labs(y="Mean Survival Rate") +ylim(0,1)
     } 
   } 

  
  output$downloadPlot <- downloadHandler(
    filename = "save.png",
    content = function(file) {
      ggsave(file, plot = plotInput2(), device = "png")
    }
  )
  
  output$downloadData <- downloadHandler(

    filename = "cancerData.csv",
    content = function(file) {
      write.csv(cancerData, file)
    }
  )
  
  output$download <- downloadHandler(
    
    filename = "cancerData.csv",
    content = function(file) {
      write.csv(cancerData, file)
    }
  )
  
  #biplots for PCA
  output$biplot <- renderPlot({
    
    PCs<-prcomp(select(subsetcancer,input$variables),scale. = TRUE,center = TRUE)
    biplot(PCs)
    
  })
  
  #new data
  names(cancerData)<- make.names(names(cancerData))
  getData <- reactive({
    
    newData <- cancerData[,c("Survival.Rate",input$treeVar)]
    
  })
  #random forest modeling
  output$rf <-renderPlot({
    
    #split into training and test
    
    train <- sample(1:nrow(cancerData), size = nrow(cancerData)*0.8)
    test <- dplyr::setdiff(1:nrow(cancerData), train)
    cancerDataTrain <- cancerData[train, ]
    cancerDataTest <- cancerData[test, ]
    
    newData<-getData()
    
    tree1<-tree(Survival.Rate~., data = newData)
    tree:::plot.tree(tree1)
    tree:::text.tree(tree1)
    
  })
  
  #boosting 
  output$bst <-renderText({
    
    #split into training and test
    newData<-getData()
    
    train <- sample(1:nrow(newData), size = nrow(newData)*0.8)
    test <- dplyr::setdiff(1:nrow(newData), train)
    newDataTrain <- newData[train, ]
    newDataTest <- newData[test, ]
    
    boostFit <- gbm(Survival.Rate ~ ., data = newDataTrain, distribution = "gaussian", n.trees = input$bst, 
                    shrinkage = 0.1, interaction.depth = 4)
    boostPred <- predict(boostFit, newdata = dplyr::select(newDataTest, -Survival.Rate), n.trees = input$bst)
    paste0("Root MSE values (root of test prediction error) of 
    boosting = ",sqrt(mean((boostPred-newData$Survival.Rate)^2))) 
    
  })
  
  
  #table for data(missing excluded)
  output$data <- renderDataTable({
    cancerData<- cancerData %>% filter(!is.na(Survival.Rate))
    colnames(cancerData)<- c("Survival Rate","Year","Race","Gender","Cancer Type")
    cancerData
  })

}
)
