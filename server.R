load("shinny.Rdata")
library(tidyverse)



cost <- data.frame(yr = c(2015,2016,2017,2018,2019),
                   cost = c(2.46,2.50,2.81,2.84,2.97))


#---calfeee------------
calfee <- function(yr,voln1=220,voln2=300,prbase =1.92 ,ratio1 = 1.72, ratio2 = 2.24) {
  pr2nd <- prbase*ratio1
  pr3rd <- prbase*ratio2
  fee = rep(0,length(yr))
  for (i in 1:length(yr)) {
    if (yr[i] <= voln1) {fee[i] = yr[i]*prbase} 
    else if (yr[i] >voln1 & yr[i] <=voln2) {fee[i] = voln1*prbase + (yr[i]-voln1)*pr2nd} 
    else {fee[i] = voln1*prbase +  (voln2-voln1)*pr2nd + (yr[i]-voln2)*pr3rd}
  }
  return(fee)
} 

#######


calcoverrate <- function(yrvector,voln1=220,voln2=300,prbase = 1.92,ratio1 = 1.72,ratio2 = 2.24) {
  # construnct split qujian 
  # according to Chengtou 0-500 by =20; 600 -1000, by =100
  
  # use -20,0 to calculate the zeros !! brilant idea- Xin Huang 2020.11.26
  m <- c(seq(-20,500,by=20),seq(600,1000,by=100),max(yrvector))
  
  # cal the sum of no. volume and fee 
  t <- data.frame(volume = yrvector,
                  label = cut(yrvector,m)) %>% 
    mutate(fee = calfee(volume,voln1,voln2,prbase,ratio1,ratio2)) %>% 
    group_by(label) %>% 
    summarise(n = n(),
              vol = sum(volume),
              fee = sum(fee))
  t_ratio <- t %>% mutate(n_ratio = round(n/sum(n)*100,1),
                          vol_ratio = round(vol/sum(vol)*100,1),
                          fee_ratio = round(fee/sum(fee)*100,1))
  
}



shinyServer <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    test <- tempyr %>% select(vari=input$vari) %>% unlist %>% as.numeric  %>%
      calcoverrate(.,
                   voln1=input$voln1,
                   voln2=input$voln2,
                   prbase=input$prbase,
                   ratio1=input$ratio1,
                   ratio2=input$ratio2)
    
    money <- test %>% select(-contains("ratio")) %>% 
      mutate(cost = cost$cost[3]*vol,
             del = fee - cost)
    round(sum(money$del)/(cost$cost[3]*sum(money$vol))*100,1)
 
    # money
    money  %>% select(label,fee,cost,del) %>% 
      gather(.,key = "feiyong", value = "yuan", -label) %>% 
      ggplot(aes(x=label, y=yuan,group = feiyong)) +
      geom_line(aes(color = feiyong,linetype = feiyong,size = I(3)))+
      scale_color_discrete(labels=c("Cost", "Income", "Delta"))+
      scale_linetype_discrete(labels=c("Cost", "Income", "Delta"))+
      theme(axis.text.x = element_text(size = I(12),angle = 270,hjust = 0),
            axis.text.y = element_text(size = I(12),
                                       face = "bold"),
            axis.title = element_text(size = I(16)),
            legend.position = c(0.8,0.8),
            legend.title = element_blank(),
            legend.text = element_text(size = I(14)),
            legend.background = element_blank(),
      )
  })
  
  output$percent <- renderTable({
    
    test <- tempyr %>% select(vari=input$vari) %>% unlist %>% as.numeric  %>%
      calcoverrate(.,
                   voln1=input$voln1,
                   voln2=input$voln2,
                   prbase=input$prbase,
                   ratio1=input$ratio1,
                   ratio2=input$ratio2)
    
    money <- test %>% select(-contains("ratio")) %>% 
      mutate(cost = cost$cost[3]*vol,
             del = fee - cost)
   percent<- round(sum(money$del)/(cost$cost[3]*sum(money$vol))*100,1)
   
  })
}
