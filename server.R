# server.R
library("shiny")
library("lattice")
library("quadprog")

load("finaldata.RData")
total.aggregate<-read.csv(file="total_aggregate.csv",header=TRUE,strip.white=TRUE)

shinyServer(function(input, output) {
  getData=reactive({
        data<-subset(finaldata,country==input$country)
   #data$funded_date<-as.Date(data$funded_date)
    x<-unique(data$map.sector)
    
    #rbind to add values to the data frame
    #loop to find mean standard deviation loan value of sector
    
    data$fdate <- factor(format(data$funded_date,'%Y'))
    
    y<-as.vector(unique(total.aggregate$year))
    aggregate<-total.aggregate
    
    
    
    for(j in 1:length(y)){
      
      ag.new<-data[(data$map.sector=="Agriculture")&(data$fdate==y[j]),]
      aggregate$Agriculture[j]<-mean(ag.new$loan_amount)
      
      ed.new<-data[(data$map.sector=="Education")&(data$fdate==y[j]),]
      aggregate$Education[j]<-mean(ed.new$loan_amount)
      
      he.new<-data[(data$map.sector=="Health")&(data$fdate==y[j]),]
      aggregate$Health[j]<-mean(he.new$loan_amount)
      
      ind.new<-data[(data$map.sector=="Industry")&(data$fdate==y[j]),]
      aggregate$Industry[j]<-mean(ind.new$loan_amount)
      
      s.new<-data[(data$map.sector=="Services")&(data$fdate==y[j]),]
      aggregate$Services[j]<-mean(s.new$loan_amount)
    }
    aggregate[aggregate=="NaN"]=total.aggregate[aggregate=="NaN"]
    
    return(aggregate)
     })
#create and return a table of aggregate returns
getData2<- reactive({
  aggregate<-getData()
  aggregate.returns<-diff(log(aggregate$Agriculture))
  aggregate.returns<-data.frame(aggregate.returns)
  colnames(aggregate.returns)=c("Agriculture")
  aggregate.returns$Education<-diff(log(aggregate$Education))
  aggregate.returns$Health<-diff(log(aggregate$Health))
  aggregate.returns$Industry<-diff(log(aggregate$Industry))
  aggregate.returns$Services<-diff(log(aggregate$Services))
  aggregate.returns$aggregate.returns <- NULL
  return(aggregate.returns)
})
#create and returns covariance matrix
covmat<-reactive({
  aggregate.returns<-getData2()
  cov.mat=var(aggregate.returns)
  return(cov.mat)
})

meansvec<-reactive({
  aggregate.returns<-getData2()
  muhat.vals = apply(aggregate.returns[1:5],2,mean)
  return(muhat.vals)
})
#create and return table of weights both optimal and user entered 
 getData3<- reactive({
 tab<-getData4()
  tab$sector<-rownames(tab)
 tab<-rbind(tab,tab)
 tab$amount<-tab$weights*input$Funds
 tab$port<-c("optimal growth")
 tab$amount[6]<-input$Agg
 tab$amount[7]<-input$Edu
 tab$amount[8]<-input$Hea
 tab$amount[9]<-input$Ind
 tab$amount[10]<-input$Ser
 tab$port[6]<-c("selected portfolio")
 tab$port[7]<-c("selected portfolio")
 tab$port[8]<-c("selected portfolio")
 tab$port[9]<-c("selected portfolio")
 tab$port[10]<-c("selected portfolio")
 tab$weights[6:10]<-tab$amount[6:10]/sum(tab$amount[6:10])
  return(tab)
  })
 
#create and return optimal growth portfolio
 getData4<- reactive({
   
   cov.mat=covmat()
   muhat.vals = meansvec()
   
   # Optimal Growth Portfolio with no shorts Kiva
   mat<-as.matrix(cov.mat)
   x<-as.matrix(muhat.vals)
   n<-length(x)
   A<-cbind( matrix( rep(1,n), nr=n ), diag(n))
   b<-c(1, rep(0,n))
   r1 <- solve.QP(mat, x, A, b, meq=1)
   # Find Sdev for optimal growth point Kiva
   tab<- data.frame(r1$solution)
   rownames(tab)=c("agriculture","education","health","industry","services")
   colnames(tab)=c("weights")
   tab[tab<0]=0
   return(tab)
  
 })

defaultRate<-reactive({
  data1<-subset(finaldata,country==input$country)
  data<-subset(data1,gender==input$Gender)
  data2<-subset(data,map.sector==input$Sector)
  x<-data1[data1$status=="defaulted",]
  x<-length(x$status)/length(data1$status)
  y<-data2[data2$status=="defaulted",]
  y<-length(y$status)/length(data2$status)
  new<-data.frame(default.rate=c(x,y))
  rownames(new)=c("Country","Selection")
  colnames(new)=c("DefaultRate")
  return(new)
})
 #this is the plot of average loan size over time by country by sector
 output$sectorbycountryplot <- renderPlot({  
 aggregate<-getData()
    print({xyplot(Agriculture + Education+Health + Industry + Services ~ year, aggregate, 
       type = "o",pch=c(1,2,3,4,5), col = c("blue","pink","green","red","orange"),
       xlab="Year",ylab="Loan Amount",auto.key =list( x =.15, y=.85, 
                                                      corner = c(.5,.5),
                                                      col = c("blue","pink","green","red","orange"),
                                                      border = FALSE,lines=FALSE)) 


  })

})
#this is the optimal weights for a country
 output$weighttable <- renderTable({
   tab<-getData4()
   print(tab)

   }) 
#this is a bar chart for recomended investment amounts
 output$pie<- renderPlot({
   tab<-getData4()
   tab$amount<-tab$weights*input$Funds
   print(barplot(tab$amount, main="Recomended Investment Amounts", xlab="Sector",  
         ylab="Amount", names.arg=c("agriculture","education","health","industry","services"), 
         border="blue", density=c(10,20,30,40,50)))
   
 })
#this is a recommended dollar allocation given an available funds
output$amounttable <- renderTable({
  tab<-getData4()
  tab$weights<-tab$weights*input$Funds
  colnames(tab)<-c("Dollar Amount")
  print(tab)

}) 

#this is a comparison bar chart of entered amounts vs. optimal recomendations 
output$pie2<-renderPlot({
  tab<-getData3()

  print(barchart(weights~sector,groups=port , data=tab,ylim=c(0,1),auto.key = list(columns = 2)))
})
output$growthtable <- renderTable({
  tab<-getData3()
  cov.mat<-covmat()
  mu.hat<-meansvec()
  cov.mat<-as.matrix(cov.mat)
  mu.hat<-as.matrix(mu.hat)
  w.o<-as.vector(tab$weights[1:5])
  w.p<--as.vector(tab$weights[6:10])
  x<-t(mu.hat)%*%w.o-.5*(t(w.o)%*%cov.mat%*%w.o)
  y<-t(mu.hat)%*%w.p-.5*(t(w.p)%*%cov.mat%*%w.p)
  tab<- data.frame(c(x,y))
  rownames(tab)=c("Optimal Portfolio","Selected Portfolio")
  colnames(tab)=c("Growth Rate")
  print(tab)
  })

output$bar<- renderPlot({
new<-defaultRate()
  print(barplot(new$DefaultRate,ylab="Default Rate",col=c("darkblue","red"),
                names.arg=c("Country", "Selection"),
                legend = rownames(new)))
})

output$defaulttable <- renderTable({
  new<-defaultRate()
  print(new)
  
})
output$growthchart<-renderPlot({
  cov.mat<-covmat()
  mu.hat<-meansvec()
  tab<-getData4()
  cov.mat<-as.matrix(cov.mat)
  mu.hat<-as.matrix(mu.hat)
  w.o<-as.vector(tab$weights[1:5])
  w.a<--as.vector(c(1,0,0,0,0))
  w.e<--as.vector(c(0,1,0,0,0))
  w.h<--as.vector(c(0,0,1,0,0))
  w.i<--as.vector(c(0,0,0,1,0))
  w.s<--as.vector(c(0,0,0,0,1))
  a<-t(mu.hat)%*%w.o-.5*(t(w.o)%*%cov.mat%*%w.o)
  b<-t(mu.hat)%*%w.a-.5*(t(w.a)%*%cov.mat%*%w.a)
  c<-t(mu.hat)%*%w.e-.5*(t(w.e)%*%cov.mat%*%w.e)
  d<-t(mu.hat)%*%w.h-.5*(t(w.h)%*%cov.mat%*%w.h)
  e<-t(mu.hat)%*%w.i-.5*(t(w.i)%*%cov.mat%*%w.i)
  f<-t(mu.hat)%*%w.s-.5*(t(w.s)%*%cov.mat%*%w.s)
  
  tab<- data.frame(c(a,b,c,d,e,f))
  rownames(tab)=c("Optimal Portfolio","Agriculture","Education","Health","Industry","Services")
  colnames(tab)=c("GrowthRate")
  tab$risk<-tab$GrowthRate
  tab$risk[1]<-(t(w.o)%*%cov.mat%*%w.o)
  tab$risk[2]<-cov.mat[1,1]
  tab$risk[3]<-cov.mat[2,2]
  tab$risk[4]<-cov.mat[3,3]
  tab$risk[5]<-cov.mat[4,4]
  tab$risk[6]<-cov.mat[5,5]
  tab$risk<-sqrt(tab$risk)
  #plot(tab)
  names<-rownames(tab)
  print(xyplot(GrowthRate~risk,data=tab,groups=names,xlab="Risk (Standard Dev)",ylab="Growth Rate",
               type="p",panel=function(x,y,groups,subscripts,...){
    panel.text(x,y,groups[subscripts])})
    )
  
       
  })

})