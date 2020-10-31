library(shiny)
library(ggplot2)
# Define server logic required to generate and plot a random distribution

shinyServer(function(input, output) {
  
  Cleaned_data<-reactive({
    matrix(c(input$height, input$angle, input$orientation,input$kv, input$f, 
             input$discontinuity_density,input$PH, input$rainfall, input$temperature),nrow=1)
  })
  
  Evaluation_matrix<-reactive({
    weight_vector<-c(0.1526, 0.2245, 0.0680, 0.1236, 0.0680, 0.0374, 0.0344, 0.2709, 0.0206)   #正向指标在前，负向指标在后
    Standard_value<-matrix(c(15.000,32.50,75.00,150.00,
                             30.000,42.50,62.50, 80.00,
                             9.000,7.00,5.00,2.00,
                             0.875,0.65,0.45,0.35,
                             17.000,10.50,5.00,3.00,
                             15.000,7.50,4.00,3.00,
                             6.500,5.25,.75,3.00,
                             2050.000,1300.00,750.00,500.00,
                             16.000,18.00,22.00,29.50),nrow=9,byrow=T)
    s=Standard_value[3:8,1:4];s      #正向指标标准值
    t=Standard_value[c(1,2,9),1:4];t    #负向指标标准值
    
    #隶属函数求解隶属矩阵Cal_Membership_pos()====正向指标
    mem1=array(data = NA,dim=c(6,4,1))     #正向隶属矩阵6*(6*4)
    Cal_Membership_pos=function(x){
      for (i in 1:dim(x)[1]){
        for (j in 1:dim(x)[2]){
          for (k in 1:4){
            if (k==1){
              if (x[i,j]>=s[j,k]){
                mem1[j,k,i]<-1
              }
              if (x[i,j]>s[j,k+1] & x[i,j]<s[j,k]){
                mem1[j,k,i]<-(x[i,j]-s[j,k+1])/(s[j,k]-s[j,k+1])
              }
              if (x[i,j]<=s[j,k+1]){
                mem1[j,k,i]<-0
              }}
            if (k==2){
              if(x[i,j]<=s[j,k+1] | x[i,j]>=s[j,k-1]){
                mem1[j,k,i]<-0
              }
              if (x[i,j]>s[j,k] & x[i,j]<=s[j,k-1]){
                mem1[j,k,i]<-(s[j,k-1]-x[i,j])/(s[j,k-1]-s[j,k])
              }
              if (x[i,j]>s[j,k+1] & x[i,j]<=s[j,k]){
                mem1[j,k,i]<-(x[i,j]-s[j,k+1])/(s[j,k]-s[j,k+1])
              }}
            if (k==3){
              if(x[i,j]<=s[j,k+1] | x[i,j]>=s[j,k-1]){
                mem1[j,k,i]<-0
              }
              if (x[i,j]>s[j,k] & x[i,j]<=s[j,k-1]){
                mem1[j,k,i]<-(s[j,k-1]-x[i,j])/(s[j,k-1]-s[j,k])
              }
              if (x[i,j]>s[j,k+1] & x[i,j]<=s[j,k]){
                mem1[j,k,i]<-(x[i,j]-s[j,k+1])/(s[j,k]-s[j,k+1])
              }}
            if (k==4){
              if(x[i,j]>=s[j,k-1]){
                mem1[j,k,i]<-0
              }
              if (x[i,j]>s[j,k] & x[i,j]<=s[j,k-1]){
                mem1[j,k,i]<-(s[j,k-1]-x[i,j])/(s[j,k-1]-s[j,k])
              }
              if ( x[i,j]<=s[j,k]){
                mem1[j,k,i]<-1
              }}
          }
        }
      }
      return(mem1)
    }
    Membership_array_pos<-Cal_Membership_pos(matrix(Cleaned_data()[,3:8],nrow=1))  #正向指标求解隶属矩阵
    Membership_array_pos
    
    #隶属函数求解隶属矩阵Cal_Membership_neg()====负向指标
    mem2=array(data = NA,dim=c(3,4,1))     #负向隶属矩阵6*(3*4)
    Cal_Membership_neg=function(x){
      for (i in 1:dim(x)[1]){
        for (j in 1:dim(x)[2]){
          for (k in 1:4){
            if (k==1){
              if(x[i,j]>t[j,k+1]){
                mem2[j,k,i]<-0
              }
              if (x[i,j]>t[j,k] & x[i,j]<=t[j,k+1]){
                mem2[j,k,i]<-(t[j,k+1]-x[i,j])/(t[j,k+1]-t[j,k])
              }
              if ( x[i,j]<=t[j,k]){
                mem2[j,k,i]<-1
              }}
            if (k==2){
              if(x[i,j]>t[j,k+1] | x[i,j]<=t[j,k-1]){
                mem2[j,k,i]<-0
              }
              if (x[i,j]>t[j,k-1] & x[i,j]<=t[j,k]){
                mem2[j,k,i]<-(x[i,j]-t[j,k-1])/(t[j,k]-t[j,k-1])
              }
              if (x[i,j]>t[j,k] & x[i,j]<=t[j,k+1]){
                mem2[j,k,i]<-(t[j,k+1]-x[i,j])/(t[j,k+1]-t[j,k])
              }}
            if (k==3){
              if(x[i,j]>t[j,k+1] | x[i,j]<=t[j,k-1]){
                mem2[j,k,i]<-0
              }
              if (x[i,j]>t[j,k-1] & x[i,j]<=t[j,k]){
                mem2[j,k,i]<-(x[i,j]-t[j,k-1])/(t[j,k]-t[j,k-1])
              }
              if (x[i,j]>t[j,k] & x[i,j]<=t[j,k+1]){
                mem2[j,k,i]<-(t[j,k+1]-x[i,j])/(t[j,k+1]-t[j,k])
              }}
            if (k==4){
              if (x[i,j]<=t[j,k-1]){
                mem2[j,k,i]<-0
              }
              if (x[i,j]>t[j,k-1] & x[i,j]<=t[j,k]){
                mem2[j,k,i]<-(x[i,j]-t[j,k-1])/(t[j,k]-t[j,k-1])
              }
              if (x[i,j]>t[j,k]){
                mem2[j,k,i]<-1
              }}
          }
        }
      }
      return(mem2)
    }
    
    Membership_array_neg<-Cal_Membership_neg(matrix(Cleaned_data()[,c(1,2,9)],nrow=1))  #负向指标求解隶属矩阵
    Membership_array_neg
    
    #合并正向与负向隶属矩阵
    Membership_array<-rbind(Membership_array_pos[,,1],Membership_array_neg[,,1])
    
    #计算隶属度
    weight_vector %*% Membership_array
  })

  output$Degree_of_Membership<-renderPlot({
    
    Suitability_Rating<-c("A","B","C","D")
    data_bar<-data.frame(Suitability_Rating=c("A","B","C","D"),lishudu=Evaluation_matrix()[1,],note=round(Evaluation_matrix()[1,],2))
    data_bar$Suitability_Rating<-factor(data_bar$Suitability_Rating,levels=data_bar$Suitability_Rating)
    #Degree_of_Membership
    ggplot(data_bar,aes(x=Suitability_Rating,y=lishudu))+
      geom_bar(stat="identity",fill="dodgerblue3",width=0.2)+
      geom_text(data=data_bar,aes(label=note,x=Suitability_Rating,y=lishudu+0.02),size=4)+
      theme(text=element_text(size=14),
            axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
            axis.title.x=element_blank(),axis.title.y=element_blank())
  })
  
  output$eco_restoration <- renderText ({
    
    Grade<-apply( Evaluation_matrix(),1,which.max)
    Suitability_Rating<-c("A","B","C","D")
    suggestions<-c("AA",
                   "BB",
                   "CC",
                   "DD")
    eco_restoration<- paste("This slope ",Grade,"level,",Suitability_Rating[Grade],".",
                            "The suggestions eco_restoration is:",suggestions[Grade],".",sep="")
  }) 
})



  




