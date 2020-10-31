library(shiny)
shinyUI(pageWithSidebar(
  # Application title
  headerPanel(h1("露天岩质边坡生态修复适宜性评级系统", align="center",
                 style ="font-family: 'times'; font-size: 24pt ")),
  
  sidebarPanel(h1("岩质边坡生态修复适宜性评价指标", align="center",
                  style ="font-family: 'times'; font-size: 18pt "),
    numericInput(inputId = "height",label="边坡高度/m",value=95),
               numericInput(inputId = "angle",label="边坡角度/°",value=45),
               numericInput(inputId = "orientation",label="边坡朝向",value=8),
               numericInput(inputId = "kv",label="岩体完整性系数",value=0.75),
               numericInput(inputId = "f",label="岩石坚固性系数",value=8),
               numericInput(inputId = "discontinuity_density",label="结构面密度/条/m",value=2),
               numericInput(inputId = "PH",label="土壤PH值",value=6.5),
               numericInput(inputId = "rainfall",label="年平均降雨量/mm",value=1250),
               numericInput(inputId = "temperature",label="年平均气温/℃",value=25)),
  
  mainPanel( h2("边坡生态修复方式等级评判", align="center",
                style ="font-family: 'times'; font-size: 18pt "),
             plotOutput("Degree_of_Membership"),
             h2("边坡生态修复方式推荐", align="center",
                style ="font-family: 'times'; font-size: 18pt "),
             textOutput("eco_restoration"))
))



