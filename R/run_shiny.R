##########################################################################################
# COVID-19 TIME SERIES
##########################################################################################
#' @title COVID-19 time series
#' @param country_region country
#' @keywords covid
#' @export
#' @examples
#' covid(country_region="Germany")
#' covid(country_region="Greece")
covid<-function(country_region="Greece"){
  if (!require("shiny")) install.packages("shiny")
  if (!require("plotly")) install.packages("plotly")
  if (!require("shinydashboard")) install.packages("shinydashboard")
  if (!require("shinyWidgets")) install.packages("shinyWidgets")
  if (!require("DT")) install.packages("DT")
  if (!require("drc")) install.packages("drc")
  if (!require("reshape2")) install.packages("reshape2")
  options(scipen=999)
  
  # https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series
  
  confirmed<-read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",stringsAsFactors=FALSE,check.names=FALSE)
  deaths<-read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",stringsAsFactors=FALSE,check.names=FALSE)
  
  covid_c<-melt(confirmed,id.vars=c("Province/State","Country/Region","Lat","Long"),variable.name="Date",value.name="Confirmed")
  covid_d<-melt(deaths,id.vars=c("Province/State","Country/Region","Lat","Long"),variable.name="Date",value.name="Deaths")
  covid_all<-merge(covid_c,covid_d,all=TRUE,by=c("Province/State","Country/Region","Date","Lat","Long"))
  covid_all<-transform(covid_all,Days=as.numeric(strftime(as.Date(covid_all$Date,format='%m/%d/%Y'),'%j')))
  
  shinyApp(ui=tagList(tags$head(tags$script('
                        var dimension=[0,0];
                        $(document).on("shiny:connected",function(e) {
                        dimension[0]=window.innerWidth;
                        dimension[1]=window.innerHeight;
                        Shiny.onInputChange("dimension",dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0]=window.innerWidth;
                        dimension[1]=window.innerHeight;
                        Shiny.onInputChange("dimension",dimension);
                        });
                        ')),
                      fluidPage(dropdownButton(selectInput(inputId="country",
                                                           label="Country",
                                                           choices=sort(unique(covid_all$Country.Region)),
                                                           selected=c(country_region),
                                                           selectize=FALSE,
                                                           size=20,
                                                           width="100%"),
                                               sliderInput(inputId="ahead","Days Ahead:",
                                                           min=80,max=365,
                                                           value=200,
                                                           width="100%"),
                                               circle=FALSE,status="info",icon=icon("info"),width="500px",inline=TRUE,
                                               tooltip=tooltipOptions(title="Click!")),
                                plotlyOutput("plot_country",width="100%"))),
           server=function(input,output) {
             output$plot_country<-renderPlotly({
               temp_country<-plyr::ddply(covid_all[covid_all$Country.Region %in% input$country,],c("Date","Days","Country.Region"),plyr::numcolwise(sum,na.rm=TRUE))
               newdata=data.frame(Days=1:input$ahead)
               model_gompertz<-drm(Confirmed~Days,data=temp_country,fct=G.3())
               model_3pl_log<-drm(Confirmed~Days,data=temp_country,fct=LL.3())
               prediction<-data.frame(Days=newdata$Days,
                                      confirmed_gompertz=round(predict(model_gompertz,newdata=newdata,interval="confidence"),0))
               predicted<-merge(temp_country,prediction,all=TRUE,by=c("Days"))
               predicted$Date=seq(as.Date("1/1/2020",format='%m/%d/%Y'),by=1,length.out=input$ahead)
               plot_ly(predicted,
                       x=~Days,
                       y=~Confirmed,
                       text=~paste0("\nConfirmed infections=",predicted$Confirmed,
                                    "\nPredicted infections=",predicted$confirmed_gompertz.Prediction,
                                    "\nDeaths=",predicted$Deaths,
                                    "\nCountry=",input$country,
                                    "\nDate=",predicted$Date),
                       name='Infections',
                       mode="markers",
                       type="scatter",
                       width=(as.numeric(input$dimension[1])-30),
                       height=(as.numeric(input$dimension[2])-50)) %>% 
                 add_trace(y=~predicted$Deaths,name='Deaths',mode='markers') %>%
                 add_trace(y=~predicted$confirmed_gompertz.Prediction,name='Gompertz',mode='lines') %>%
                 layout(title=list(text=""),
                        margin=list(l=50,r=50,b=50,t=50,pad=0),
                        legend=list(orientation="h",xanchor="center",x=.5,y=1.2),
                        xaxis=list(title="Days since 1/1/2020",
                                   autotick=F,
                                   tickmode="array",
                                   tickvals=c(seq(0,input$ahead,by=30))))
             })
           }
  )
}
