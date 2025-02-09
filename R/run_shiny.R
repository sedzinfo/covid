##########################################################################################
# COVID-19 TIME SERIES
##########################################################################################
#' @title COVID-19 time series
#' @param country_region country
#' @import shiny
#' @importFrom utils read.csv
#' @importFrom shinyWidgets dropdownButton tooltipOptions
#' @importFrom reshape2 melt
#' @importFrom plotly plotlyOutput renderPlotly plot_ly layout add_trace
#' @importFrom plyr ddply
#' @keywords covid
#' @export
#' @examples
#' covid(country_region="Germany")
#' covid(country_region="Greece")
covid<-function(country_region="Greece"){
  options(scipen=999)
  confirmed<-read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",stringsAsFactors=FALSE,check.names=FALSE)
  deaths<-read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",stringsAsFactors=FALSE,check.names=FALSE)
  
  covid_c<-reshape2::melt(confirmed,id.vars=c("Province/State","Country/Region","Lat","Long"),variable.name="Date",value.name="Confirmed")
  covid_d<-reshape2::melt(deaths,id.vars=c("Province/State","Country/Region","Lat","Long"),variable.name="Date",value.name="Deaths")
  covid_all<-merge(covid_c,covid_d,all=TRUE,by=c("Province/State","Country/Region","Date","Lat","Long"))
  covid_all<-transform(covid_all,Days=as.numeric(strftime(as.Date(covid_all$Date,format='%m/%d/%Y'),'%j')))
  names(covid_all)<-make.names(names(covid_all))
  
  ui<-tagList(tags$head(tags$script('
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
              fluidPage(shinyWidgets::dropdownButton(selectInput(inputId="country",
                                                                 label="Country",
                                                                 choices=sort(unique(covid_all$Country.Region)),
                                                                 selected=c(country_region),
                                                                 selectize=FALSE,
                                                                 size=20,
                                                                 width="100%"),
                                                     circle=FALSE,status="info",
                                                     icon=icon("info"),
                                                     width="500px",
                                                     inline=TRUE,
                                                     tooltip=shinyWidgets::tooltipOptions(title="Click!")),
                        plotly::plotlyOutput("plot_country",width="100%")))
  
  server<-function(input,output) {
    output$plot_country<-renderPlotly({
      temp_country<-plyr::ddply(covid_all[covid_all$Country.Region %in% input$country,],c("Date","Country.Region"),plyr::numcolwise(sum,na.rm=TRUE))
      plot_ly(temp_country,
              x=~Date,
              y=~Confirmed,
              text=~paste0("\nConfirmed infections=",temp_country$Confirmed,
                           "\nDeaths=",temp_country$Deaths,
                           "\nCountry=",input$country,
                           "\nDate=",temp_country$Date),
              name='Infections',
              mode="markers",
              type="scatter",
              width=(as.numeric(input$dimension[1])-30),
              height=(as.numeric(input$dimension[2])-50)) %>% 
        add_trace(y=~temp_country$Deaths,name='Deaths',mode='markers') %>%
        layout(title=unique(input$country),
               margin=list(l=50,r=50,b=50,t=50,pad=0),
               legend=list(orientation="h",xanchor="center",x=.5,y=1.2),
               xaxis=list(title="Date"))
    })
  }
  
  shinyApp(ui=ui,server=server)
}
