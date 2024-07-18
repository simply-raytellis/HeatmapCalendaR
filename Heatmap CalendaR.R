##############################################################################
#                        Heatmap CalendaR                                    #
#                                by                                          #
#                           Ray Tellis                                       #
############################################################################## 


## calendarHeat: An R function to display time-series data as a calendar heatmap 
## A complete rewrite of Paul Bliecher's Calendar Heatmap, retrieved from 
## https://gist.github.com/aaronwolen/4d93d7e676ed01351afb

## This program is free software and is licensed under Creative Commons Zero (CC0)
## https://creativecommons.org/publicdomain/zero/1.0/
## CC0 (aka CC Zero) is a public dedication tool, which enables creators to 
## give up their copyright and put their works into the worldwide public domain. 
## CC0 enables reusers to distribute, remix, adapt, and build upon the material 
## in any medium or format, with no conditions.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

#' An R function to display time-series data as a calendar heatmap
#' Weeks start on Monday and end on Sunday
#'

#'
#' @param data A dataframe with columns "Date" and "Value
#' @param plotTitle A string to be used as the chart's title (default blank)
#' @param plotSubtitle A string to be used as the chart's subtitle (default blank)
#' @param legendTitle A string to be used as the legend's title (default blank)
#' @param fontSize Base fontsize in points. Titles, subtitles etc are scaled proportionally (default 14)
#' @param daylabels A string vector of length 7 with the y axis Day labels (default ddd)
#' @param monthLabels A string vector of length 12 with the x axis Month labels (default mmm)
#' @param dayOutline A color for the outline of each day (default "white")
#' @param monthOutline A color for the outline of month and year (default "black")
#' @param lineWidth ggplot linewidth for month and year outlines (default 1)
#' @param viridisOption The viridis color scale option to use for the heatmap (default "mako")
#' @param viridisDirection The direction option for color scale direction for the heatmap (default -1)
#' @return ggplot object
#' @author Ray Tellis
#' @export

calendarHeat <- function(
    data,
    plotTitle = "",
    plotSubtitle = "",
    legendTitle = "",
    fontSize = 14,
    dayLabels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
    monthLabels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
    dayOutline = "white",
    monthOutline = "black",
    lineWidths = 1,
    viridisOption = "mako",
    viridisDirection = -1
) {


require(tidyverse)
require(sqldf)
require(ggthemes)

names(data)[1:2] = c("Date", "Value")
    
data$Year = year(data$Date) - min(year(data$Date)) 
data$DayOfYear = yday(data$Date)
data$LeapYear = leap_year(data$Date)
data$Month = month(data$Date)
data$Week = as.numeric(strftime(data$Date, format = "%W")) + 1
data$Day = wday(data$Date, week_start = 1)
data$x = data$Week
data$y = data$Year * 9 + data$Day


yLabs = rep(c(dayLabels, "", ""), max(data$Year) + 1)

xLabs = data.frame(Labels = rep(monthLabels, max(data$Year) + 1))
xLabs$y = rep(seq(8, (max(data$Year) + 1) * 9, by = 9), each = 12)
xAvgs = sqldf("select avg(Week) as x from data group by Month order by Month")
xLabs$x = rep(xAvgs$x, times = max(data$Year) + 1)

yearLabs = data.frame(Labels = unique(year(data$Date)))
yearLabs$x = 27.5
yearLabs$y = seq(from = 0, to = max(data$Year) * 9, by = 9)


YearOutline = rbind(
    data.frame(
        xstart = numeric(),
        xend = numeric(),
        ystart = numeric(),
        yend = numeric()
    ),
    # find the first day of the year and define outline segments
    sqldf("
        select
            Week - 0.5 as xstart,
            Week + 0.5 as xend,
            Year * 9 + Day - 0.5 as ystart,
            Year * 9 + Day - 0.5 as yend

            from data

            where
                DayOfYear = 1
    "),
    # find the first week of the year and define outline segments
    sqldf("
        select 
            Week - 0.5 as xstart,
            Week - 0.5 as xend,
            Year * 9 + Day - 0.5 as ystart,
            Year * 9 + Day + 0.5 as yend
            
            from (
                select
                    Year,
                    Week,
                    Day,
                    row_number() over (partition by Year, Day order by Year, DayOfYear) as DayIncidence 
                    
                    from data
            ) 
            
            where
                DayIncidence = 1
    "),
    # define outline segments for the top of each year
    sqldf("
        select
            Week - 0.5 as xstart,
            Week + 0.5 as xend,
            Year * 9 + Day - 0.5 as ystart,
            Year * 9 + Day - 0.5 as yend
            
            from data
            
            where
                Day = 1
    "),
    #define outline segments for bottom of each year
    sqldf("
        select 
            Week - 0.5 as xstart,
            Week + 0.5 as xend,
            Year * 9 + Day + 0.5 as ystart,
            Year * 9 + Day + 0.5 as yend
            
            from (
                select
                    Year,
                    Week,
                    Day,
                    row_number() over (partition by Year, Month order by Year, DayOfYear desc) as DayIncidence 
                    
                    from data
                )
            
            where
                Day = 7 and DayIncidence <> 1
    ")
    )

MonthOutline = rbind(
    data.frame(
        xstart = numeric(),
        xend = numeric(),
        ystart = numeric(),
        yend = numeric()
    ),
    # define vertical segments for month outlines
    sqldf("
        select
            Week + 0.5 as xstart,
            Week + 0.5 as xend,
            Year * 9 + Day - 0.5 as ystart,
            Year * 9 + Day + 0.5 as yend
            
            from (
                select
                    Year,
                    Week,
                    Day,
                    row_number() over (partition by Year, Month, Day order by Year, DayOfYear desc) as DayIncidence 
                    
                    from data
                )
                
            where
                DayIncidence = 1
    "),
    # define horizontal segements for month outlines
    sqldf("
        select
            Week - 0.5 as xstart,
            Week + 0.5 as xend,
            Year * 9 + Day + 0.5 as ystart,
            Year * 9 + Day + 0.5 as yend
            
            from (
                select
                    Year,
                    Week,
                    Day,
                    row_number() over (partition by Year, Month order by Year, DayOfYear desc) as DayIncidence 
                    
                    from data
                )
                
            where
                DayIncidence = 1
    ")
)
                
                    
ggplot() + 
    geom_tile(
        data = data,
        aes(x = x, y = y, fill = Value),
        color = dayOutline
    ) +
    geom_segment(
        data = YearOutline,
        aes(x = xstart, xend = xend, y = ystart, yend = yend),
        linewidth = lineWidths,
        lineend = "square",
        color = monthOutline
    ) +
    geom_segment(
        data = MonthOutline,
        aes(x = xstart, xend = xend, y = ystart, yend = yend),
        linewidth = lineWidths,
        lineend = "square",
        color = monthOutline
    ) +
    geom_text(
        data = xLabs,
        aes(x = x, y = y, label = Labels),
        vjust = 0.5,
        size = fontSize / .pt
    ) + 
    geom_text(
        data = yearLabs,
        aes(x = x, y = y, label = Labels),
        vjust = 0.25,
        size = fontSize * 1.25 / .pt
    ) + 
    scale_fill_viridis_c(
        option = viridisOption,
        direction = viridisDirection,
        name = legendTitle
    ) +
    scale_y_reverse(
        breaks = seq(from = 1, to = max(data$y) + 2, by = 1),
        labels = yLabs,
        expand = c(0,0.5),
        sec.axis = dup_axis()
    ) + 
    scale_x_continuous(
        expand = c(0,0.1)
    ) + 
    coord_equal(
        clip = 'off'
    ) + 
    theme_fivethirtyeight() +
    theme(
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(fontSize,fontSize,fontSize,fontSize), "pt"),
        text = element_text(size = fontSize),
        legend.title = element_text(vjust = .75),
        plot.title = element_text(size = fontSize * 2),
        plot.subtitle = element_text(size = fontSize * 1.5)
    ) +
    labs(
        title = plotTitle,
        subtitle = plotSubtitle
    ) 

}
