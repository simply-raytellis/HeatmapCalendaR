# HeatmapCalendaR

An R function to display time-series data as a calendar heatmap. Returns a ggplot object.

Required libraries: tidyverse, sqldf, ggthemes

Weeks start on Monday and end on Sunday

## Arguments
| Argument | Details |
| -- | -- |
| data | A dataframe with columns "Date" and "Value |
| plotTitle | A string to be used as the chart's title (default blank) |
| plotSubtitle | A string to be used as the chart's subtitle (default blank) |
| legendTitle | A string to be used as the legend's title (default blank) |
| fontSize | Base fontsize in points. Titles, subtitles etc are scaled proportionally (default 14) |
| daylabels | A string vector of length 7 with the y axis Day labels (default ddd) |
| monthLabels| A string vector of length 12 with the x axis Month labels (default mmm) |
| dayOutline | A color for the outline of each day (default "white") |
| monthOutline | A color for the outline of month and year (default "black") |
| lineWidth | ggplot linewidth for month and year outlines (default 1) |
| viridisOption | The viridis color scale option to use for the heatmap (default "mako") |
| viridisDirection | The direction option for color scale direction for the heatmap (default -1) |

## Example Usage

```R
source("Heatmap CalendaR.r")

d = data.frame(Date = seq.Date(from = as.Date("2021-01-01"), to = as.Date("2023-12-31"), by = "day"))

set.seed(123)
d$Value = rnorm(nrow(d))

calendarHeat(data = d, fontSize = 10, plotTitle = "Example heatmap", plotSubtitle = "Random data", legendTitle = "Normalized data")
```
![Example](https://github.com/user-attachments/assets/64a9b6da-d001-4b96-9120-84c5aeb4632a)


