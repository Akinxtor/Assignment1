library(tidyverse)
library(rvest)

url <- "https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/"

page <- read_html(url)

column_data <- page %>%
  html_nodes(".countRow>td") %>%
  html_text() %>%
  trimws()

extracted_column1 <- column_data[seq(2, length(column_data), by = 15)]
extracted_column2<- column_data[seq(3, length(column_data), by = 15)]
extracted_column3 <- column_data[seq(4, length(column_data), by = 15)]
extracted_column4 <- column_data[seq(5, length(column_data), by = 15)]
extracted_column5 <- column_data[seq(6, length(column_data), by = 15)]
extracted_column6<- column_data[seq(7, length(column_data), by = 15)]
extracted_column7<- column_data[seq(8, length(column_data), by = 15)]
extracted_column8 <- column_data[seq(9, length(column_data), by = 15)]
extracted_column9<- column_data[seq(10, length(column_data), by = 15)]
extracted_column10 <- column_data[seq(11, length(column_data), by = 15)]
extracted_column11 <- column_data[seq(12, length(column_data), by = 15)]
extracted_column12 <- column_data[seq(13, length(column_data), by = 15)]

nifty50_data <- tibble(
  "Company" = extracted_column1,
  "CMP" = extracted_column2,
  "Price Change" = extracted_column3,
  "Market Cap (Cr)" = extracted_column4,
  "52 Week High" = extracted_column5,
  "52 Week Low" = extracted_column6,
  "ROE" = extracted_column7,
  "P/E" = extracted_column8,
  "P/BV" = extracted_column9,
  "EV/EBITDA" = extracted_column10,
  "5YSales Gr(%)" = extracted_column11,
  "5YProfit Gr(%)" = extracted_column12
)

print(nifty50_data)

tennis<- function(p)
{
  x<-0
  A<-0
  B<-0
  for(i in 1:5)
  {
    x<-x+1
    if(sample(0:1,size=1,prob=c(1-p,p)))
    {
      A<-A+1
    }
    else
    {
      B<-B+1
    }
    if(A>2|B>2)
    {
      return(x)
    }
  }
}
matches<- vector(length=1000)
for(i in 1:1000)
{
  matches[i]=tennis(0.7)
}
ans<-mean(matches)
MontyHall <- function() {
  doors <- c(0, 0, 1)
  contestant_choice <- sample(1:3, 1)
  
  remaining_doors <- setdiff(1:3, contestant_choice)
  monty_opened_door <- sample(remaining_doors[doors[remaining_doors] == 0], 1)
  
  remaining_door <- setdiff(1:3, c(contestant_choice, monty_opened_door))
  switched_choice <- remaining_door[1]
  
  if (doors[switched_choice] == 1) {
    return(1)  
  } else {
    return(0)  
  }
}
results<-replicate(1000,MontyHall())
probability<-mean(results)
