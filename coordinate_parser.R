if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(xml2)) install.packages("xml2", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(RCurl)) install.packages("RCurl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")

wd <- '/Users/...ENTERYOURS HERE' ## make sure to set your directory
setwd(wd)

list.files()

html <- read_html("coordinates.txt") # pull in coordinate file

# map <- xml_children(xml_children(html))[4]

map <- html_nodes(html, "area")

# or xml_find_all(html, ".//area")


shape <- html_attr(map, "shape")

coords <- html_attr(map, "coords")
coords <- str_replace_all(coords, "[\r\n]" , " ")

href <- html_attr(map, "href")

#############

num_poly <- length(coords)

length(coords[c(1:num_poly)])

# str_split(coords, " ")


final <- data.frame()
test <- list()

test <- lapply(str_split(coords," "),function(x) {
  str_split(x, ",") } )

for ( i in 1:num_poly ) {

  df <- data.frame()
  list <- test[[i]]
  
  for (j in 1:length(list) ) { # i is number of poly
  temp <- list[[j]]
  
  df[j,1] <- href[i] # build data frame
  df[j,2] <- j  # path id
  df[j,3] <- temp[1] # x
  df[j,4] <- temp[2] # y
  df[j,5] <- i
  
  }
  final <- rbind(final, df)
}

names(final) <- c("href", "path_id", "x", "y", "href_num")

final <- final[!is.na(final[,4]),]

library(xlsx)

write.xlsx(final, "coordinates2.xlsx", sheetName = "coords", row.names = FALSE )

library(ggplot2)
library(gganimate)
final %>% filter(href_num >= 1) %>% ggplot(aes(x, y, color=path_id)) + geom_path(aes(group=href_num)) +
  scale_colour_gradient(low = "dark green", high = "red") +
  transition_manual(path_id, cumulative = T) + 
  ggtitle("Face Plot") + theme(legend.position = "none")
#+ geom_text(aes(label=path_id), vjust=2) + scale_x_continuous(limits = c(-.5,9))
