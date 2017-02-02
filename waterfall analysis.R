###########################################
###########################################
#********** Funnel analysis  *************#
###########################################
###########################################

library(ggplot2)

user_data <- data.frame(desc = c("Login/Registration","Dietary login", "Meds entry", "Insulin dose entry", 
                                 "Preprandial blood sugar measurement","Postprandial blood sugar measurement", "Sleep duration"), 
                        Total_users = c(2000,3400, -1100, -100, -6600, 3800, 1400))

user_data$desc <- factor(user_data$desc, levels = user_data$desc)
user_data$id <- seq_along(user_data$Total_users)
user_data$type <- ifelse(user_data$Total_users > 0, "in","out")
user_data[user_data$desc %in% c("Starting Cash", "End Cash"),"type"] <- "net"

user_data$end <- cumsum(user_data$Total_users)
user_data$end <- c(head(user_data$end, -1), 0)
user_data$start <- c(0, head(user_data$end, -1))
user_data<- user_data[, c(1,3,4,6,5,2)]

ggplot(user_data, aes(desc, fill = type)) + geom_rect(aes(x = desc,
                                                        xmin = id - 0.45, xmax = id + 0.45, ymin = end,
                                                        ymax = start))

strwr <- function(str) gsub(" ", "\n", str)

p1 <- ggplot(user_data, aes(fill = type)) + geom_rect(aes(x = desc,xmin = id - 0.45, xmax = id + 0.45, ymin = end,
                                                          ymax = start)) + 
  scale_x_discrete("", breaks = levels(user_data$desc),labels = strwr(levels(user_data$desc))) 


