# Heatmaps for Health Insurance Products using ggplot
# Preshit Ambade
# preshitambade@gmail.com
# 2020-02-25

# create r project first and then following directories
#dir.create("r_HIproduct_data")
#dir.create("r_HIproduct_output")


#rename scheme names

# bring rural-urban variable into the data

# use data
mine.data <- read.csv(file = "heatmap_HI_products_statewise.csv") #use tab to fill complete the word
str(mine.data)  #tells structure of the data

# Rename column names for schemes
names(mine.data)[names(mine.data) == "ESIS"] <- "1"
names(mine.data)[names(mine.data) == "CGHS"] <- "2"
names(mine.data)[names(mine.data) == "State.Scheme"] <- "3"
names(mine.data)[names(mine.data) == "RSBY"] <- "4"
names(mine.data)[names(mine.data) == "CBHI"] <- "5"
names(mine.data)[names(mine.data) == "Employer.Based"] <- "6"
names(mine.data)[names(mine.data) == "Medical.Reimbursement"] <- "7"
names(mine.data)[names(mine.data) == "Private"] <- "8"
names(mine.data)[names(mine.data) == "Other"] <- "9"

mine.data

# use tidyr for data wrangling ie convert data from wide to long
# install.packages("tidyr")
library(tidyr)
head(mine.data) #tells the headings of the data set and first six rows of the data

mine.long <- gather(data = mine.data, # command to change wide data to long. Put [,-4] before comma if want to exclude 4th column
                    key = Scheme.Name, #this tells column names will go into new variable Bacterial.class
                    value = Percentages,  #tells which values to put in the column Abundance
                    -c(1:2)) # column 1 not transformed

head(mine.long) #check the transformation

# use ggplot2 for heatmap
# install.packages("ggplot2")
library(ggplot2)

# create object for plotting and then create heatmap with adding subtitle
library(cowplot)
mine.heatmap <- ggplot(data = mine.long, mapping = aes(x = Scheme.Name,
                                                         y = State,
                                                         fill = Percentages)) +
  geom_tile() +  #tells type of plot
  xlab(label = "1-ESIS 2-CGHS 3-State 4-RSBY 5-CBHI 6-Employer 7-Reimbursement 8-Private 9-Other") +   # tells how to label the values
  facet_grid(~ Scheme.Name, scales = "free_x", space = "free_x") +  # creates miniplots according to depth and drops missing columns in each miniplot
  scale_fill_gradient2(low = "#FFFFFF",
                       mid = "#87CEFA",
                       high = "#012345")+ #hexa-decimal codes for colors   
  ggtitle(label = "Health Insurance Penetration At The State Level (2015-16)") +
  labs(subtitle = "Faceted By Each Scheme Type (%)") +  # added code to display subtitle
  theme_bw() +  # reversed the order of the code otherwise the titles will not be centered
  theme(plot.title = element_text(hjust = 0.5, face="bold"),   # theme(plot.title = element_text(hjust = 0.5)) code from stackoverflow
        plot.subtitle = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +  #source- https://stackoverflow.com/questions/35090883/remove-all-of-x-axis-labels-in-ggplot
  scale_y_discrete(limits = rev(levels(as.factor(mine.long$State)))) # plot with reversing the order of states

mine.heatmap # show the created plot

ggdraw(add_sub(mine.heatmap,"Source: Author's Calcutions from NFHS-4 data", size = 10, fontface="italic")) 

# source-https://rdrr.io/cran/cowplot/man/add_sub.html
# source-https://www.datanovia.com/en/blog/ggplot-title-subtitle-and-caption/

# saving ggplot-http://www.sthda.com/english/wiki/ggsave-save-a-ggplot-r-software-and-data-visualization
ggsave(file="statelevel_HIscheme_penetration.pdf")  
ggsave(file="statelevel_HIscheme_penetration.png")  



