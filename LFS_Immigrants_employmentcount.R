rm(list=ls())

library(readr)
library(data.table)
library(dplyr)
library(broom)
library(tidyr)
library(zoo)
library(ggplot2)
library(stringr)

main  <- fread("K:/RDA/LMIC Tools and Training/Economist folders/Bolanle/OLIP Economic Sector Table/LFS characteristics by immigrant status.csv")

#zoe: added immigrant status filter
          main_a  <- main %>% 
            filter(`Labour force characteristics` == "Employment", `Age group` == "25 to 54 years" , `Immigrant status` == 'Landed immigrants' | `Immigrant status` == 'Born in Canada',
                   REF_DATE == "Jan-19" | REF_DATE == "Feb-19" | REF_DATE == "Mar-19" | REF_DATE == "Apr-19" | REF_DATE == "May-19"  | REF_DATE == "Jan-20" |
                     REF_DATE == "Feb-20" | REF_DATE == "Mar-20" | REF_DATE == "Apr-20" | REF_DATE == "May-20")


## don't ever delete this ### 

main_a$REF_DATE <- as.Date(paste0("1-", main_a$REF_DATE),format="%d-%b-%y")


main_a <- main_a %>% select(REF_DATE, GEO, `Immigrant status`, VALUE)


## change table format from long to wide ###

#main_c <- dcast(main_a, REF_DATE + GEO + `Age group` + `Labour force characteristics`  ~ `Immigrant status`,  value.var = "VALUE")


## converts date to usable date format ###
#main_c$REF_DATE <- format(main_c$REF_DATE, "%b-%Y")




## seperate REF_DATE column ## 

# main_b <- separate(main_a, col = REF_DATE, into = c("Month", "Year"), sep = "-")

#as.yearmon(paste(main_b$Year, main_b$Month), '%Y %m')

## change class of value to numeric ##
# main_b$Year <- as.numeric(main_b$Year)
# class(main_b$Year)
# as.character.numeric_version(main_b$Month)

###############################################################################################################################################################################################################

#Cleaning data for plotting## 

## seperate REF_DATE column ## 

main_gg <- separate(main_a, col = REF_DATE, into = c("Year", "Month"), sep = "-")

            #' zoe: merge the text in the Immigrant status and year to make one variable
            main_gg$imm_status <- paste(main_gg$`Immigrant status`, main_gg$Year)

            # zoe: remerging month and year for one date variable in case you want to look at time trends
            main_gg$date <- paste(main_gg$Month, main_gg$Year)



Canada <-  main_gg %>% filter(GEO == "Canada")
Province <-  main_gg %>% filter(GEO != "Canada", GEO!= "Toronto, Ontario", GEO != "Vancouver, British Columbia", GEO!= "Toronto, Ontario", GEO!= "MontrÃ©al, Quebec", 
                                GEO != "Atlantic region", GEO!=  "Manitoba and Saskatchewan")

################################################################################################################################################################################################################
           
          #I don't think we can include all info in one graph: immigrant status, year, and province
        
          ### Canada-wide: immigrant status and year ###


          ggplot(Canada, aes(x=Month, y=VALUE, fill = imm_status)) +
            geom_bar(position = 'dodge', stat = 'identity') + #' or you can change it to position = 'stack' 
            scale_x_discrete(breaks = c('01', '02', '03', '04', '05'), labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May')) +
            labs(y = 'Employment level') +
            theme_bw() +
            theme(legend.position = 'bottom',
                  legend.title = element_blank(),
                  axis.title.x = element_blank())
        
          ggplot(Canada, aes(x=Month, y=VALUE, fill = imm_status)) +
            geom_bar(position = 'stack', stat = 'identity') + #' or you can change it to position = 'stack' 
            scale_x_discrete(breaks = c('01', '02', '03', '04', '05'), labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May')) +
            labs(y = 'Employment level') +
            theme_bw() +
            theme(legend.position = 'bottom',
                  legend.title = element_blank(),
                  axis.title.x = element_blank())
          
################################################################################################################################################################################

          ### By province: immigration status and year ###
          
          #' one solution I found online is that you can make the x-axis show both month and immigration status
          #' but we need to fix the x-axis since it is very messy
        
          #Province$imm_status  <-  as.factor(Province$imm_status, 
                                             levels = c("01.Landed immigrants 2019", "01.Born in Canada 2019", "02.Landed immigrants 2019", "02.Born in Canada 2019", "03.Landed immigrants 2019", "03.Born in Canada 2019", "04.Landed immigrants 2019", 
                                                        "04.Born in Canada 2019", "05.Landed immigrants 2019", "05.Born in Canada 2019", "01.Landed immigrants 2020", "01.Born in Canada 2020", "02.Landed immigrants 2020", "02.Born in Canada 2020", "03.Landed immigrants 2020", "03.Born in Canada 2020", "04.Landed immigrants 2020", 
                                                        "04.Born in Canada 2020", "05.Landed immigrants 2020", "05.Born in Canada 2020"), labels = c("Jan 2019", "Jan 2019", "Feb 2019", "Feb 2019", "Mar 2019", "Mar 2019", "Apr 2019", "Apr 2019", "May 2019", "May 2019", "Jan 2020", "Jan 2020", "Feb 2020", "Feb 2020", "Mar 2020", "Mar 2020", "Apr 2020", "Apr 2020", "May 2020", "May 2020"))
          


          Province$imm_status <- factor(Province$imm_status, levels = c('Landed immigrants 2019', 'Born in Canada 2019',
                                                             'Landed immigrants 2020', 'Born in Canada 2020'))

          
          
          #ggplot(Province, aes(x=as.character(interaction(Month, imm_status)), y=VALUE, fill = GEO)) +
            ggplot(Province, aes(x= imm_status, y=VALUE, fill = GEO)) +
            geom_bar(stat = 'identity', color = 'white') +
            labs(y= 'Employment level') +
            coord_flip() + #Bolanle added coord_flip so we can read the axis 
            #geom_text(aes( label = `Immigrant status`),
                     # position = position_dodge(1.1),
                     # vjust = 2) +
            theme_bw() +
            theme(legend.position = 'bottom',
                  legend.title = element_blank(),
                  axis.text.x = element_blank(),
                  axis.title.y = element_blank())
            
            
            ggplot(aes(nlp_frequency, 
                       x = fct_relevel(response, 
                                       "Rarely", "Sometimes", "Often", "Most of the time"))) + 
              geom_bar()
                  
            
            geom_text(aes(label = percent(perc_rude), 
                          y = perc_rude + .03), 
                      position = position_dodge(0.9),
                      vjust = 1)
            
            theme(
              text = element_text(family = "Bookman"),
              title = element_text(color = "gray25"),
              plot.caption = element_text(color = "gray30"),
              plot.subtitle = element_text(size = 12)
            )
            
          
          
          ##Notes ##
          
            ## Reorder: SI2, SI1, I1
            p + scale_x_discrete(limit = c("SI2", "SI1", "I1"))
            
            scale_x_discrete(limit = c("Born in Canada-Jan", "Landed Immigrant-Jan", "Born in Canada-Feb", "Landed Immigrant-Feb", 
                                       "Born in Canada-Mar", "Landed Immigrant-Mar", "Born in Canada-Apr", "Landed Immigrant-Apr", 
                                       "Born in Canada-May", "Landed Immigrant-May"))
            
            scale_y_continuous(expand = c(0,0)) + scale_x_discrete(expand = c(0,0))
            
            
            



            theme(
              text = element_text(family = "Bookman"),
              title = element_text(color = "gray25"),
              plot.caption = element_text(color = "gray30"),
              plot.subtitle = element_text(size = 12)
            )


















































 main_a  <- main %>% 
           filter(`Labour force characteristics` == "Employment", `Age group` == "25 to 54 years" , 
                              REF_DATE == "Jan-19" | REF_DATE == "Feb-19" | REF_DATE == "Mar-19" | REF_DATE == "Apr-19" | REF_DATE == "May-19"  | REF_DATE == "Jan-20" |
                          REF_DATE == "Feb-20" | REF_DATE == "Mar-20" | REF_DATE == "Apr-20" | REF_DATE == "May-20")
 
       
        ## don't ever delete this ### 
 
      main_a$REF_DATE <- as.Date(paste0("1-", main_a$REF_DATE),format="%d-%b-%y")
      
     
       main_a
       
 
        ## change table format from long to wide ###
        
        main_c <- dcast(main_a, REF_DATE + GEO + `Age group` + `Labour force characteristics`  ~ `Immigrant status`,  value.var = "VALUE")
   
        
        ## converts date to usable date format ###
        main_c$REF_DATE <- format(main_c$REF_DATE, "%b-%Y")
        
            
      
        
        ## seperate REF_DATE column ## 
        
           # main_b <- separate(main_a, col = REF_DATE, into = c("Month", "Year"), sep = "-")
            
            #as.yearmon(paste(main_b$Year, main_b$Month), '%Y %m')
  
            ## change class of value to numeric ##
            main_b$Year <- as.numeric(main_b$Year)
            class(main_b$Year)
            as.character.numeric_version(main_b$Month)
            
            
            #Plots## 
            
              ## seperate REF_DATE column ## 
            
               main_gg <- separate(main_c, col = REF_DATE, into = c("Month", "Year"), sep = "-")
            
        
        
             Canada <-  main_gg %>% filter(GEO == "Canada")
             Province <-  main_gg %>% filter(GEO != "Canada", GEO!= "Toronto, Ontario", GEO != "Vancouver, British Columbia", GEO!= "Toronto, Ontario", GEO!= "MontrÃ©al, Quebec", 
                                            GEO != "Atlantic region", GEO!=  "Manitoba and Saskatchewan")
             
        
                ggplot(Province, aes(x = month)) + 
                    geom_bar(aes (y = `Born in Canada`)) +
                    geom_bar(aes (y = `Landed immigrants`))
   
        
        
######################################################################################################################################################
        
        
        
                Canada$imm_status <- factor(Canada$imm_status, levels = c('Landed immigrants 2019', 'Born in Canada 2019',
                                                                          'Landed immigrants 2020', 'Born in Canada 2020'))
                
                Canada$Month <- factor(Canada$Month, levels = c('05', '04', '03', '02', '01'))
                
                ggplot(Canada, aes(x=Month, y=VALUE, fill = imm_status)) +
                  geom_bar(position = 'dodge', stat = 'identity') +
                  geom_text(aes(x=Month, y= VALUE, label = VALUE), position = position_dodge(width = 1), hjust = -0.1) +
                  scale_x_discrete(breaks = c('01', '02', '03', '04', '05'), labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May')) +
                  labs( y = 'Employment level (in thousands)', title = "Employment Level by Immigration Status") +
                  #coord_flip() + 
                  theme_bw() +
                  theme(legend.position = 'bottom',
                        legend.title = element_blank(),
                        axis.title.x = element_blank()) 
                
                Canada$Month <- factor(Canada$Month, levels = c('05', '04', '03', '02', '01'))
                
                ggplot(Canada, aes(x=Month, y=VALUE, fill = imm_status)) +
                  geom_bar(position = 'dodge', stat = 'identity') +
                  geom_text(aes(x=Month, y= VALUE, label = VALUE), position = position_dodge(width = 1), hjust = -0.1) +
                  scale_x_discrete(breaks = c('01', '02', '03', '04', '05'), labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May')) +
                  labs( y = 'Employment level (in thousands)', title = "Employment Level by Immigration Status") +
                  coord_flip() + 
                  theme_bw() +
                  theme(legend.position = 'bottom',
                        legend.title = element_blank(),
                        axis.title.y = element_blank()) 