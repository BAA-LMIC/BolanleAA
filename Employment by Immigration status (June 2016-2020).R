rm(list=ls())

library(readr)
library(data.table)
library(dplyr)
library(broom)
library(tidyr)
library(zoo)
library(ggplot2)
library(stringr)
library(ggrepel)

main  <- fread("K:/RDA/LMIC Tools and Training/Economist folders/Bolanle/OLIP Economic Sector Table/14100082.csv")

main_a <- main[, .(REF_DATE, GEO, `Immigrant status`, `Labour force characteristics`, `Age group`, VALUE)]

main_a[, c('year', 'month') := tstrsplit(REF_DATE, '-')]
#main_a[, month := substr(month,2,2)]

main_a[, REF_DATE:=as.yearmon(paste(year, month), '%Y %m')]
main_a[, REF_DATE:=as.Date(REF_DATE, format = "%d-%b-%y")]

main_a[, year:=NULL]
main_a[, month:=NULL]

main_b <- main_a[REF_DATE >= '2016-03-01' & GEO == 'Canada' & `Labour force characteristics` == 'Employment' & `Age group` == '15 years and over']

main_b[REF_DATE == '2017-01-01' & `Immigrant status` == 'Landed immigrants']
main_c <- main_b[`Immigrant status` == 'Landed immigrants' | `Immigrant status` == 'Born in Canada']
main_c[, GEO := NULL]
main_c[, `Labour force characteristics` := NULL]
main_c[, `Age group` := NULL]

test <- main_c[, .(REF_DATE, `Immigrant status`)]
test[duplicated(test)]

#main_c[REF_DATE == '2017-01-01']
main_c <- 
  dcast(main_c, REF_DATE ~ `Immigrant status`,  value.var = "VALUE")

setnames(main_c, c('Born in Canada', 'Landed immigrants'), c('can', 'imm'))

base_level <- as.list(main_c[REF_DATE == '2016-03-01',  c('can', 'imm')])

main_c[, normalized_can:=can*100/base_level[["can"]]]
main_c[, normalized_imm:=imm*100/base_level[["imm"]]]

#'this function calculates the percent change of employment every three months 
#'so once the June data is loaded, it will show the change of employment between March and June
#'if you want to keep it to February just make n=4 so then it will calculate change every four months instead of 3
growth <- function(x)(x/shift(x, n=3, type = 'lag')-1)

main_c[, growth_can:=growth(can)]
main_c[, growth_imm:=growth(imm)]

break.vec <- c(as.Date("2016-06-01"), c(as.Date('2017-06-01'), c(as.Date('2018-06-01'))),
               c(as.Date("2019-06-01")), c(as.Date('2020-06-01')))

colors <- c('Canadian Born' = 'black', 'Landed Immigrants' = 'dodgerblue2')


plot1 <- 
  ggplot(main_c, aes(x=REF_DATE)) +
  geom_line(aes(y=normalized_can, color = 'Canadian Born'), size = 2) +
  geom_line(aes(y=normalized_imm, color = 'Landed Immigrants'), size = 2) +
  labs(x = 'Date',
       y = 'Employment level\n(Mar 2016 = 100)', 
       color = 'Legend',
       caption=paste0('LMIC; LFS Table ', 14100082, ' \nseasonally adjusted')) +
  ggtitle('Employment Level by Immigrant Status') + 
  scale_color_manual(values = colors) +
  scale_x_date(breaks = break.vec, date_labels = '%b %Y') +
  theme_bw() +
  theme(text = element_text(size=15),
        panel.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom", 
        legend.text = element_text(size = 15),
        axis.text.x = element_text(color="black", size=12),
        axis.title.x = element_blank(),
        axis.text.y=element_text(color='black', size=12),
        axis.title.y=element_text(size = 15),
        plot.title.position = 'panel',
        plot.title = element_text(size=15, hjust = 0.5),
        plot.caption = element_text(size=10)) +
  geom_label_repel(data=subset(main_c, REF_DATE == '2016-03-01'), aes(y=normalized_can, 
                                                                      label=paste0('Employment in Mar 2016\nCanadian Born: ', scales::comma(round(can*1000, -3)), 
                                                                                   '\nLanded Immigrants: ', scales::comma(round(imm*1000, -3)))),
                   nudge_y = -4, nudge_x = 30, segment.color = 'black') +
  geom_label_repel(data=subset(main_c, REF_DATE == '2020-06-01'), aes(y=normalized_imm,
                                                                      label=paste0('Employment in Jun 2020\nLanded Immigrants: ', scales::comma(round(imm*1000, -3)),
                                                                                   '\n Change from Mar 2020: ', round(growth_imm*100), '%')),
                   nudge_x = -300, nudge_y = 4, fill = 'dodgerblue2', color = 'white', segment.color = 'dodgerblue2') +
  geom_label_repel(data=subset(main_c, REF_DATE == '2020-06-01'), aes(y=normalized_can,
                                                                      label=paste0('Employment in Jun 2020\nCanadian Born: ', scales::comma(round(can*1000, -3)),
                                                                                   '\n Change from Mar 2020: ', round(growth_can*100), '%')),
                   nudge_x = -300, nudge_y = .5, fill = 'black', color = 'white', segment.color = 'black')

plot1

out_folder <- "K:/RDA/LMIC Tools and Training/Economist folders/Bolanle/OLIP Economic Sector Table"

setDPI = 150

ggsave(plot = plot1, "JUNE_emp_immigrant_status_feb.png",
       path = out_folder,
       bg = 'transparent', device = NULL,
       dpi = setDPI,
       width = 1450/setDPI, height = 800/setDPI)
