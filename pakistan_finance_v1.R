# filename : pakistan_finance_v1.R
# author: kody crowell
# date : nov 22 2018

# see article: https://www.mironline.ca/pakistans-healthcare-is-run-by-a-dead-man
# https://www.dawn.com/news/1389117
#  https://www.mironline.ca/the-art-of-pakistans-superpower-financial-finesse

# load libs
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(ggrepel)
library(lubridate)
library(readxl)
library(tmap)
library(ggmap)
library(scales)
library(rworldmap)
library(rworldxtra)
library(maptools)
library(RColorBrewer)

theme_set(theme_minimal())

'%!in%' <- function(x,y)!('%in%'(x,y))

## load data

## plots to make:
# pakistan's % military spending wrt rest of mid east
# pakistan's imf debt  foreign debt over time
# trade balance over time ?
# military numbers compared to others

mil <- read_xlsx("SIPRI-Milex-data-1949-2017.xlsx", sheet=9, range="A8:AG198",
                 col_names = T)

nations <- c("Pakistan", "Afghanistan", "India", "Turkmenistan", "Uzbekistan", "Iran", "Nepal",
             "Iraq", "Syria", "Oman", "Saudi Arabia", "Qatar", "Kuwait", "Israel", "Turkey",
             "United Arab Emirates", "Bangladesh", "Kazakhstan", "Kyrgyzstan",
             "Sri Lanka", "Lebanon", "Azerbaijan", "Bahrain", "Jordan")

mil <- mil %>% 
  select(Country, Notes, `2017`) %>%
  mutate(`2017` = as.numeric(`2017`)) %>%
  filter(!is.na(`2017`))

# plotting themes
# mir colors: #4b4b4b gray / #f00d0d red / #f9f9f9 white / #eeeeee light gray
mir.red <- "#cf0808" #"#f00d0d"
mir.white <- "#f9f9f9"
mir.gray <- "#4b4b4b"
mir.lgray <- "#eeeeee"
red.ramp <- c('#fee5d9','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#99000d')

theme_mir <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Georgia", color = mir.gray),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_text(size=rel(1.2), angle=45, hjust=1),
      axis.text.y = element_text(size=rel(1.3)),
      axis.title.y = element_text(size=rel(1.5), vjust=2),
      axis.title.x = element_text(size=rel(1.5), vjust=-0.5),
      panel.grid.major = element_line(color = "lightgray", size = 0.2),
      panel.grid.minor = element_line(color = mir.lgray, size = 0.2),
      plot.background = element_rect(fill = mir.white, color = NA), 
      panel.background = element_rect(fill = mir.white, color = NA), 
      legend.background = element_rect(fill = mir.white, color = NA),
      panel.border = element_blank(),
      ...
    )
}

mil.p <- mil %>% 
  filter(Country %in% nations) %>% 
  mutate(is.pak = as.factor(ifelse(Country=="Pakistan", 1, 0)))

mil.p$Country[mil.p$Country %in% "Iraq"] <- "Iraq *¶"
mil.p$Country[mil.p$Country %in% "Oman"] <- "Oman *‡"
mil.p$Country[mil.p$Country %in% "Kazakhstan"] <- "Kazakhstan †"
mil.p$Country[mil.p$Country %in% "Nepal"] <- "Nepal ¶"
mil.p$Country[mil.p$Country %in% "Pakistan"] <- "Pakistan ‡"
mil.p$Country[mil.p$Country %in% "Saudi Arabia"] <- "Saudi Arabia §"

# plot
ggplot(data=mil.p %>% mutate(`2017` = `2017`*100), 
       aes(x=reorder(Country, `2017`), y=`2017`, group=is.pak, fill=is.pak)) +
  geom_bar(stat="identity") +
  labs(x="Country", y="Military Spending (% general government expenditure)",
       title="Military spending of Pakistan and its neighbours",
       subtitle="Military expenditure by country as percentage of government spending, 2017",
       caption="Author: Kody Crowell (@hummushero); Source: SIPRI (2018)\n
       * Estimates are highly uncertain
       † Does not include military pensions
       ¶ Does not include spending on paramilitary forces
       ‡ Excludes capital spending
       § Adopted budget, rather than actual expenditure") +
  theme_mir() +
  coord_flip() +
  scale_fill_manual(values=c(mir.red, mir.gray)) +
  guides(fill=F) +
  geom_text(aes(label = round(`2017`, 2)), family="Georgia", hjust=1.1, color=mir.white) +
  # geom_text(aes(label = round(`2017`,2)), family="Georgia", hjust=-0.5) +
  theme(strip.text.x = element_text(size=rel(1)),
        strip.text.y = element_text(size=rel(1)),
        strip.background = element_blank(),
        legend.background = element_blank(),
        axis.title.y = element_blank(), 
        legend.justification = c(0, 0),
        plot.title = element_text(size=18, margin=margin(b=10)),
        plot.subtitle = element_text(size=12, color=mir.gray, face="italic",
                                     margin=margin(b=25)),
        plot.caption = element_text(size=10, margin=margin(t=-10), 
                                    color="grey60", hjust=1)) # 
 

## indicators positions at -4, other units US$ m
# exchange rate (113) - ? LCU/US$
# trade balance (195)
# capital flight (250) - ?
# foreign debt (254)
# imf debt (260)

eiu <- read_xls("x-WWWU07-alacra-com-20181121020149.xls", range="A4:AR261", col_names=T)
eiu <- eiu[c(109, 191, 246, 250, 256),]

eiu.long <- eiu %>% 
  select(-c(`Country/Region`, X__1, Currency, Units)) %>% 
  gather(year, measure, `1980`:`2017`) %>%
  rename(series = `Series Title`,
         code = X__2)

ggplot(data=eiu.long %>% filter(code %in% c('TDBT', 'IMFL')), 
       aes(x=year, y=measure, group=as.factor(code), color=as.factor(code))) +
  geom_line() +
  labs(x="Year", y="Debt ($USD Million)",
       title="Pakistan's Financial Finesse",
       subtitle="IMF and Foreign Debt in Pakistan, 1980-2017",
       caption="Author: Kody Crowell (@hummushero)\nSource: Economist Intelligence Unit (2018)") +
  scale_color_manual(values=rev(red.ramp[c(7,5)]), name="", labels=c("IMF Debt", "Total Foreign Debt"),
                     guide = guide_legend(
                       direction = "vertical", keyheight = unit(2, units = "mm"),
                       keywidth = unit(50/length(labels), units = "mm"),
                       title.position = 'right', title.hjust = 0.5, label.hjust = 0.5,
                       reverse = T, label.position = "top")) +
  theme_mir() +
  theme(strip.text.x = element_text(size=rel(1)),
        strip.text.y = element_text(size=rel(1)),
        strip.background = element_blank(),
        legend.background = element_blank(),
        legend.justification = c(0, 0),
        plot.title = element_text(size=18, margin=margin(b=10)),
        plot.subtitle = element_text(size=12, color=mir.gray, face="italic",
                                     margin=margin(b=25)),
        plot.caption = element_text(size=10, margin=margin(t=10), 
                                    color="grey60", hjust=0))

# load total armed forces personnel info
wdi <- read_xls("API_MS.MIL.TOTL.P1_DS2_en_excel_v2_10226453.xls", col_names=T)

wdi <- wdi %>% 
  select(`Country Name`, `2016`)

wdi.p <- wdi %>%
  filter(`Country Name` != "Libya") %>%
  mutate(is.pak = as.factor(ifelse(`Country Name`=="Pakistan", 1, 0)))

wdi.p$`Country Name`[wdi.p$`Country Name` == "Egypt, Arab Rep."] <- "Egypt"
wdi.p$`Country Name`[wdi.p$`Country Name` == "Iran, Islamic Rep."] <- "Iran"
wdi.p$`Country Name`[wdi.p$`Country Name` == "Syrian Arab Republic"] <- "Syria"
wdi.p$`Country Name`[wdi.p$`Country Name` == "United Arab Emirates"] <- "UAE"

# disable scientific notation
options(scipen = 999)

# plot
ggplot(data=wdi.p%>% filter(`Country Name` %!in% c("Kuwait", "Qatar")), 
       aes(x=reorder(`Country Name`, `2016`), y=`2016`, group=is.pak, fill=is.pak)) +
  geom_bar(stat="identity") +
  labs(x="Country", y="Total Military Personnel",
       title="Military Forces Across the Islamic World",
       subtitle="Total military personnel by country, 2016",
       caption="Author: Kody Crowell (@hummushero); Source: IISS (2018)") +
  theme_mir() +
  coord_flip() +
  scale_fill_manual(values=c(mir.red, mir.gray)) +
  guides(fill=F) +
  geom_text(aes(label = as.character(`2016`)), family="Georgia", hjust=1.1, color=mir.white) +
  theme(strip.text.x = element_text(size=rel(1)),
        strip.text.y = element_text(size=rel(1)),
        strip.background = element_blank(),
        legend.background = element_blank(),
        axis.title.y = element_blank(), 
        legend.justification = c(0, 0),
        plot.title = element_text(size=18, margin=margin(b=10)),
        plot.subtitle = element_text(size=12, color=mir.gray, face="italic",
                                     margin=margin(b=25)),
        plot.caption = element_text(size=10, margin=margin(t=-10), 
                                    color="grey60", hjust=1)) # 1400x800




