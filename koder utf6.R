library(tidyverse)
library(PxWebApiData)
library(patchwork) 
library(hrbrthemes)
library(reshape2)

sykefravær <- ApiData("https://data.ssb.no/api/v0/no/table/12441/", 
                      Kjonn=list('item', c("1", "2")), 
                      NACE2007=list('item', c("00-99")), 
                      Sykefraver2=list('item', c("Alt")), 
                      Tid=list('item', c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")), 
                      ContentsCode=TRUE)

sykefravær <- as.tibble(sykefravær[[1]])

sykefravær <- sykefravær %>% 
  subset(select = c(kjønn, år, value)) %>% 
  rename(sykeprosent = value)

arbeidsledige <- ApiData("https://data.ssb.no/api/v0/no/table/05111/", 
                         ArbStyrkStatus=list('item', c("2")), 
                         Kjonn=list('item', c("1", "2")), 
                         Alder=list('item', c("15-74")), 
                         ContentsCode=list('item', c("Prosent")), 
                         Tid=list('item', c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")))

arbeidsledige <- as.tibble(arbeidsledige[[1]])

arbeidsledige <- arbeidsledige %>% 
  subset(select = c(kjønn, år, value)) %>% 
  rename(ledigprosent = value)

total <- left_join(sykefravær, arbeidsledige)

total$år <- as.numeric(as.character(total$år))

totalmenn <- total %>% 
  filter(kjønn == "Menn")

manncoeff <- 1.3

mannplot <- ggplot(totalmenn, aes(x=år)) +
  geom_line(aes(y=ledigprosent), colour = "blue") +
  geom_line(aes(y=sykeprosent/manncoeff), colour = "red") +
  scale_y_continuous(name="Ledigprosent",
                     sec.axis = sec_axis(~.*manncoeff, name="Sykefravær")) +
  theme(
    axis.title.y = element_text(color = "blue", size=13),
    axis.title.y.right = element_text(color = "red", size=13)) +
  ggtitle("Sykefravær og arbeidsledighet for menn 2005-2019")


totalkvinner <- total %>% 
  filter(kjønn == "Kvinner")

kvinnecoeff <- 0.5

kvinneplot <- ggplot(totalkvinner, aes(x=år)) +
  geom_line(aes(y=ledigprosent), colour = "blue") +
  geom_line(aes(y=sykeprosent*kvinnecoeff), colour = "red") +
  scale_y_continuous(name="Ledigprosent",
                     sec.axis = sec_axis(~./kvinnecoeff, name="Sykefravær")) +
  theme(
    axis.title.y = element_text(color = "blue", size=13),
    axis.title.y.right = element_text(color = "red", size=13)) +
  ggtitle("Sykefravær og arbeidsledighet for kvinner 2005-2019")

mannplot + kvinneplot