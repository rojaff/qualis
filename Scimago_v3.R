
gc()

library(tidyverse)
library(readxl)
library(bayestestR)
library(gridExtra)

#### Overall citations----
## Load and prepare data
## Source: Scimago Country Rankings (Latin America) - https://www.scimagojr.com/countryrank.php
files.la <- list.files(path="Scimago/LA", 
                       pattern = ".xlsx",  full.names = TRUE)
years <- 2000:2019
la.countries <- list()
for(i in 1:length(files.la)){
  la.countries[[i]] <- read_excel(files.la[i])
  la.countries[[i]] <- la.countries[[i]] %>% mutate(year=years[i])
}

la.countriesF <- bind_rows(la.countries)
la.countriesF <- la.countriesF %>% rename(Citable_documents = "Citable documents", Self_citations = "Self-citations",
                                    Citations_per_document = "Citations per document", H_index = "H index")

## Visualize data
la.countriesF
unique(la.countriesF$Country)
la.countriesF %>% filter(year==2019) %>% arrange(desc(Citable_documents))
la.countriesF %>% filter(Country=="Brazil") %>% ggplot(aes(x=year, y=Documents)) + geom_line()
la.countriesF %>% filter(Country=="Brazil") %>% ggplot(aes(x=year, y=Citations_per_document)) + geom_line()
la.countriesF %>% filter(Country=="Brazil") %>% ggplot(aes(x=year, y=Self_citations)) + geom_line()

## Plot overall citations over time for top 5 LA countries (Fig. 1)
pal <- c("cyan", "green", "blue3", "yellow", "red")

cit <- la.countriesF %>% filter(Country %in% c("Brazil", "Mexico", "Chile", "Argentina", "Colombia")) %>%
  ggplot(aes(x=year, y=Citations_per_document, group = Country)) + 
  geom_point(aes(size=Citable_documents, color=Country)) + 
  scale_size_continuous(range = c(1,10)) +
  geom_line(aes(color=Country), size=2, alpha=0.75) +
  scale_x_continuous(breaks = c(2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018)) +
  scale_colour_manual(values=pal) + geom_vline(xintercept=2009, color="black", linetype="dashed", size=1) +
  ylab("Citations per document") + xlab("Year") + 
  guides(fill=guide_legend(title="Country"), size=guide_legend(title="Citable documents")) +
  theme_minimal() + theme(
    legend.position = "bottom", 
    legend.title=element_text(size=10, face="bold"),
    legend.text=element_text(size=10),
    axis.title.x = element_text(size=15, face="bold"),
    axis.title.y = element_text(size=15, face="bold"))

cit
ggsave("LA_countries.png", width = 28, height = 18, units = "cm", dpi = 300)
ggsave("LA_countries.tif", plot = cit, device = "tiff", width = 28, height = 18, units = "cm", dpi = 300)

## Overall documents over time (not shown in the preprint)
doc <- la.countriesF %>% filter(Country %in% c("Brazil", "Mexico", "Chile", "Argentina", "Colombia")) %>%
  ggplot(aes(x=year, y=Citable_documents/1000, group = Country)) + 
  geom_line(aes(color=Country), size=2, alpha=0.75) +
  scale_colour_manual(values=pal) + geom_vline(xintercept=2009, color="black", linetype="dashed", size=1) +
  ylab("Citable documents (x 1000)") + xlab(NULL) + 
  theme_minimal() + theme(
    legend.position = "none", 
    #legend.title=element_text(size=15, face="bold"),
    #legend.text=element_text(size=15),
    axis.title.x = element_text(size=15, face="bold"),
    axis.title.y = element_text(size=15, face="bold"))

doc

ga.plot <- grid.arrange(doc, cit, ncol=1)
ggsave(file="LA_countries2.png", ga.plot, width = 20, height = 20, units = "cm", dpi = 300)

#### AUC 1 ----
Brazil_pre <- la.countriesF %>% filter(Country %in% c("Brazil"), year<=2009)
Brazil_pos <- la.countriesF %>% filter(Country %in% c("Brazil"), year>2009)
Mexico_pre <- la.countriesF %>% filter(Country %in% c("Mexico"), year<=2009)
Mexico_pos <- la.countriesF %>% filter(Country %in% c("Mexico"), year>2009)
Chile_pre <- la.countriesF %>% filter(Country %in% c("Chile"), year<=2009)
Chile_pos <- la.countriesF %>% filter(Country %in% c("Chile"), year>2009)
Argentina_pre <- la.countriesF %>% filter(Country %in% c("Argentina"), year<=2009)
Argentina_pos <- la.countriesF %>% filter(Country %in% c("Argentina"), year>2009)
Colombia_pre <- la.countriesF %>% filter(Country %in% c("Colombia"), year<=2009)
Colombia_pos <- la.countriesF %>% filter(Country %in% c("Colombia"), year>2009)

auc_scimago <- function(data){
  res <- area_under_curve(x=data$year, y=data$Citations_per_document, method="trapezoid")
  return(res)
}

nrow(Brazil_pre)
nrow(Brazil_pos)

aucTable <- data.frame(Country = c("Brazil", "Mexico", "Chile", "Argentina", "Colombia"), 
                       AUC.2000_2009 = NA, AUC.2009_2019 = NA)

aucTable[1, 2] <- auc_scimago(Brazil_pre)
aucTable[1, 3] <- auc_scimago(Brazil_pos)
aucTable[2, 2] <- auc_scimago(Mexico_pre)
aucTable[2, 3] <- auc_scimago(Mexico_pos)
aucTable[3, 2] <- auc_scimago(Chile_pre)
aucTable[3, 3] <- auc_scimago(Chile_pos)
aucTable[4, 2] <- auc_scimago(Argentina_pre)
aucTable[4, 3] <- auc_scimago(Argentina_pos)
aucTable[5, 2] <- auc_scimago(Colombia_pre)
aucTable[5, 3] <- auc_scimago(Colombia_pos)

write.csv(aucTable, file="aucTable.csv", row.names=FALSE)

#### Brazilian Physics and Social Work citations ----
## Load and prepare data
## Source: Scimago Country Rankings (Latin America) - https://www.scimagojr.com/countryrank.php
files.ph <- list.files(path="Scimago/Physics", 
                       pattern = ".xlsx",  full.names = TRUE)
years <- 2009:2019
ph.countries <- list()
for(i in 1:length(files.ph)){
  ph.countries[[i]] <- read_excel(files.ph[i])
  ph.countries[[i]] <- ph.countries[[i]] %>% mutate(year=years[i])
}

ph.countries
ph.countriesF <- bind_rows(ph.countries)
ph.countriesF <- ph.countriesF %>% 
  rename(Citable_documents = "Citable documents", Self_citations = "Self-citations",
          Citations_per_document = "Citations per document", H_index = "H index") %>%
  mutate(Area = "Physics and Astronomy")

ph.countriesF

files.sw <- list.files(path="Scimago/SocialWork", 
                       pattern = ".xlsx",  full.names = TRUE)
years <- 2009:2019
sw.countries <- list()
for(i in 1:length(files.sw)){
  sw.countries[[i]] <- read_excel(files.sw[i])
  sw.countries[[i]] <- sw.countries[[i]] %>% mutate(year=years[i])
}

sw.countries
sw.countriesF <- bind_rows(sw.countries)
sw.countriesF <- sw.countriesF %>% 
  rename(Citable_documents = "Citable documents", Self_citations = "Self-citations",
        Citations_per_document = "Citations per document", H_index = "H index") %>%
  mutate(Area = "Social Work")

ph.countriesF
sw.countriesF

com.countries <- bind_rows(ph.countriesF, sw.countriesF)
com.countries
unique(com.countries$Area)

## Plot citations in time for Brazilian physics and social work (Fig. 4)
ph.sw <- com.countries %>% filter(Country %in% c("Brazil")) %>% 
  ggplot(aes(x=year, y=Citations_per_document, group = Area)) + 
  geom_point(aes(color=Area)) + #, size=Citable_documents 
  geom_line(aes(color=Area), size=2, alpha=0.75) +
  #geom_smooth(method="gam", se=FALSE, aes(color=Area), size=2) +
  scale_x_continuous(breaks = c(2009, 2011, 2013, 2015, 2017, 2019)) +
  ylab("Citations per document") + xlab("Year") + 
  #guides(fill=guide_legend(title="Area"), size=guide_legend(title="Citable documents")) +
  theme_minimal() + theme(
    legend.position = "bottom", 
    legend.title=element_text(size=10, face="bold"),
    legend.text=element_text(size=10),
    axis.title.x = element_text(size=15, face="bold"),
    axis.title.y = element_text(size=15, face="bold"))

ph.sw
ggsave(file="Physics_vs_SW.png", width = 20, height = 15, units = "cm", dpi = 300)
ggsave(file="Physics_vs_SW.tif", plot = ph.sw, device = "tiff", width = 20, height = 15, units = "cm", dpi = 300)

#### AUC 2 ----
auc.ph <- com.countries %>% filter(Country %in% c("Brazil")) %>% filter(Area == "Physics and Astronomy")
auc.sw <- com.countries %>% filter(Country %in% c("Brazil")) %>% filter(Area == "Social Work")

area_under_curve(x=auc.ph$year, y=auc.ph$Citations_per_document, method="trapezoid")
area_under_curve(x=auc.sw$year, y=auc.sw$Citations_per_document, method="trapezoid")
  
