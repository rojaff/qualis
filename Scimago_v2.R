
library(tidyverse)
library(readxl)
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

#### Brazilian Physics and Arts citations ----
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

files.ar <- list.files(path="Scimago/Arts", 
                       pattern = ".xlsx",  full.names = TRUE)
years <- 2009:2019
ar.countries <- list()
for(i in 1:length(files.ar)){
  ar.countries[[i]] <- read_excel(files.ar[i])
  ar.countries[[i]] <- ar.countries[[i]] %>% mutate(year=years[i])
}

ar.countries
ar.countriesF <- bind_rows(ar.countries)
ar.countriesF <- ar.countriesF %>% 
  rename(Citable_documents = "Citable documents", Self_citations = "Self-citations",
        Citations_per_document = "Citations per document", H_index = "H index") %>%
  mutate(Area = "Arts and Humanities")

ph.countriesF
ar.countriesF

com.countries <- bind_rows(ph.countriesF, ar.countriesF)
com.countries
unique(com.countries$Area)

## Plot citations in time for Brazilian physics and arts (Fig. 4)
ph.ar <- com.countries %>% filter(Country %in% c("Brazil")) %>% 
  ggplot(aes(x=year, y=Citations_per_document, group = Area)) + 
  geom_point(aes(color=Area)) + #, size=Citable_documents
  geom_line(aes(color=Area), size=2, alpha=0.7) +
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

ph.ar
ggsave(file="Physics_vs_Arts.png", width = 20, height = 15, units = "cm", dpi = 300)

