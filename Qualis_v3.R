
library(tidyverse)
library(broom)

### Load and prepare QUALIS adata ----
## Data downloaded from: https://sucupira.capes.gov.br/sucupira/public/consultas/coleta/veiculoPublicacaoQualis/listaConsultaGeralPeriodicos.jsf
## All tables where saved as .csv
files <- list.files("QUALIS/", 
                    pattern = "classificacoes",  full.names = TRUE)
length(files)

capes <- list()
for(i in c(4,5,10,12,13,20,23,34,35,37,44,45,48)){
  capes[[i]] <- read.csv(files[[i]], sep="\t") # some files are separated by tabs
}

for(i in setdiff(c(1:49), c(4,5,10,12,13,20,23,34,35,37,44,45,48))){
  capes[[i]] <- read.csv(files[[i]], sep=",") # other files are separated by commas
}

### Check
for(i in 1: length(capes)){
  print(i)
  print(head(capes[[i]]))
} #OK
  
  ### Area names
  dnames <- NA
  files2 <- str_split(files, pattern = "classificacoes_publicadas_", n=2)
  for(i in 1:length(files2)){
    dnames[i] <- files2[[i]][2]
  }
  
  dnamesF <- NA
  files3 <- str_split(dnames, pattern = "_2017", n=2)
  for(i in 1:length(files3)){
    dnamesF[i] <- files3[[i]][1]
  }
  
  dnamesF
  
  ### Prepare datasets
  capes2 <- list()
  for(k in 1:length(capes)){
    capes2[[k]] <- capes[[k]] %>% 
      mutate(ISSN = as.factor(str_replace(ISSN, "-", "")),
             Area = dnamesF[k])
  }
  
  ### Bind dataframes and rename Area
  capesF <- bind_rows(capes2)
  capesF <- capesF %>% mutate(Area = fct_recode(Area, admin_pub_emp_cien_cont_tur = "administracao_publica_e_de_empresas_ciencias_contabeis_e_turismo"))
  head(capesF)
  length(unique(capesF$ISSN)) ## Total number of journals in QUALIS

### Load and prepare Scopus data ----
## Data downloaded from: https://www.researchgate.net/publication/330967992_List_of_Scopus_Index_Journals_February_2019_New
scopus <- read.csv("ListofScopusIndexJournalsFebruary2019.csv", sep=",")
scopus <- scopus[, -c(1:2)]
head(scopus)
summary(scopus$X2017.CiteScore)
length(unique(scopus$Print.ISSN)) ## Total number of journals in the Scopus database

#### Proportion of journals by category ----
#### Get QUALIS journals with CiteScore
capes.scopus <- capesF %>% inner_join(scopus, by = c("ISSN" = "Print.ISSN")) %>% na.omit()
length(unique(capes.scopus$ISSN)) ## Total number of QUALIS journals with a Scopus CiteScore 2017

nested <- capes.scopus %>% group_by(Area) %>% nest() %>%
  mutate(value = map(data, ~length(unique(.x$ISSN))),
         group = "Indexed with CiteScore") %>% unnest(value)
  
nested %>% arrange((value)) # Number of QUALIS journals with a Scopus CiteScore in each subject Area

## Get QUALIS journals indexed by Scopus but without CiteScore (NA)
capes.scopus.NA <- capesF %>% inner_join(scopus, by = c("ISSN" = "Print.ISSN")) %>%
  filter(is.na(X2017.CiteScore)==TRUE)

nested.NA <- capes.scopus.NA %>% group_by(Area) %>% nest() %>%
  mutate(group = "Indexed without CiteScore")

nrow(nested.NA) # one Area missing
nested.NA$data[[1]]

for(i in 1:nrow(nested.NA)){
  df <- nested.NA$data[[i]] #%>% filter(Estrato == "A1") 
  res <- length(unique(df$ISSN))
  nested.NA[i, "value"] <- res
}

nested.NA %>% arrange((value))

## Get QUALIS journals that are not indexed by Scopus
capes.scopus.anti <- capesF %>% anti_join(scopus, by = c("ISSN" = "Print.ISSN")) 

length(unique(capesF$ISSN)) ## Total number of journals in QUALIS
length(unique(capes.scopus.anti$ISSN)) ## Total number of QUALIS journals not indexed by Scopus
21541*100/27619
all.anti <- capes.scopus.anti %>% filter(!duplicated(ISSN))
write.csv(all.anti, file="NotIndexed_journals.csv", row.names=FALSE)

nested.anti <- capes.scopus.anti %>% group_by(Area) %>% nest() %>%
  mutate(group = "Not indexed")
nrow(nested.anti)
nested.anti$data[[1]]

for(i in 1:nrow(nested.anti)){
  df <- nested.anti$data[[i]] 
  res <- length(unique(df$ISSN)) 
  nested.anti[i, "value"] <- res
}

nested.anti %>% arrange(desc(value))

## Calculate the proportion Indexed journals per subject Area
capes.scopus.all <- capesF %>% inner_join(scopus, by = c("ISSN" = "Print.ISSN")) # %>% na.omit()
nested.all <- capes.scopus.all %>% group_by(Area) %>% nest() %>%
  mutate(group = "Indexed")

for(i in 1:nrow(nested.all)){
  df <- nested.all$data[[i]] 
  res <- length(unique(df$ISSN)) 
  nested.all[i, "value"] <- res
}

nested.all

com.all <- bind_rows(nested.all, nested.anti) %>% dplyr::select(-data) %>%
  mutate(group = as.factor(group))

indexed <- com.all  %>%
  group_by(Area, group) %>%
  summarise(n = sum(value)) %>%
  mutate(percentage = n / sum(n)) %>% 
  dplyr::filter(group=="Indexed")

indexed %>% arrange(desc(percentage))

#### Plot proportion of journals by category (Fig. 2)
## Bind dataframes and add an entry for missing Area ciencias_da_religiao_e_teologia
com.N <- bind_rows(nested, nested.NA, nested.anti) %>% dplyr::select(-data) %>%
  mutate(group = as.factor(group)) %>%
  bind_rows(tibble(Area = "ciencias_da_religiao_e_teologia", value=0, group="Indexed without CiteScore"))
#com.N %>% filter(Area=="ciencias_da_religiao_e_teologia")

com.N %>% mutate(group = factor(group, levels = c("Not indexed", "Indexed with CiteScore", "Indexed without CiteScore"))) %>%
  rename(Type = "group") %>%
  ggplot(aes(x=fct_reorder(Area, (value)), y=value)) + 
  geom_area(aes(colour = Type, group=Type, fill = Type)) + 
  ylab("Number of journals") + xlab("Area") + 
  theme_minimal() + theme(
    legend.position = "top", 
    legend.title=element_text(size=10, face="bold"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title.x = element_text(size=15, face="bold"),
    axis.title.y = element_text(size=15, face="bold"))

ggsave("Area.png", width = 20, height = 15, units = "cm", dpi = 300)

## Calculate percentage by Area
com.N2 <- com.N  %>%
  group_by(Area, group) %>%
  summarise(n = sum(value)) %>%
  mutate(percentage = n / sum(n))

## Area plot (not shown in preprint)
com.N2 %>% 
  mutate(group = factor(group, levels = c("Not indexed", "Indexed with CiteScore", "Indexed without CiteScore"))) %>%
  rename(Type = "group") %>%
  ggplot(aes(x=fct_reorder(Area, (percentage)), y=percentage)) + 
  geom_area(aes(colour = Type, group=Type, fill = Type)) + 
  ylab("Number of journals") + xlab("Area") + 
  theme_minimal() + theme(
    legend.position = "top", 
    legend.title=element_text(size=10, face="bold"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title.x = element_text(size=15, face="bold"),
    axis.title.y = element_text(size=15, face="bold"))

ggsave("Area.png", width = 20, height = 15, units = "cm", dpi = 300)

#### CiteScore Analyses ----
### Indicators
## KW anova
kw.res <- nested %>%
  mutate(kw = map(data, ~kruskal.test(formula = X2017.CiteScore ~ Estrato, data = .x))) %>%
  mutate(res = map(kw, ~tidy(.x))) %>%
  unnest(res)

kw.res %>% arrange(desc(statistic))# %>% dplyr::select(Area)
kw.res %>% filter(p.value>=0.05)

## N A1 journals with IF below median
median.res <- kw.res %>% 
  mutate(median = map(data, ~median(.x$X2017.CiteScore))) %>% unnest(median) %>%
  mutate(Nmedian=as.numeric(NA))

for(i in 1:nrow(median.res)){
  df <- median.res$data[[i]] %>% filter(X2017.CiteScore <= median.res$median[i]) %>%
    filter(Estrato=="A1")
  median.res[i, "Nmedian"] <- nrow(df)
}

median.res %>% arrange((Nmedian)) #%>% dplyr::select(Area)

## Increasing IF in lower qualis categories
median.comp <- function(i){
Mtable <- median.res$data[[i]] %>% group_by(Estrato) %>% summarize(median = median(X2017.CiteScore), n = n())
rows <- nrow(Mtable)

if(rows==1){
  RES <- NA
} else if(rows==2){
  a <- Mtable[1,"median"] > Mtable[2,"median"]
  comp <- c(a)
  RES <- length(comp[comp==FALSE])
} else if(rows==3){
  a <- Mtable[1,"median"] > Mtable[2,"median"]
  b <- Mtable[2,"median"] > Mtable[3,"median"]
  comp <- c(a,b)
  RES <- length(comp[comp==FALSE])
} else if (rows==4){
  a <- Mtable[1,"median"] > Mtable[2,"median"]
  b <- Mtable[2,"median"] > Mtable[3,"median"]
  c <- Mtable[3,"median"] > Mtable[4,"median"]
  comp <- c(a,b, c)
  RES <- length(comp[comp==FALSE])
}else if (rows==5){
  a <- Mtable[1,"median"] > Mtable[2,"median"]
  b <- Mtable[2,"median"] > Mtable[3,"median"]
  c <- Mtable[3,"median"] > Mtable[4,"median"]
  d <- Mtable[4,"median"] > Mtable[5,"median"]
  comp <- c(a,b, c,d)
  RES <- length(comp[comp==FALSE])
}else if (rows==6){
  a <- Mtable[1,"median"] > Mtable[2,"median"]
  b <- Mtable[2,"median"] > Mtable[3,"median"]
  c <- Mtable[3,"median"] > Mtable[4,"median"]
  d <- Mtable[4,"median"] > Mtable[5,"median"]
  e <- Mtable[5,"median"] > Mtable[6,"median"]
  comp <- c(a,b, c,d,e)
  RES <- length(comp[comp==FALSE])
}else if (rows==7){
  a <- Mtable[1,"median"] > Mtable[2,"median"]
  b <- Mtable[2,"median"] > Mtable[3,"median"]
  c <- Mtable[3,"median"] > Mtable[4,"median"]
  d <- Mtable[4,"median"] > Mtable[5,"median"]
  e <- Mtable[5,"median"] > Mtable[6,"median"]
  f <- Mtable[6,"median"] > Mtable[7,"median"]
  comp <- c(a,b, c,d,e,f)
  RES <- length(comp[comp==FALSE])
}else if (rows==8){
  a <- Mtable[1,"median"] > Mtable[2,"median"]
  b <- Mtable[2,"median"] > Mtable[3,"median"]
  c <- Mtable[3,"median"] > Mtable[4,"median"]
  d <- Mtable[4,"median"] > Mtable[5,"median"]
  e <- Mtable[5,"median"] > Mtable[6,"median"]
  f <- Mtable[6,"median"] > Mtable[7,"median"]
  g <- Mtable[7,"median"] > Mtable[8,"median"]
  comp <- c(a,b, c,d,e,f,g)
  RES <- length(comp[comp==FALSE])
}else {
  RES <- "error"
}
return(RES)
}

COMP <- list()
for(i in 1:nrow(median.res)) {
  COMP[[i]] <- median.comp(i)
}

median.res$median.comp <- unlist(COMP)
median.res %>% arrange((median.comp)) #%>% dplyr::select(Area)

#### Results ----
unique(com.N$group)
CiteScore <- com.N %>% mutate(Area = as.factor(Area)) %>% filter(group=="Indexed with CiteScore")
NoCiteScore <- com.N %>% mutate(Area = as.factor(Area)) %>% filter(group=="Indexed without CiteScore")
NotIndexed <- com.N %>% mutate(Area = as.factor(Area)) %>% filter(group=="Not indexed")

identical(results$Area, CiteScore$Area)
identical(results$Area, indexed$Area)

results <- median.res
results$PIndexed <- indexed$percentage
results$CiteScore <- CiteScore$value
results$NoCiteScore <- NoCiteScore$value
results$NotIndexed <- NotIndexed$value

resultsF <- results %>% dplyr::select(Area, NotIndexed, NoCiteScore, CiteScore, PIndexed, statistic, p.value, median.comp, Nmedian)
resultsF %>% arrange((NoCiteScore))

resultsF %>% 
  arrange(statistic, median.comp, Nmedian) %>%
  write.csv(file = "Results.csv", row.names=FALSE)

#### Table 1
quantile(resultsF$PIndexed, probs = seq(0, 1, 0.05))[2]
quantile(resultsF$PIndexed, probs = seq(0, 1, 0.05))[20]

resultsF %>% filter(PIndexed<=quantile(resultsF$PIndexed, probs = seq(0, 1, 0.05))[2]) %>% select(Area)
resultsF %>% filter(PIndexed>=quantile(resultsF$PIndexed, probs = seq(0, 1, 0.05))[20]) %>% select(Area)

resultsF %>% filter(statistic<=quantile(resultsF$statistic, probs = seq(0, 1, 0.05))[2]) %>% select(Area)
resultsF %>% filter(statistic>=quantile(resultsF$statistic, probs = seq(0, 1, 0.05))[20]) %>% select(Area)

resultsF %>% filter(median.comp<quantile(resultsF$median.comp, probs = seq(0, 1, 0.05))[2]) %>% select(Area) #%>% print(n=23)
resultsF %>% filter(median.comp>=quantile(resultsF$median.comp, probs = seq(0, 1, 0.05))[20]) %>% select(Area)

resultsF %>% filter(Nmedian<=quantile(resultsF$Nmedian, probs = seq(0, 1, 0.05))[2]) %>% select(Area)
resultsF %>% filter(Nmedian>=quantile(resultsF$Nmedian, probs = seq(0, 1, 0.05))[20]) %>% select(Area)

#### Plots ----
#### Supplementary Plots (Figs. S1-S4)
names(resultsF)
# median.res2 <- median.res %>% dplyr::select(Area, statistic, Nmedian, median.comp) %>% 
#   ungroup(Area) %>%
#   mutate(Area = fct_reorder(Area, statistic))
                          
Pkw <- resultsF %>% #ggplot(aes(x=Area, y=statistic, group = 1)) + 
  ggplot(aes(x=fct_reorder(Area, (statistic)), y=statistic, group = 1)) +
  geom_line(size=2) + #geom_point(aes(size=NCiteScore)) +
  ylab("Kruskal-Wallis chi-squared") + xlab("Area") + #ylim(0,15) +
  theme_minimal() + theme(
    legend.position = "top", 
    legend.title=element_text(size=10, face="bold"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title.x = element_text(size=15, face="bold"),
    axis.title.y = element_text(size=15, face="bold"))

Pmcomp <- resultsF %>% 
  ggplot(aes(x=fct_reorder(Area, desc(median.comp)), y=median.comp, group = 1)) +
  geom_line(size=2) + #geom_point(aes(size=NCiteScore)) +
  ylab("Cases where lower QUALIS > higher QUALIS") + xlab("Area") + #ylim(0,15) +
  theme_minimal() + theme(
    legend.position = "top", 
    legend.title=element_text(size=10, face="bold"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title.x = element_text(size=15, face="bold"),
    axis.title.y = element_text(size=15, face="bold"))

Pnmed <- resultsF %>% 
  ggplot(aes(x=fct_reorder(Area, desc(Nmedian)), y=Nmedian, group = 1)) +
  geom_line(size=2) + #geom_point(aes(size=NCiteScore)) +
  ylab("A1 journals with CiteScore below the area median") + xlab("Area") + #ylim(0,15) +
  theme_minimal() + theme(
    legend.position = "top", 
    legend.title=element_text(size=10, face="bold"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title.x = element_text(size=15, face="bold"),
    axis.title.y = element_text(size=14, face="bold"))

Pindex <- resultsF %>% 
  ggplot(aes(x=fct_reorder(Area, (PIndexed)), y=PIndexed, group = 1)) +
  geom_line(size=2) + #geom_point(aes(size=NCiteScore)) +
  ylab("Proportion of journals indexed by Scopus") + xlab("Area") + #ylim(0,15) +
  theme_minimal() + theme(
    legend.position = "top", 
    legend.title=element_text(size=10, face="bold"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title.x = element_text(size=15, face="bold"),
    axis.title.y = element_text(size=14, face="bold"))

Pkw
Pmcomp
Pnmed
Pindex

ggsave("Pindex.png", width = 20, height = 20, units = "cm", dpi = 300)
# tiff(filename = "Pmcomp.tif",
#      width = 15, height = 15, units = "cm",
#      compression = "lzw", bg = "white", res = 300)
# Pmcomp
# dev.off()

# library(gridExtra)
# grid.arrange(Pkw, Pmcomp, Pnmed, ncol=1)

#### General plot of CiteScore by class per subject Area (Fig. 3)
Qplot <- median.res %>% unnest(data) %>%
  ggplot(aes(y=X2017.CiteScore, x=Estrato)) + 
  geom_boxplot() + #outlier.shape=NA
  #coord_cartesian(ylim=c(0, upper.limit)) +
  #coord_cartesian(ylim = quantile(kw.res$data[[i]]$X2017.CiteScore, c(0.01, 0.99))) +
  ylab("Scopus CiteScore (2017)") + xlab("QUALIS Category (2013-2016)") + #ylim(0,15) +
  facet_wrap(~ Area,  scales = "free") +
  theme_minimal() + theme(
    strip.text.x = element_text(size = 4, face="bold"),
    axis.title.x = element_text(size=20, face="bold"),
    axis.title.y = element_text(size=20, face="bold"))

Qplot
ggsave("Qplot.png", width = 30, height = 30, units = "cm", dpi = 300)


