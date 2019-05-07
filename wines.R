library(tidyverse)
library(hrbrthemes)

#Authors:Aaron Brown, Daniel O'Neil

##########################
#SLURP CREATION FUNCTIONS#
##################################################################################################################################################################################################################################
##Creates the data frame for metadata. df is the original data, 
# IDvect is a vector containing the row ID's of the original data, 
# and metanamesvect is a vector that states the columns in string form from which to pull the data.

makeSIPmetaDF <- function(df, IDvect, metanamesvect) {
  xdf <- data.frame(matrix(nrow = length(metanamesvect), ncol = length(IDvect)))
  colnames(xdf) <- IDvect
  for(j in 1:length(IDvect)) {
    for(i in 1:length(metanamesvect)) {
      xdf[i,j] <- as.character(df[match(IDvect[j], df[[1]]), match(metanamesvect[i], colnames(df))])
    }
  }
  xdf$Named <- metanamesvect
  colnames(xdf)[length(IDvect)+1] <- "Meta"
  return(xdf)
}

#Example: SIPMetaDF(data, c(0001,0002,0003), c("Location", "Date")) will return a dataframe where the jobs 0001, 0002, and 0003 have location and date information.
createSLURP(price_now_updated, PriceNow_SLURP.xml, average = TRUE, meta = MetaDF)
dataframe=price_now_updated; filename="PriceNow_SLURP.xml"; average=T; meta=MetaDF
index=FALSE;provenance="";csvr=4
###
createSLURP <- function(dataframe,filename,index=FALSE,provenance="",csvr=4,average=FALSE,median=FALSE,meta=NULL) {
  #The CreateSLURP function will break the columns of a DataFrame into SIPS, by taking each column and pasting it into string.
  #The paste function will add all of the additional information to the string including necessary formatting.
  #The string will be saved to a variable to be added to the SLURP string, which has its own set of formatting.
  #The "write" function will save the string to a file in the current working directory with the name set by "filename".
  
  SIPS <- NULL      #SIPS is the variable to add all SIPS into 1 string. The format of the vector at the end is c( SIPS(1st SIP), SIPS(2nd SIP),... SIPS(n SIP) )
  metas <- NULL
  res <- ""            #res is the variable to take the vector of SIPS and create a single string from the vector of strings.
  res.meta <- ""
  if (index) start<- 2 else start<- 1          #If there is an index, we dont want to add the index to the SLURP. Start at column 2 if there is an index
  
  metadata.function <- function(i) {                 #for loop iterrating over the number of columns:1 to n without an index, 2 to n with index
    for (j in 1:nrow(meta[i])) {
      metas <- c(metas,
                 paste(" ",meta[j,length(meta)],'="',meta[j,i],'"',collapse = "",sep = ""))                          #Paste metadata (in development)
    }
    for (metadata in metas) {
      res.meta <- paste(res.meta,metadata,sep = "")
    }
    return(res.meta)}
  
  for (i in seq_along(dataframe)) {                 #for loop iterrating over the number of columns:1 to n without an index, 2 to n with index
    SIPS <- c(SIPS,
              paste( "<SIP name=",'"',colnames(dataframe[i]),'"',                                  #Paste the column name with SIP name
                     " count=",'"',length(dataframe[,i]),'"',                                      #Paste in the count of items in the column
                     " type=",'"',"CSV",'"',                                                       #Paste type with hardcoded default CSV
                     if (provenance!="") paste(" provenance=",'"',provenance,'"',sep = "")         #If Provenance is blank then skip, otherwise paste Provenance
                     else "",
                     metadata.function(i),                                                         #If there is metadata added, then use it by calling the metadata.function function
                     if (average) paste(" average=",'"',mean(dataframe[,i]),'"')             #If average is true, take mean of column otherwise skip
                     else "",
                     if (median) paste(" median=",'"',median(dataframe[,i]),'"')             #If median is true, take median of column otherwise skip
                     else "",
                     "> ",
                     paste(
                       if (is.numeric(csvr))
                         round(
                           dataframe[,i],                                                          #Paste the data from the current column
                           digits = as.numeric(csvr))                                              #Round by the CSVR argument
                       ,collapse = ",", sep = ", "),                                               #Separate the data with a comma
                     " </SIP>",                                                                    #End each string with the ending XML
                     "\n",                                                                         #At the end of the function, the write function will add each SIP to a new line with this
                     sep = "",
                     collapse = "") )
  }
  for (items in SIPS) {
    res <- paste(res,items, sep = "")
  }
  write(
    paste( "<SLURP name=",'"',deparse(substitute(dataframe)),'"',
           " provenance=",'"',provenance,'"',
           if (index==TRUE) paste(" count=",'"',ncol(dataframe)-1,'"')
           else paste(" count=",'"',ncol(dataframe),'"'),
           "> ",
           "\n",
           res,
           "</SLURP>",
           "\n",
           sep = "",
           collapse = ""),
    deparse(substitute(filename)),sep = "\n") }                                                    #Puts it all together and outputs the file in the current working directory


# Example: CreateSLURP(test.df,testdfxml21.xml,provenance = "Testing with 1000 values",csvr = 4,average = TRUE,median = FALSE,meta = MetaDF) outputs an XML file that named "testdfxml21.xml" can be read into Excel using the SIPmath tools to generate a library. It'll have the metadata included from the SIPMetaDf function.

# Data available from
# https://www.kaggle.com/zynicide/wine-reviews

wine130k <- read_csv("data/Winemag130k_Reviews/winemag-data-130k-v2.csv") %>% 
  select(-X1)

wine150k <- read_csv("data/Winemag130k_Reviews/winemag-data_first150k.csv") %>% 
  select(-X1)

wine200k <- full_join(wine130k, wine150k, na_matches = "never") %>% 
  distinct() %>% 
  filter(variety %in% c("Pinot Gris", "Gewürztraminer", "Chenin Blanc", "Riesling", "Tokay Pinot Gris"),
         points<95, price<90) %>%
  replace_na(list(designation="", title="", description="", country="")) %>% 
  mutate(grand_cru=str_detect(designation, "Grand Cru"),
         reserve=str_detect(designation, "Reserve"),
         dessert=str_detect(designation, "([Ll]ate|[Ww]inter) [Hh]arvest|Ice")|
                 str_detect(description, "[Ii]ce"),
         sgn=str_detect(designation, "Grains Nobles|SGN|[Bb]otryti|Noble"),
         qual_class = cut(points, 
                          breaks=seq(75, 100, by=5),
                          labels=c("poor", "average", "good", "excellent", "premium")),
         adj_price=price/5)  


price_now_df <- wine200k %>% 
  filter(variety=="Riesling", country=="US", 
         !dessert, !sgn, !reserve, !grand_cru,
         qual_class=="good") 

quantile(price_now_df$adj_price,probs=seq(0, 1, 0.1))
ggplot(price_now_df)+geom_histogram(aes(adj_price))+
  theme_ipsum_rc(grid_col = "gray95")+
  labs(title="Price of a bottle of dry Riesling in USA",
       subtitle="Histogram of prices",
       caption="Source: Winemag Reviews dataset, kaggle.com",
       x="Price, US$", y="Count")

# https://math.stackexchange.com/questions/2132005/percentile-of-lognormal-and-its-first-moments/2132116
set.seed(42)

n <- 10000
muPP <- log(2.75)
sigmaPP <- (log(4.0)-log(2.75))/qnorm(0.9)

PriP <- rlnorm(n, muPP, sigmaPP)
hist(PriP)

mu_s <- rnorm(n, muPP, 0.01)
sigma_s <- runif(n, sigmaPP*0.7, sigmaPP*1.3)
ys <- rlnorm(n, mu_s, sigma_s)
hist(ys)

sm::sm.density.compare(x=c(PriP, ys), 
                       group=rep(1:2, each=n), 
                       model = "equal")

#summary(PP)
#ggplot()+
#  geom_density(data=data.frame(PriP), aes(PriP), color="blue")+
#  geom_density(data=price_now_df, aes(adj_price), color="green")+
#  theme_ipsum_rc(grid_col = "gray95")+
#  labs(title="Prior")
  

library(rjags)

modelstring <- "model{
  for (i in 1:length(y)){
    y[i] ~ dlnorm(m, s^(-2))
  }
  m ~ dnorm(muPP, 0.01)
  s ~ dunif(sigmaPP*0.7, sigmaPP*1.3)
  y.new ~ dlnorm(m, s^(-2))
}"


dataList <- list(
  y=price_now_df$adj_price,
  muPP=muPP, sigmaPP=sigmaPP
  )
parameters <- c("m" , "s", "y.new")  


jagsMod <- jags.model(textConnection(modelstring), 
                      data=dataList, n.adapt = 1000, 
                      inits = list(.RNG.name = "base::Wichmann-Hill", 
                                   .RNG.seed = 100))
update( jagsMod , n.iter=1000)

codaSam <- coda.samples(model=jagsMod, variable.names = parameters,
             n.iter = n)
plot(codaSam)
mcmcChain <- as.matrix( codaSam )

price_now_updated <- tibble(Prior_predictive =PriP,
                            Data_resampled =sample(price_now_df$adj_price, n, replace = TRUE),
                            Posterior_predictive = mcmcChain[,"y.new"])

gather(price_now_updated, key, value) %>% 
  ggplot()+
  geom_density(aes(x=value, color=key), size=1.1)+
  theme_ipsum_rc(grid_col = "gray95")+
  labs(title="Price of a bottle of dry Riesling in USA",
       subtitle="Density function",
       x="Price, US$", y=NULL, color=NULL)

write_csv(price_now_updated, "price_now_data_updated.csv")

MetaDF <- gather(price_now_updated, key, value) %>% 
  group_by(key) %>% 
  summarise(n_obs = n()) %>%
  makeSIPmetaDF(.$key, "n_obs")

createSLURP(as.data.frame(price_now_updated), PriceNow_SLURP.xml, average = TRUE, meta = MetaDF)
       