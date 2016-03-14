# Alcohol duty analysis

library(ggplot2)

# Downloading databases ---------------------------------------------------
url <- numeric()

# Alcoholic beverage tax revenue as a per cent of government revenue 
# (country and year)
url[1] <- "http://apps.who.int/gho/athena/data/data-text.csv?target=GHO/SA_0000001474&profile=text&filter=COUNTRY:*;REGION:EUR"

# Alcohol expenditure as a per cent of total household expenditure
# (country and year)
url[2] <- "http://apps.who.int/gho/athena/data/data-text.csv?target=GHO/SA_0000001476&profile=text&filter=COUNTRY:*;REGION:EUR"

# Social costs of alcohol use
# (country and year) - limited availability 
url[3] <- "http://apps.who.int/gho/athena/data/data-text.csv?target=GHO/SA_0000001477&profile=text&filter=COUNTRY:*;REGION:EUR;SOCIALCOSTTYPE:*"

# Road traffic crashes involving alcohol
# Data by country and year
url[4] <- "http://apps.who.int/gho/athena/data/data-text.csv?target=GHO/SA_0000001471&profile=text&filter=COUNTRY:*;REGION:EUR"

# Total consumption
# Data by country, period and sex
url[5] <- "http://apps.who.int/gho/athena/data/data-text.csv?target=GHO/SA_0000001403&profile=text&filter=COUNTRY:*;REGION:EUR;SEX:*"

# Consumption of pure alcohol by type of beverage
# Data by country and type
url[6] <- "http://apps.who.int/gho/athena/data/data-text.csv?target=GHO/SA_0000001398&profile=text&filter=COUNTRY:*;REGION:EUR;ALCOHOLTYPE:*"

# Excise tax for 1L of pure alcohol
# Data by country in 2008
url[7] <- "http://apps.who.int/gho/athena/data/data-text.csv?target=GHO/SA_0000001548&profile=text&filter=COUNTRY:*"

# Binomial Excise Tax on Alcoholic Beverages
url[8] <- "http://apps.who.int/gho/athena/data/data-text.csv?target=GHO/SA_0000001550&profile=text&filter=COUNTRY:*;ALCOHOLTYPE:*"


# Download and read files
file_name <- c("tax_rev_gov", "houshold_exp", "social_costs",
               "traffic_crashes","consum_tot", "consum_type", "excise_tax",
               "tax_type")
databases <- list()
for (i in 1:length(file_name)){
  download.file(url[i], file_name[i])
  databases[[i]] <- read.table(file = file_name[i], header = TRUE, sep = ",", 
                               stringsAsFactors = FALSE, na.strings = FALSE)
}

# Average annual wages
# Data by country, type of currency and year
databases[[length(file_name)+1]] <- read.table(file = "average_annual_wages.csv", header = TRUE, sep = ",", 
                                               stringsAsFactors = FALSE, na.strings = FALSE)

names(databases) <- c(file_name, "annual_wages")
country_code <- cbind(unique(databases$annual_wages[,1]),unique(databases$annual_wages[,2]))
unite_code <- cbind(unique(databases$annual_wages$Unit.Code), unique(databases$annual_wages$Unit))
lapply(databases, head)


# DESCRIPTIVE ANALYSIS ----------------------------------------------------


# Alcoholic beverage tax revenue as a percent of government revenue -------
databases$tax_rev_gov <- databases$tax_rev_gov[, c("Year", "Country", "Numeric")]

# Available years
years <- sort(unique(databases$tax_rev_gov$Year))

# Available countries according to the year of choice
year <- 2007
countries <- databases$tax_rev_gov[databases$tax_rev_gov$Year == year, "Country"]

# Database 
data01 <- databases$tax_rev_gov[databases$tax_rev_gov$Year == year,
                                c("Country", "Numeric")]
    # loop to avoid a "big country name" 
br <- 0; i <- 1
while( br == 0){
  if (data01[i,"Country"] == "United Kingdom of Great Britain and Northern Ireland"){
    data01[i, "Country"] <- "UK"
    br <- 5
  }
  i <- i + 1
}
names(data01) <- c("Country", "Percentage_Gov_Rev")

# Data Visualization
data01 <- data01[order(-data01[ ,"Percentage_Gov_Rev"]), ]
pos <- data01$Country
title <- paste("Alcoholic Beverage Tax Revenue as a Percentage of Government Revenue at",
               year)
ggplot(data = data01, aes(y = Percentage_Gov_Rev, x = Country)) + 
  geom_bar(stat = "identity", colour = "black", fill = "#333333") + 
  scale_x_discrete(limits = pos) +
  theme_light() + 
  xlab("Country") + ylab("Percentage of Gov. Revenue") +
  ggtitle(title)

mean(data01$Percentage_Gov_Rev)
sd(data01$Percentage_Gov_Rev)

# Alcohol expenditure as a per cent of total household expenditure --------
databases$houshold_exp <- databases$houshold_exp[, c("Year", "Country", "Numeric")]

# Available years 
years2 <- sort(unique(databases$houshold_exp$Year))

# Available countries according to the year of choice
year2 <- 2006
countries2 <- databases$houshold_exp[databases$houshold_exp$Year == year2, "Country"]

# Database
data02 <- databases$houshold_exp[databases$houshold_exp$Year == year2, c("Country","Numeric")]
    # loop to avoid a "big country name" 
for(i in 1:nrow(data02)){
  if (data02[i,"Country"] == "United Kingdom of Great Britain and Northern Ireland"){
    data02[i, "Country"] <- "UK"
  }
  if (data02[i,"Country"] == "Russian Federation"){
    data02[i, "Country"] <- "Russia"
  }
  if (data02[i,"Country"] == "The former Yugoslav republic of Macedonia"){
    data02[i, "Country"] <- "Macedonia"
  }
}

mean(data02$Numeric)
sd(data02$Numeric )

# Data Visualization
data02 <- data02[order(-data02[ ,"Numeric"]), ]
pos <- data02$Country
title <- paste("Alcohol Expenditure as a Percentage of Total Household Expenditure in",
               year2)
ggplot(data = data02, aes(y = Numeric, x = Country)) + 
  geom_bar(stat = "identity", colour = "black", fill = "#333333") + 
  scale_x_discrete(limits = pos) +
  theme_light() + 
  xlab("Country") + ylab("Percentage of Alcohol Expenditure") +
  ggtitle(title)

# Social costs of alcohol use ---------------------------------------------
# Data by country and year, limitd availability
# in millions of USD
# includes: direct law costs, direct health costs, direct other costs, inderect costs
data03 <- databases$social_costs[databases$social_costs$SUBSTANCE_ABUSE_SOCIAL_COST_TYPES == "Total costs", c("Year", "Country", "Numeric")]

# Database
    # loop to avoid a "big country name" 
for(i in 1:nrow(data03)){
  if (data03[i,"Country"] == "United Kingdom of Great Britain and Northern Ireland"){
    data03[i, "Country"] <- "UK"
  }
  if (data03[i,"Country"] == "Russian Federation"){
    data03[i, "Country"] <- "Russia"
  }
  if (data03[i,"Country"] == "The former Yugoslav republic of Macedonia"){
    data03[i, "Country"] <- "Macedonia"
  }
}

# Data Visualization
data03 <- data03[order(-data03[ ,"Numeric"]), ]
pos <- data03$Country
title <- paste("Social Costs of Alcohol Use per Country")
ggplot(data = data03, aes(y = Numeric, x = Country)) + 
  geom_bar(stat = "identity", colour = "black", fill = "#333333") + 
  scale_x_discrete(limits = pos) +
  theme_light() + 
  xlab("Country") + ylab("Total Costs (in millions of USD)") +
  ggtitle(title)

mean(data03$Numeric)
sd(data03$Numeric)

# Road traffic crashes involving alcohol ----------------------------------
databases$traffic_crashes <- databases$traffic_crashes[, c("Year", "Country", "Numeric")]

# Available years
years <- sort(unique(databases$tax_rev_gov$Year))

# Available countries according to the year of choice
year <- 2007
countries <- databases$traffic_crashes[databases$traffic_crashes$Year == year, "Country"]

# Database 
data04 <- databases$traffic_crashes[databases$traffic_crashes$Year == year,
                                c("Country", "Numeric")]
    # loop to avoid a "big country name" 
for(i in 1:nrow(data04)){
  if (data04[i,"Country"] == "United Kingdom of Great Britain and Northern Ireland"){
    data04[i, "Country"] <- "UK"
  }
  if (data04[i,"Country"] == "Russian Federation"){
    data04[i, "Country"] <- "Russia"
  }
  if (data04[i,"Country"] == "The former Yugoslav republic of Macedonia"){
    data04[i, "Country"] <- "Macedonia"
  }
}

# Data Visualization
data04 <- data04[order(-data04[ ,"Numeric"]), ]
pos <- data04$Country
title <- paste("Road Traffic Crashes Involving Alcohol as a percentage in",
               year)
ggplot(data = data04, aes(y = Numeric, x = Country)) + 
  geom_bar(stat = "identity", colour = "black", fill = "#333333") + 
  scale_x_discrete(limits = pos) +
  theme_light() + 
  xlab("Country") + ylab("Percentage from Total Crashes") +
  ggtitle(title)

# Total consumption -------------------------------------------------------
# Three year average 2008 - 2010
selected <- c("Country", "Sex", "Numeric")
databases$consum_tot <- databases$consum_tot[databases$consum_tot$Year == "2008-2010", selected]

data05 <- databases$consum_tot

for(i in 1:nrow(data05)){
  if (data05[i,"Country"] == "United Kingdom of Great Britain and Northern Ireland"){
    data05[i, "Country"] <- "UK"
  }
  if (data05[i,"Country"] == "Russian Federation"){
    data05[i, "Country"] <- "Russia"
  }
  if (data05[i,"Country"] == "The former Yugoslav republic of Macedonia"){
    data05[i, "Country"] <- "Macedonia"
  }
  if (data05[i,"Country"] == "Republic of Moldova"){
    data05[i, "Country"] <- "Moldova"
  }
}
countr_un <- unique(data05$Country)
sele_coun <- c("UK", "France", "Finland", "Denmark", "Austria", "Sweden", "Germany", "Russia", "Italy")

for (i in 1:length(sele_coun)){
  if (i == 1)
    data051 <- data05[data05$Country == sele_coun[i], ]
  else
    data051 <- rbind(data051, data05[data05$Country == sele_coun[i], ])
}



datase <- data051[data05$Sex != "Both sexes",  ]
databs <- data051[data05$Sex == "Both sexes",  ]

databs <- databs[order(-databs[ ,"Numeric"],na.last = NA), ]
pos <- databs$Country

ggplot(data = databs, aes(x = Country, y = Numeric)) + 
  geom_bar(stat = "identity", colour = "black", fill = "#333333") + 
  scale_x_discrete(limits = pos) +
  theme_light() + 
  xlab("Country") + ylab("Consumption (L)") +
  ggtitle("(2008 - 2010) Average Alcohol Consumption per Country")

mean(databs$Numeric)
sd(databs$Numeric)

aux <- datase[datase$Sex=="Male" ,c("Numeric","Country")]
pos <- aux[order(-aux[,1], na.last = NA),2]
datase <- datase[order(datase$Sex, na.last = NA, decreasing = TRUE),]

ggplot(data=datase, aes(x = Country, y = Numeric, fill = Sex)) +
  geom_bar(stat = "identity", position="dodge", colour = "black") +
  scale_x_discrete(limits = pos) +
  scale_fill_manual(values = c("#777777","#333333")) +
  xlab("Country") + ylab("Consumption (L)") +
  ggtitle("(2008 - 2010) Average Alcohol Consumption by Sex and Country") + 
  theme_minimal()

mean(datase[datase$Sex=="Male", "Numeric"])
sd(datase[datase$Sex=="Male", "Numeric"])
mean(datase[datase$Sex=="Female", "Numeric"])
sd(datase[datase$Sex=="Female", "Numeric"])

# Consumption of pure alcohol by type of beverage in 2010 -----------------

databases$consum_type <- databases$consum_type[, c("Country", "Beverage.Types", "Numeric")]
data06 <- databases$consum_type
for(i in 1:nrow(data06)){
  if (data06[i,"Country"] == "United Kingdom of Great Britain and Northern Ireland"){
    data06[i, "Country"] <- "UK"
  }
  if (data06[i,"Country"] == "Russian Federation"){
    data06[i, "Country"] <- "Russia"
  }
  if (data06[i,"Country"] == "The former Yugoslav republic of Macedonia"){
    data06[i, "Country"] <- "Macedonia"
  }
  if (data06[i,"Country"] == "Republic of Moldova"){
    data06[i, "Country"] <- "Moldova"
  }
  if (data06[i,"Beverage.Types"] == "Other alcoholic beverages"){
    data06[i, "Beverage.Types"] <- "Other"
  }
}

    # function provided by Erwan Le Pennec 
coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") 
    "y"
  else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

# general radial plot
pal <- replicate(length(unique(data06$Country)) ,"#555555")
ggplot(data = data06, aes(x  = Beverage.Types, y = Numeric) ) + 
  geom_polygon(aes(group = Country, color = Country), fill = NA, show.legend = FALSE) +
  geom_line(aes(group = Country, color = Country)) +
  scale_color_manual(values = pal) +
  ggtitle("General Consumption of Alcohol by Type of Beverage in 2010") +
  theme_minimal() +
  coord_radar() +
  xlab("") + ylab("") +
  theme(strip.text.x = element_text(size = rel(0.8)),
        axis.text.x = element_text(size = rel(0.8)),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position="none")

length(unique(data06[is.na(data06$Numeric) == FALSE, "Country"]))
m.wine <- mean(data06[data06$Beverage.Types=="Wine", "Numeric"], na.rm = TRUE)
m.beer <- mean(data06[data06$Beverage.Types=="Beer", "Numeric"], na.rm = TRUE)
m.spir <- mean(data06[data06$Beverage.Types=="Spirits", "Numeric"], na.rm = TRUE)
m.oth <- mean(data06[data06$Beverage.Types=="Other", "Numeric"], na.rm = TRUE)
c(m.wine,m.beer,m.spir,m.oth)
sum(m.wine,m.beer,m.spir,m.oth)


# Select data of interest
country_choice <- c("UK", "France", "Germany", "Denmark","Russia")
for (i in 1:length(country_choice)){
  if (i == 1)
    data061 <- data06[data06$Country == country_choice[i], ]
  else
    data061 <- rbind(data061, data06[data06$Country == country_choice[i], ])
}

ty <- c("Wine", "Spirits", "Beer", "Other")
for( i in 1:length(ty)){
  av <- mean(data06[data06$Beverage.Types == ty[i], c("Numeric")], na.rm = TRUE)
  data061 <- rbind(data061, c("Average", ty[i], round(av,1)))
}
data061 <- data061[order(data061$Beverage.Types),]

# selected radial plot
pal <- c("#ff2106", "#1c444b", "#538db8", "#73c382", "#b4801d", "#964712","#b70d0e")
ggplot(data = data061, aes(x  = Beverage.Types, y = Numeric) ) + 
  geom_polygon(aes(group = Country, color = Country), fill = NA, show.legend = FALSE, size = 1.1, alpha = 0.5) +
  geom_line(aes(group = Country, color = Country), size = 1.1, alpha = 0.5) +
  guides(color = guide_legend(ncol=1)) +
  theme_minimal() +
  coord_radar() +
  xlab("") + ylab("") +
  ggtitle("Consumption of Alcohol by Type of Beverage in 2010") +
  scale_color_manual(values = pal[1:6]) +
  theme(strip.text.x = element_text(size = rel(0.8)),
        axis.text.x = element_text(size = rel(0.8)),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

# Average annual wages ----------------------------------------------------
# USD
data07 <- databases$annual_wages[databases$annual_wages$Unit.Code == "USD", c("Country","Series", "Time", "Value")]
data07 <- data07[data07$Series == "In 2014 constant prices at 2014 USD exchange rates", c("Country", "Time", "Value")]

for(i in 1:nrow(data07)){
  if (data07[i,"Country"] == "United Kingdom"){
    data07[i, "Country"] <- "UK"
  }
  if (data07[i,"Country"] == "United States"){
    data07[i, "Country"] <- "US"
  }
}

country_unique <- unique(data07$Country)
select_countr <- c("Germany", "Italy", "Netherlands", "UK",
                   "Denmark", "US", "Mexico", "France","Switzerland")

for (i in 1:length(select_countr)){
  if (i == 1)
    data071 <- data07[data07$Country == select_countr[i], ]
  else
    data071 <- rbind(data071, data07[data07$Country == select_countr[i], ])
}



pal <- c("#87c7de", "#3d87d3", "#1b51a6", "#1d136a", "#0d0548", "#0f0f0f", "#560000","#810000", "#8d0000")
aux <- data071[data071$Time == 2014, c("Value", "Country")]
a <- aux[order(-aux[,1]), ]
a <- cbind(a, pal)
b <- a[order(a[,2]), ]

ggplot(data071, aes(x = Time, y = Value, group = Country)) + 
  #geom_line(aes(colour = Country), size = 1) +
  geom_point(aes(colour = Country), size = 2) +
  scale_color_manual(values = as.character(b[,3])) +
  theme_light() + 
  ggtitle("Average Annual Wages per Country") +
  xlab("Years") + ylab("USD") +
  theme(axis.ticks.x = element_line())

# Excise tax for 1L of pure alcohol ---------------------------------------
# 2008

selecteds <- c("Country", "Display.Value")
databases$excise_tax <- databases$excise_tax[ , selecteds]
data08 <- databases$excise_tax

# loop to avoid a "big country name" 
for(i in 1:nrow(data08)){
  if (data08[i,"Country"] == "United Kingdom of Great Britain and Northern Ireland"){
    data08[i, "Country"] <- "UK"
  }
  if (data08[i,"Country"] == "Russian Federation"){
    data08[i, "Country"] <- "Russia"
  }
  if (data08[i,"Country"] == "The former Yugoslav republic of Macedonia"){
    data08[i, "Country"] <- "Macedonia"
  }
  if (data08[i,"Country"] == "Democratic Republic of the Congo"){
    data08[i, "Country"] <- "Congo"
  }
  if (data08[i,"Country"] == "United Republic of Tanzania"){
    data08[i, "Country"] <- "Tanzania"
  }
  if (data08[i,"Country"] == "Venezuela (Bolivarian Republic of)"){
    data08[i, "Country"] <- "Venezuela"
  }
  if (data08[i,"Country"] == "Micronesia (Federated States of)"){
    data08[i, "Country"] <- "Micronesia"
  }
}

countries_select <- c("UK", "France", "Finland", "Denmark", "Estonia",
                      "Iceland", "Sweden", "Norway", "Croatia", "Macedonia",
                      "Hungary","Canada", "Philippines", "Netherlands")

for (i in 1:length(countries_select)){
  if (i == 1)
    data081 <- data08[data08$Country == countries_select[i], ]
  else
    data081 <- rbind(data081, data08[data08$Country == countries_select[i], ])
}

# Data Visualization
data081 <- data081[order(-data081[ ,"Display.Value"]), ]
pos <- data081$Country
title <- paste("Excise Tax for 1L of Pure Alcohol in 2008")
ggplot(data = data081, aes(y = Display.Value, x = Country)) + 
  geom_bar(stat = "identity", colour = "black", fill = "#333333") + 
  scale_x_discrete(limits = pos) +
  theme_light() + 
  xlab("Country") + ylab("Percentage of the Total Retail Price") +
  ggtitle(title)

mean(data081$Display.Value)
sd(data081$Display.Value)

# Binomial Excise Tax on Alcoholic Beverages ------------------------------
# 2012
sele_par <- c("Country", "Beverage.Types", "Display.Value")
databases$tax_type <- databases$tax_type[databases$tax_type$Year==2012, sele_par]

indic <- databases$tax_type$Display.Value == "Yes" 
indico <- databases$tax_type$Display.Value == "No"
aux1 <- databases$tax_type[indic, ]
aux1$Display.Value <- replicate(nrow(aux1), 1)
aux2 <- databases$tax_type[indico, ]
aux2$Display.Value <- replicate(nrow(aux2), 0)
data09 <- rbind(aux1, aux2) 

total_contr <- length(unique(data09$Country))
categ <- unique(data09$Beverage.Types)
spirits_per <- sum(data09[data09$Beverage.Types =="Spirits", "Display.Value"])/total_contr
wine_per <- sum(data09[data09$Beverage.Types =="Wine", "Display.Value"])/total_contr
beer_per <- sum(data09[data09$Beverage.Types =="Beer", "Display.Value"])/total_contr

contr1 <- unique(data09$Country)
data092 <- numeric()
for(i in 1:total_contr){
  data092[i] <- sum(data09[data09$Country == contr1[i], "Display.Value"])
}

all_per <- sum(data092 == 3)/total_contr

data091 <- data.frame(Beverage = c("Spirits","Wine","Beer", "All"),
                         Existing_tax = c(spirits_per, wine_per, beer_per, all_per))

# barplot 
data091 <- data091[order(-data091[ ,"Existing_tax"]), ]
pos <- data091$Beverage
title <- paste("Percentage of Countries With an Existing Alcohol Tax by Type in 2012")
ggplot(data = data091, aes(y = Existing_tax, x = Beverage)) + 
  geom_bar(stat = "identity", colour = "black", fill = "#333333") + 
  scale_x_discrete(limits = pos) +
  theme_light() + 
  xlab("Beverage") + ylab("Percentage") +
  ggtitle(title)

# Select data of interest
country_choice <- c("France", "Germany", "Denmark", "Greece")
for (i in 1:length(country_choice)){
  if (i == 1)
    data093 <- data09[data09$Country == country_choice[i], ]
  else
    data093 <- rbind(data093, data09[data09$Country == country_choice[i], ])
}


data093 <- data093[order(data093$Beverage.Types),]

# selected radial plot
pal <- c("#ff2106", "#1c444b", "#538db8", "#73c382", "#b4801d", "#964712","#b70d0e")
ggplot(data = data093, aes(x  = Beverage.Types, y = Display.Value) ) + 
  geom_polygon(aes(group = Country, color = Country), fill = NA, show.legend = FALSE, size = 1.1, alpha = 0.5) +
  geom_line(aes(group = Country, color = Country), size = 1.1, alpha = 0.5) +
  guides(color = guide_legend(ncol=1)) +
  theme_minimal() +
  coord_radar() +
  xlab("") + ylab("") +
  ggtitle("Excise Tax on Alcoholic Beverages by Country in 2012") +
  scale_color_manual(values = pal[1:4]) +
  theme(strip.text.x = element_text(size = rel(0.8)),
        axis.text.x = element_text(size = rel(0.8)),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())


# CORRELATION ANALYSIS ----------------------------------------------------


# Expenditure vs wage -----------------------------------------------------
# databases$houshold_exp for expenditure
# data07 for wages

for(i in 1:nrow(databases$houshold_exp)){
  if (databases$houshold_exp[i,"Country"] == "United Kingdom"){
    databases$houshold_exp[i, "Country"] <- "UK"
  }
  if (databases$houshold_exp[i,"Country"] == "United States"){
    databases$houshold_exp[i, "Country"] <- "US"
  }
}

raw_x <- databases$houshold_exp
raw_y <- data07

countries_x <- unique(raw_x$Country)
countries_x <- countries_x[order(countries_x)]
countries_y <- unique(raw_y$Country)
countries_y <- countries_y[order(countries_y)]

year_x <- unique(raw_x$Year)
year_x <- year_x[order(year_x)]
year_y <- unique(raw_y$Time)
year_y <- year_y[order(year_y)]

k <- 1
year_co    <- numeric()
country_co <- numeric()
x <- numeric()
y <- numeric()
for(i in 1:nrow(raw_x)){
  for(j in 1:nrow(raw_y)){
    if(raw_x$Country[i] == raw_y$Country[j]){
      if(raw_x$Year[i] == raw_y$Time[j]){
        year_co[k] <- raw_x$Year[i]
        country_co[k] <- raw_x$Country[i]
        x[k] <- raw_x$Numeric[i]
        y[k] <- raw_y$Value[j]
        k <- k + 1
      }
    }
  }
}

dataset <- data.frame(Country = country_co, Year = year_co,
                      Expenditure = x, Annual_Wage = y)
dataset <- na.omit(dataset)
with(dataset, cor.test(Expenditure, Annual_Wage, alternative="greater"))
lm(Expenditure ~ Annual_Wage, data=dataset)

with(dataset, cor(Expenditure, Annual_Wage,
                  use = "everything", method = "pearson"))

# 0.41
ggplot(dataset, aes(x=Annual_Wage, y=Expenditure)) + 
  geom_point(aes(colour = Year)) +
  geom_smooth(se = FALSE, colour="darkblue", method = "lm", size = 0.7)+#, formula = y ~ poly(x, 5, raw=TRUE))+
  ylab("% expenditure in alcohol") +
  xlab("Annual wage in USD") +
  ggtitle("Relation Between Wage and Percentage of Expenditure") +
  theme_light()


# Alcohol consumption vs taxation -----------------------------------------
# alcohol consumption (2008 - 2010): data05 (y variabe)
# taxation (2008): data08 (x variable)

raw_x <- data08
raw_y <- data05[data05$Sex=="Both sexes", c("Country", "Numeric")]

countries_x <- unique(raw_x$Country)
countries_x <- countries_x[order(countries_x)]
countries_y <- unique(raw_y$Country)
countries_y <- countries_y[order(countries_y)]

j <- 1
for(i in 1:length(countries_x)){
  if(sum(countries_x[i] == countries_y) == 1){
    if(j == 1){
      dataseta <- cbind(raw_y[raw_y$Country == countries_x[i], ],
                       raw_x[raw_x$Country == countries_x[i],
                             c("Display.Value") ])
      j <- j + 1
    } else {
      temp <- cbind(raw_y[raw_y$Country == countries_x[i], ],
                    raw_x[raw_x$Country == countries_x[i],
                          c("Display.Value") ])
      dataseta <- rbind(dataseta, temp)
    }
  }
}

colnames(dataseta) <- c("Country", "Alcohol_consumpt", "Taxation")
with(dataseta, cor(Alcohol_consumpt, Taxation,
                  use = "everything", method = "pearson"))
with(dataseta, cor.test(Alcohol_consumpt, Taxation, alternative="less"))
lm(Alcohol_consumpt ~ Taxation, data=dataseta)


# -0.22
ggplot(dataseta, aes(x=Taxation, y=Alcohol_consumpt)) + 
  geom_point(fill = "dark blue", colour = "black", alpha=4/10, shape=21, size = 2) +
  geom_smooth(se = FALSE, colour="dark blue",method = "lm", size = 0.9)+
  ylab("Alcohol consumption (L)") +
  xlab("Alcohol duty (% of total retail price)") +
  ggtitle("Relation Between Alcohol Consumption and Alcohol Duty") +
  theme_light()


# Crashes vs consumption/taxation ------------------------------------
# crashes (y variable) databases$traffic_crashes
# consumption (x1) data05 (2008 - 2010)
# NOT ENOUGH DATA -- taxation (x2) data08 (2008)

for(i in 1:nrow(databases$traffic_crashes)){
  if (databases$traffic_crashes[i,"Country"] == "United Kingdom of Great Britain and Northern Ireland"){
    databases$traffic_crashes[i, "Country"] <- "UK"
  }
  if (databases$traffic_crashes[i,"Country"] == "Russian Federation"){
    databases$traffic_crashes[i, "Country"] <- "Russia"
  }
  if (databases$traffic_crashes[i,"Country"] == "The former Yugoslav republic of Macedonia"){
    databases$traffic_crashes[i, "Country"] <- "Macedonia"
  }
  if (databases$traffic_crashes[i,"Country"] == "Democratic Republic of the Congo"){
    databases$traffic_crashes[i, "Country"] <- "Congo"
  }
  if (databases$traffic_crashes[i,"Country"] == "United Republic of Tanzania"){
    databases$traffic_crashes[i, "Country"] <- "Tanzania"
  }
  if (databases$traffic_crashes[i,"Country"] == "Venezuela (Bolivarian Republic of)"){
    databases$traffic_crashes[i, "Country"] <- "Venezuela"
  }
  if (databases$traffic_crashes[i,"Country"] == "Micronesia (Federated States of)"){
    databases$traffic_crashes[i, "Country"] <- "Micronesia"
  }
}

raw_x1 <- data05[data05$Sex=="Both sexes", c("Country", "Numeric")]
raw_x2 <- data08
raw_y <- databases$traffic_crashes[databases$traffic_crashes$Year == 2008, c("Country", "Numeric")]

countries_x1 <- unique(raw_x1$Country)
countries_x1 <- countries_x1[order(countries_x1)]
countries_x2 <- unique(raw_x2$Country)
countries_x2 <- countries_x2[order(countries_x2)]
countries_y <- unique(raw_y$Country)
countries_y <- countries_y[order(countries_y)]

j <- 1
for(i in 1:length(countries_x1)){
  if(sum(countries_x1[i] == countries_y) == 1){
    if(j == 1){
      dataset1 <- cbind(raw_y[raw_y$Country == countries_x1[i], ],
                       raw_x1[raw_x1$Country == countries_x1[i],
                             c("Numeric") ])
      j <- j + 1
    } else {
      temp <- cbind(raw_y[raw_y$Country == countries_x1[i], ],
                    raw_x1[raw_x1$Country == countries_x1[i],
                          c("Numeric") ])
      dataset1 <- rbind(dataset1, temp)
    }
  }
}

colnames(dataset1) <- c("Country", "Crashes", "Consumption")
dataset1 <- na.omit(dataset1)
with(dataset1, cor(Consumption, Crashes,
                  use = "everything", method = "pearson"))

j <- 1
for(i in 1:length(countries_x2)){
  if(sum(countries_x2[i] == countries_y) == 1){
    if(j == 1){
      dataset2 <- cbind(raw_y[raw_y$Country == countries_x2[i], ],
                        raw_x2[raw_x2$Country == countries_x2[i],
                               c("Display.Value") ])
      j <- j + 1
    } else {
      temp <- cbind(raw_y[raw_y$Country == countries_x2[i], ],
                    raw_x2[raw_x2$Country == countries_x2[i],
                           c("Display.Value") ])
      dataset2 <- rbind(dataset2, temp)
    }
  }
}

colnames(dataset2) <- c("Country", "Crashes", "Taxation")
dataset2 <- na.omit(dataset2)
with(dataset2, cor(Crashes, Taxation,
                  use = "everything", method = "pearson"))

with(dataset1, cor.test(Crashes, Consumption, alternative="greater"))
lm(Crashes ~ Consumption, data=dataset1)

# 0.354
ggplot(dataset1, aes(x=Consumption, y=Crashes)) + 
  geom_point(color = "black", fill = "dark red", alpha=4/10, shape=21, size = 2) +
  geom_smooth(se = FALSE, colour="dark red",method = "lm", size = 0.9, formula = y ~ poly(x, 1, raw=TRUE))+
  ylab("Percentage of crashes") +
  xlab("Alcohol consumption (L)") +
  ggtitle("Relation Between Alcohol Consumption and Traffic Crashes") +
  theme_light()

# -0.17
# ggplot(dataset2, aes(x=Taxation, y=Crashes)) + 
#   geom_point(color = "Dark red") +
#   geom_smooth(se = FALSE, colour="black",method = "lm", size = 0.7)

