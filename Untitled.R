#LIBRARY
library(dplyr)
library(ggplot2)

#DATA
mpv <- read.csv("https://airtable-csv-exports-production.s3.amazonaws.com/7d93a0bda21ed57d/Mapping%20Police%20Violence.csv?AWSAccessKeyId=ASIAR7KB7OYSJIYVVXQA&Expires=1670888756&Signature=Vg03v0wmyqTTPQuX4vjH9vSzUbA%3D&x-amz-security-token=FwoGZXIvYXdzEJn%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaDHK3eGtcGFjLkMCgLCLKAaIdXioYhZgUbxBM%2BjgA7puOKM7yzsAwvbbFpejW%2FRymY%2BKvj%2FxyrEoJTNHxV18u3i0ItuFTW7f7%2FM18iVqF2R8Fejg2NsRE5CGPT2U1OJ59iTrMfKh%2BTFa58tcecJ6vh7HvyaCeSRAX2xdgwfP%2FI1VCO05hiGaRoGSs1eUVZB1208nZuDCKgHYck4erMEcTyu6m4XnuyFnLTY6iKlzFvwrDKGBrhpDh5nLm%2FOya6alf70EvwawI%2BFwOXl6d0fVs3z9%2Bgr16QKIAejYo%2FfXenAYyLYcTBiL0KCaPLv08iDRAiRRgPbNFMY4lYZGlBaxNj3%2Fu3coF9tj9I%2FOLIvRuqw%3D%3D")
tax <- read.csv("https://www.irs.gov/pub/irs-soi/20in34tr.xls")
#medialhouseholdincome = https://www.census.gov/library/visualizations/2022/comm/median-household-income.html

#WRANGLE DATA
mpv<- mpv[!(is.na(mpv$hhincome_median_census_tract) | mpv$hhincome_median_census_tract==""), ] #remove na values from income
mpv <- mpv[!grepl("0", mpv$hhincome_median_census_tract),]
mpv.income <- mpv %>% select(hhincome_median_census_tract)
mpv.income <- mpv.income %>% #Create income brackets
  mutate(income_bracket = case_when(
    hhincome_median_census_tract < 14100~"<14,100",
    hhincome_median_census_tract >14100 & hhincome_median_census_tract<=53700~"14,100-53,700",
    hhincome_median_census_tract >53701 & hhincome_median_census_tract<=85500~"53,701-85,500",
    hhincome_median_census_tract >85501 & hhincome_median_census_tract<=163300~"85,501-163,300",
    hhincome_median_census_tract >163301 & hhincome_median_census_tract<=207350~"163,301-207,350",
    hhincome_median_census_tract >207351 & hhincome_median_census_tract<=518400~"207,351-518,400"))
mpv.income <- mpv.income %>% group_by(income_bracket) %>% summarise(count = n())
mpv.income <- mpv.income %>%
  mutate(irs_bracket = case_when(
    income_bracket == "<14,100"~"0",
    income_bracket == "14,100-53,700"~"12",
    income_bracket =="53,701-85,500"~"22",
    income_bracket =="85,501-163,300"~"24",
    income_bracket =="163,301-207,350"~"32",
    income_bracket =="207,351-518,400"~"35"))

tax <- tax[10:23,1:2]
tax <- rename(tax, irs_bracket = 1)
tax$irs_bracket <-sub("   ", "", tax$irs_bracket)
tax$irs_bracket <-sub("\\ .*", "", tax$irs_bracket)
tax$X <-sub(",", "", tax$X)
tax$X <-sub(",", "", tax$X)
tax$X <- as.integer(tax$X)
tax <- aggregate(.~irs_bracket,data=tax,FUN=sum)
tax <- tax[-c(4,5,8,9,12),]

#ANALYZE
mpv.tax <- merge(mpv.income, tax,by.y="irs_bracket",by.x="irs_bracket")
mpv.tax$per100000 <- round((mpv.tax$count/mpv.tax$X)*100000,2)

temp <- mpv %>% select(hhincome_median_census_tract)
temp <- temp %>% group_by(hhincome_median_census_tract) %>% summarise(count = n())
temp <- temp %>% #Create income brackets
  mutate(income_bracket = case_when(
    hhincome_median_census_tract < 14100~"<14,100",
    hhincome_median_census_tract >14100 & hhincome_median_census_tract<=53700~"14,100-53,700",
    hhincome_median_census_tract >53701 & hhincome_median_census_tract<=85500~"53,701-85,500",
    hhincome_median_census_tract >85501 & hhincome_median_census_tract<=163300~"85,501-163,300",
    hhincome_median_census_tract >163301 & hhincome_median_census_tract<=207350~"163,301-207,350",
    hhincome_median_census_tract >207351 & hhincome_median_census_tract<=518400~"207,351-518,400"))
temp <- temp %>%
  mutate(irs_bracket = case_when(
    income_bracket == "<14,100"~"0",
    income_bracket == "14,100-53,700"~"12",
    income_bracket =="53,701-85,500"~"22",
    income_bracket =="85,501-163,300"~"24",
    income_bracket =="163,301-207,350"~"32",
    income_bracket =="207,351-518,400"~"35"))
temp <- merge(temp, tax,by.y="irs_bracket",by.x="irs_bracket")
temp$per100000 <- round((temp$count/temp$X)*100000,5)
temp2 <- subset(temp, hhincome_median_census_tract > 70784 )
temp3 <- subset(temp, hhincome_median_census_tract < 70784 )
temp.t.test <- t.test(temp2$per100000, temp3$per100000)

mpv.tax$income_bracket<-factor(mpv.tax$income_bracket, levels=c("<14,100","14,100-53,700","53,701-85,500","85,501-163,300","163,301-207,350","207,351-518,400")) #Order labels
ggplot(mpv.tax, aes(x = income_bracket, y = per100000)) + 
  geom_bar(stat = "identity", color = "#6495ED", fill = "#6495ED") +
  labs(y= 'Count per 100,000', x="Income Bracket Ceiling", title = "Number of Killings by Law Enforcement per 100,000 of Respective IRS Tax Bracket Fillings per IRS Income Bracket Ceiling") +
  scale_x_discrete(labels=c('<14,100','53,700', '85,500', '163,300', '207,350','518,400'))

boxplot(hhincome_median_census_tract~per100000,temp,
  main="Box Plot of Number Law Enforcement Killings per 100,000 of Respective IRS Tax Braket Fillings per Household Income",
  xlab="Number of Law Enforcement Killings per 100,000",
  ylab="Household Income")
