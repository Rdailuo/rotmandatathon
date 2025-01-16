dat <- read.csv("C:\\Users\\ksr20\\Documents\\Rotman Datathon\\Datathon_data-2025-Raw.csv")
country_dat <- read.csv("C:\\Users\\ksr20\\Documents\\Rotman Datathon\\Country-meta_data.csv")

dat <- dat[-c(3256:3260),]

#Importing Data
numeric_dat <- dat[,3:283]
numeric_dat <- numeric_dat[,-2]

numeric_dat[numeric_dat == ".."] <- NA
numeric_dat <- lapply(numeric_dat,as.numeric)

#Replacing NA with Column Means
for(i in 1:280){
  mean_x <- mean(numeric_dat[[i]], na.rm = TRUE)
  numeric_dat[[i]][is.na(numeric_dat[[i]])] <- mean_x
}

numeric_dat <- as.data.frame(numeric_dat)
numeric_dat <- numeric_dat[,-205]

#Correlation Matrix between variables
cors <- cor(numeric_dat, use = "pairwise.complete.obs")

# The explanatory variables mark told me to look at
marks_explanatory <- c("Air.transport..freight..million.ton.km...IS.AIR.GOOD.MT.K1.",
                       "Broad.money....of.GDP...FM.LBL.BMNY.GD.ZS.",
                       "Central.government.debt..total....of.GDP...GC.DOD.TOTL.GD.ZS.",
                       "Current.account.balance....of.GDP...BN.CAB.XOKA.GD.ZS.",
                       "Current.education.expenditure..total....of.total.expenditure.in.public.institutions...SE.XPD.CTOT.ZS.",
                       "Current.health.expenditure.per.capita..PPP..current.international.....SH.XPD.CHEX.PP.CD.",
                       "Electric.power.consumption..kWh.per.capita...EG.USE.ELEC.KH.PC.",
                       "Energy.imports..net....of.energy.use...EG.IMP.CONS.ZS.",
                       "Exports.of.goods.and.services....of.GDP...NE.EXP.GNFS.ZS.",
                       "Final.consumption.expenditure....of.GDP...NE.CON.TOTL.ZS.",
                       "Foreign.direct.investment..net.inflows....of.GDP...BX.KLT.DINV.WD.GD.ZS.",
                       "GDP.per.capita..PPP..current.international.....NY.GDP.PCAP.PP.CD.",
                       "General.government.final.consumption.expenditure....of.GDP...NE.CON.GOVT.ZS.",
                       "Gini.index..SI.POV.GINI.",
                       "Human.capital.index..HCI...scale.0.1...HD.HCI.OVRL.",
                       "Imports.of.goods.and.services....of.GDP...NE.IMP.GNFS.ZS.",
                       "Labor.force.participation.rate..total....of.total.population.ages.15....national.estimate...SL.TLF.CACT.NE.ZS.",
                       "Labor.tax.and.contributions....of.commercial.profits...IC.TAX.LABR.CP.ZS.",
                       "Logistics.performance.index..Overall..1.low.to.5.high...LP.LPI.OVRL.XQ.",
                       "Manufacturing..value.added....of.GDP...NV.IND.MANF.ZS.",
                       "Poverty.headcount.ratio.at.national.poverty.lines....of.population...SI.POV.NAHC.",
                       "Unemployment..total....of.total.labor.force...national.estimate...SL.UEM.TOTL.NE.ZS.",
                       "Income.share.held.by.highest.10...SI.DST.10TH.10.",
                       "Time")

# Subsetting all the data with the explanatory + CPI mark told me
marks_dat <- numeric_dat[c(marks_explanatory,
                           "Consumer.price.index..2010...100...FP.CPI.TOTL.")]

# Renaming Variables CUZ THEY ARE SO BAD WTF IS R DOING
colnames(marks_dat) <- c("Air_Transport_Freight","Broad_Money","Cent_Govt_Dep",
                         "Account_Balance","Education_Expend","Health_Expend",
                         "Power_Consump","Energy_Imports","Goods_Exports",
                         "Final_consump_Exp","FDI","GDP_Capita","Govt_Consum_Exp",
                         "Gini_Index","Human_Capital_Index","Imports_Goods_Services",
                         "Labor_Particp","Labor_tax_Contrib","LPI","Manufacturing_Value",
                         "Poverty_Headcount","Unemployment","Top_10_Income",
                         "Time","CPI")

marks_dat$Country <-  as.factor(dat$Country.Code)
marks_dat$Time <- as.factor(marks_dat$Time)

## Adding Region as a Covariate instead of country to a different dataframe
regions <- c()

for (i in 1:length(country_dat$Region)){
  regions[(i*15-14):(i*15)] = country_dat$Region[i]
}

marks_new_dat <- marks_dat[,!(names(marks_dat) %in% c("Country"))]
marks_new_dat$Region <- as.factor(regions)

# basic linear model with all covariates modelling CPI
basic.lm <- lm(Consumer.price.index..2010...100...FP.CPI.TOTL. ~ . 
               + as.factor(regions) + marks_dat$Time, data = numeric_dat)


#remove outliers of CPI (improves model fit greatly)
#Should check what these are
marks_dat <- marks_dat[-c(which(marks_dat$CPI > 1000)),]
marks_new_dat <- marks_new_dat[-c(which(marks_new_dat$CPI > 1000)),]
dat <- dat[-c(dat$Consumer.price.index..2010...100...FP.CPI.TOTL. > 1000),]

basic.lm_noOutliers <-lm(Consumer.price.index..2010...100...FP.CPI.TOTL. ~ . 
                           + as.factor(regions) + marks_dat$Time, data = numeric_dat)
########################
# MODELS USING COUNTRY
########################


#Model with 23 selected by mark variates, and Time+Country Code
limited_model <- lm(CPI ~ . ,data= marks_dat)

#Model without Country
limited_model_noCountry <- lm(CPI ~ ., 
                              data = marks_dat[,!(names(marks_dat) %in% c("Country"))])

#Model with only time
model_onlyTime <- lm(CPI ~ Time, data = marks_dat)

#Doing Step AIC to select variables from mark's list
#If you type this variable into console, it'll tell you what the reduced model is
#You can copy its output to see what the best model is and its summary
variable_selection <- stepAIC(limited_model, direction = "both")

variable_selection_noCountry <- stepAIC(limited_model_noCountry, direction = "both")

#Lasso regression (more variale selection)
x <- data.matrix(marks_dat[,!(names(marks_dat) %in% c("CPI"))])
y <- marks_dat$CPI
cv_model <- cv.glmnet(x, y, alpha = 1)
best_lambda <- cv_model$lambda.1se
limited_lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
#Its not that good though. Leaves a lot of variables in


########################
# MODELS USING REGION
########################

## THESE ARE THE BEST MODELS

#Don't have both GINI and Top 10

## R^2 of 0.32
limited_region_model <- lm(CPI ~ . + Region*. + Region*Time,data = marks_new_dat)

# R^2 of 0.52. Best predictive model. A lot of useless variates though.
# Use to see what impacts CPI more or less.
# Even useless variablesa are good; you can say they aren't important
full_region_model <- lm(CPI ~ .^2, data = marks_new_dat)

## AIC stepping applied to the limited region model
variable_selection_region <- stepAIC(limited_region_model, direction = "both")

variable_selection_region_model <- variable_selection_region$call

## I made the Step AIC model better
kaarthick_arbitrary_model <- lm(formula = CPI ~ Air_Transport_Freight + Broad_Money+ GDP_Capita + 
                                  Human_Capital_Index + Imports_Goods_Services + 
                                  Labor_Particp + Labor_tax_Contrib + LPI + Manufacturing_Value + 
                                  Poverty_Headcount + Unemployment + Time+ Health_Expend:Region + Goods_Exports:Region + 
                                  Final_consump_Exp:Region + GDP_Capita:Region + Govt_Consum_Exp:Region + 
                                  Imports_Goods_Services:Region +  Labor_tax_Contrib:Region + 
                                  Manufacturing_Value:Region + Unemployment:Region, data = marks_new_dat)

#region - power, gini, HCI, LPI
#


## Super Full Model

#fullest_model <- lm(Consumer.price.index..2010...100...FP.CPI.TOTL. ~ (. 
#                    + as.factor(regions) + marks_dat$Time)^2, data = numeric_dat)

df <- summary(full_region_model)[4]
df <- df[[1]]


######################################################
# THE BEST MODELS I COULD MAKE
######################################################


# R^2 of 0.52. Best predictive model. A lot of useless variates though.
# Use to see what impacts CPI more or less.
# Even useless variablesa are good; you can say they aren't important
full_region_model <- lm(CPI ~ .^2, data = marks_new_dat)

## Probably the best model
## R^2 0.38 with minimal variables
another_model <- lm(CPI ~ Region +
                    Air_Transport_Freight:Goods_Exports +
                      Broad_Money:Account_Balance +
                      Broad_Money:Health_Expend +
                      Broad_Money:Goods_Exports+
                      Broad_Money:GDP_Capita +
                      Broad_Money:Gini_Index +
                      Broad_Money:Imports_Goods_Services +
                      Broad_Money:Labor_tax_Contrib +
                      Broad_Money:Unemployment +
                      Broad_Money:Time+
                      Cent_Govt_Dep:GDP_Capita +
                      Cent_Govt_Dep:Imports_Goods_Services +
                      Cent_Govt_Dep:Region+
                      Account_Balance:Health_Expend +
                      Account_Balance:Govt_Consum_Exp+ 
                      Account_Balance:Time+
                      Education_Expend:Region+
                      Health_Expend:Energy_Imports+ 
                      Health_Expend:Govt_Consum_Exp+ 
                      Health_Expend:Region+
                      Power_Consump:Region+
                      Energy_Imports:GDP_Capita+ 
                      Energy_Imports:Govt_Consum_Exp+ 
                      Goods_Exports:Labor_Particp +
                      Goods_Exports:Manufacturing_Value +
                      Goods_Exports:Time+
                      Goods_Exports:Region+
                      Final_consump_Exp:Imports_Goods_Services +
                      Final_consump_Exp:Labor_Particp +
                      Final_consump_Exp:Manufacturing_Value +
                      Final_consump_Exp:Time+
                      Final_consump_Exp:Region+
                      FDI:GDP_Capita +
                      GDP_Capita:Govt_Consum_Exp +
                      GDP_Capita:Manufacturing_Value+ 
                      GDP_Capita:Unemployment +
                      Govt_Consum_Exp:Gini_Index +
                      Govt_Consum_Exp:Imports_Goods_Services +
                      Govt_Consum_Exp:Manufacturing_Value +
                      Govt_Consum_Exp:Unemployment +
                      Govt_Consum_Exp:Top_10_Income +
                      Govt_Consum_Exp:Region+
                      Gini_Index:Human_Capital_Index +
                      Human_Capital_Index:Top_10_Income + 
                      Human_Capital_Index:Region+
                      Imports_Goods_Services:Manufacturing_Value +
                      Imports_Goods_Services:Time +
                      Imports_Goods_Services:Region+ 
                      Labor_Particp:Region +
                      Labor_tax_Contrib:Unemployment+
                      Labor_tax_Contrib:Region+
                      Manufacturing_Value:Time +
                      Manufacturing_Value:Region+ 
                      Poverty_Headcount:Time +
                      Poverty_Headcount:Region+ 
                      Unemployment:Region +
                      Time:Region,
                    data= marks_new_dat)

## Contains an even simpler model
## R^2 is 0.35
## Get model using another_step$call
another_step <- stepAIC(another_model)


#Plot for fun
plot(marks_dat$CPI, ylim = c(50,500))



