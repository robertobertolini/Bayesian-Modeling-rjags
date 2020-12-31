# Uninformative Priors

library(R2jags)
library(abind)
library(boot)
library(MCMCvis)

# Read in the imputed and standardized variables

df_all <- read.csv('mice_All_3225_Week3_Both.csv') # Use entire corpus of student data

# Remove variables in the corpus not considered in this study
df_all <- df_all[-c(7,9,15,16,17,18,19,20,21,22,25,27,28,29,30)]

df_final <- df_all

# Reassign the term years (1-6) based on the six semesters
df_final$both_STRM[df_final$both_STRM==1148] <- 1
df_final$both_STRM[df_final$both_STRM==1154] <- 2
df_final$both_STRM[df_final$both_STRM==1158] <- 3
df_final$both_STRM[df_final$both_STRM==1164] <- 4
df_final$both_STRM[df_final$both_STRM==1168] <- 5
df_final$both_STRM[df_final$both_STRM==1174] <- 6

sink("RE_UniCourseAll.jags")

cat("
model {

# Logistic regression model

    for (i in 1:length(Y)){
      Y[i] ~ dbern(p[i])
      logit(p[i]) <- Intercept+Gender*gender[i]+Asian*asian[i]+
      Black*black[i]+Hispanic*hispanic[i]+White*white[i]+
      Age*age[i]+Alien*alien[i]+Naturalized*naturalized[i]+GPA*gpa[i]+
      SAT*sat[i]+MPE*mpe[i]+
      Continue*continue[i]+EnrollGrad*enrollgrad[i]+Transfer*transfer[i]+
      Pretot*pretot[i]+Precumulative*precum[i]+Aid*aid[i]+PELL*pell[i]+
      TAP*tap[i]+Units*units[i]+BB*bb[i]+Totalcourse*totalcourse[i]+
      CINS*cins[i]+KC*kc[i]+random_intercept[both_STRM[i]]
    }
    
  # Random Effect Term
  
  for (j in 1:6){
    random_intercept[j] ~ dnorm(0,tau_year)
  }
  
  tau_year ~ dgamma(0.01,0.01)
  
  Intercept ~ dnorm(0,1.0E-6)
  Gender ~ dnorm(0,1.0E-6)
  Asian  ~ dnorm(0,1.0E-6)
  Black  ~ dnorm(0,1.0E-6)
  Hispanic ~ dnorm(0,1.0E-6)
  White  ~ dnorm(0,1.0E-6)
  Age  ~ dnorm(0,1.0E-6)
  Alien  ~ dnorm(0,1.0E-6)
  Naturalized  ~ dnorm(0,1.0E-6)
  GPA ~ dnorm(0,1.0E-6)
  SAT ~ dnorm(0,1.0E-6)
  MPE ~ dnorm(0,1.0E-6)
  Continue  ~ dnorm(0,1.0E-6)
  EnrollGrad  ~ dnorm(0,1.0E-6)
  Transfer ~ dnorm(0,1.0E-6)
  Pretot  ~ dnorm(0,1.0E-6)
  Precumulative  ~ dnorm(0,1.0E-6)
  Aid ~ dnorm(0,1.0E-6)
  PELL  ~ dnorm(0,1.0E-6)
  TAP  ~ dnorm(0,1.0E-6)
  Units  ~ dnorm(0,1.0E-6)
  BB ~ dnorm(0,1.0E-6)
  Totalcourse ~ dnorm(0,1.0E-6)
  CINS ~ dnorm(0,1.0E-6)
  KC ~ dnorm(0,1.0E-6)

}",fill = TRUE)

sink()

# List of data information

Dat <- list(Y = df_final$both_grade, 
            gender = df_final$Gender,
            asian = df_final$Ethnicity_Asian,
            black = df_final$Ethnicity_Black,
            hispanic = df_final$Ethnicity_Hispanic,
            white = df_final$Ethnicity_White,
            age = df_final$CensusAge,
            alien = df_final$Citizenship_Alien,
            naturalized = df_final$Citizenship_Naturalized,
            gpa=df_final$highschoolgpa,
            sat=df_final$sat1600score,
            mpe = df_final$mathplacement,
            continue = df_final$EnrollStatus_Continuing,
            enrollgrad = df_final$EnrollStatus_Grad,
            transfer = df_final$EnrollStatus_NewTransfer,
            pretot = df_final$preTOT_TAKEN_PRGRSS,
            precum = df_final$preCUM_GPA,
            aid = df_final$DISBURSED_AMOUNT_SUM,
            pell = df_final$PELL,
            tap = df_final$TAP,
            units = df_final$UNT_TAKEN_PRGRSS,
            bb = df_final$bio201TotalWk3,
            totalcourse = df_final$BBTotalCrs,
            cins = df_final$CINS,
            kc = df_final$Total_KC_pre,
            both_STRM = as.numeric(df_final$both_STRM))

# Initial Values

InitStage <- list(list(Intercept = 0,
                       Gender = 0,
                       Asian  = 0,
                       Black  = 0,
                       Hispanic = 0,
                       White  = 0,
                       Age  = 0,
                       Alien  = 0,
                       Naturalized  = 0,
                       GPA = 0,
                       SAT = 0,
                       MPE = 0,
                       Continue  = 0,
                       EnrollGrad  = 0,
                       Transfer = 0,
                       Pretot = 0,
                       Precumulative = 0,
                       Aid  = 0,
                       PELL = 0,
                       TAP  = 0,
                       Units  = 0,
                       BB = 0,
                       Totalcourse = 0,
                       CINS = 0,
                       KC = 0,
                       random_intercept=rep(0,6)
),
list(Intercept = 0.5,
     Gender = 0.5,
     Asian  = 0.5,
     Black  = 0.5,
     Hispanic = 0.5,
     White  = 0.5,
     Age  = 0.5,
     Alien  = 0.5,
     Naturalized  = 0.5,
     GPA = 0.5,
     SAT = 0.5,
     MPE = 0.5,
     Continue  = 0.5,
     EnrollGrad  = 0.5,
     Transfer = 0.5,
     Pretot = 0.5,
     Precumulative = 0.5,
     Aid  = 0.5,
     PELL = 0.5,
     TAP  = 0.5,
     Units  = 0.5,
     BB = 0.5,
     Totalcourse = 0.5,
     CINS = 0.5,
     KC = 0.5,
     random_intercept=rep(0.5,6)
))

# Parameters to Store

ParsStage <- c('Intercept',
               'Gender' ,
               'Asian',
               'Black'  ,
               'Hispanic' ,
               'White',
               'Age'  ,
               'Alien'  ,
               'Naturalized'  ,
               'GPA' ,
               'SAT',
               'MPE' ,
               'Continue'  ,
               'EnrollGrad'  ,
               'Transfer',
               'Pretot' ,
               'Precumulative',
               'Aid'  ,
               'PELL' ,
               'TAP' ,
               'Units',
               'BB',
               'Totalcourse', 
               'CINS',
               'KC',
               'random_intercept',
               'tau_year')

# MCMCM specifications

ni <- 50000  # number of draws from the posterior
nt <- 1    #thinning rate
nb <- 5000  # number to discard for burn-in
nc <- 2  # number of chains

# Execute the Code

m = jags(inits=InitStage,
         n.chains=nc,
         model.file="RE_UniCourseAll.jags",
         working.directory=getwd(),
         data=Dat,
         parameters.to.save=ParsStage,
         n.thin=nt,
         n.iter=ni,
         n.burnin=nb,
         DIC=T)

saveRDS(m,'UniCourse_RE.rds')

library(MCMCvis)
MCMCtrace(m,
          ISB = FALSE,
          excl = 'random_intercept',
          iter = 45000,
          ind = TRUE,
          pdf = FALSE)


# Informative Priors Model

# For this model, informative priors are acquired from the semesters prior to the term.
# The code can be modified to read in the appropriate semester data.

# For demonstration, the model was only run on the fall 2016 data.

# Read in files containing pre-tabulated beta and tau values
prior_data <- read.csv('PriorNumbers_BeforeImputation.csv')
prior_variance <- read.csv('PriorNumbersVariance.csv')

df <- read.csv('mice_f1168_Week3_Both.csv')
df_response <- read.csv('f1168_Two_class.csv')

entry <- 18 # Row in excel file with priors

library(R2jags)
library(abind)
library(boot)

# Composite model

sink("L_UniCourse_F1168_Inform.jags")

cat("
model {

  for (i in 1:length(Y)){
    Y[i] ~ dbern(prob[i])
    logit(prob[i]) <- Intercept_Coef+Gender_Coef*gender[i]+Asian_Coef*asian[i]+
    Black_Coef*black[i]+Hispanic_Coef*hispanic[i]+White_Coef*white[i]+
    Age_Coef*age[i]+Alien_Coef*alien[i]+
    Naturalized_Coef*naturalized[i]+
    GPA_Coef*gpa[i]+
    SAT_Coef*sat[i]+MPE_Coef*mpe[i]+
    Continue_Coef*continue[i]+EnrollGrad_Coef*enrollgrad[i]+Transfer_Coef*transfer[i]+
    Pretot_Coef*pretot[i]+Precumulative_Coef*precum[i]+Aid_Coef*aid[i]+PELL_Coef*pell[i]+
    TAP_Coef*tap[i]+Units_Coef*units[i]+BB_Coef*bb[i]+Totalcourse_Coef*totalcourse[i]+
    CINS_Coef*cins[i]+KC_Coef*kc[i]
  }
  
# Informed Prior Values
  Intercept_Coef ~ dnorm(0,1.0E-6)
  Gender_Coef ~ dnorm(Gender,Gender_Var)
  Asian_Coef  ~ dnorm(Asian,Asian_Var)
  Black_Coef  ~ dnorm(Black,Black_Var)
  Hispanic_Coef ~ dnorm(Hispanic,Hispanic_Var)
  White_Coef  ~ dnorm(White,White_Var)
  Age_Coef  ~ dnorm(Age,Age_Var)
  Alien_Coef  ~ dnorm(Alien,Alien_Var)
  Naturalized_Coef  ~ dnorm(Naturalized,Naturalized_Var)
  GPA_Coef ~ dnorm(GPA,GPA_Var)
  SAT_Coef ~ dnorm(SAT,SAT_Var)
  MPE_Coef ~ dnorm(MPE,MPE_Var)
  Continue_Coef  ~ dnorm(Continue,Continue_Var)
  EnrollGrad_Coef  ~ dnorm(EnrollGrad,EnrollGrad_Var)
  Transfer_Coef ~ dnorm(Transfer,Transfer_Var)
  Pretot_Coef  ~ dnorm(Pretot,Pretot_Var)
  Precumulative_Coef  ~ dnorm(Precum,Precum_Var)
  Aid_Coef ~ dnorm(Aid,Aid_Var)
  PELL_Coef  ~ dnorm(PELL,PELL_Var)
  TAP_Coef ~ dnorm(TAP,TAP_Var)
  Units_Coef  ~ dnorm(Units,Units_Var)
  BB_Coef ~ dnorm(BB,BB_Var)
  Totalcourse_Coef ~ dnorm(Totalcourse,Totalcourse_Var)
  CINS_Coef ~ dnorm(CINS,CINS_Var)
  KC_Coef ~ dnorm(KC,KC_Var)
  
}",fill = TRUE)

sink()

# Data columns and prior values 

Dat <- list(Y = df_response$crse_grade_off_cat, 
            gender = df$Gender,
            asian = df$Ethnicity_Asian,
            black = df$Ethnicity_Black,
            hispanic = df$Ethnicity_Hispanic,
            white = df$Ethnicity_White,
            age = df$CensusAge,
            alien = df$Citizenship_Alien,
            naturalized = df$Citizenship_Naturalized,
            gpa=df$highschoolgpa,
            sat=df$sat1600score,
            mpe = df$mathplacement,
            continue = df$EnrollStatus_Continuing,
            enrollgrad = df$EnrollStatus_Grad,
            transfer = df$EnrollStatus_NewTransfer,
            pretot = df$preTOT_TAKEN_PRGRSS,
            precum = df$preCUM_GPA,
            aid = df$DISBURSED_AMOUNT_SUM,
            pell = df$PELL,
            tap = df$TAP,
            units = df$UNT_TAKEN_PRGRSS,
            bb = df$bio201TotalWk3,
            totalcourse = df$BBTotalCrs,
            cins = df$CINS,
            kc = df$Total_KC_pre,
            Gender = prior_data[entry,2],
            Asian = prior_data[entry,3],
            Black = prior_data[entry,4],
            Hispanic = prior_data[entry,5],
            White = prior_data[entry,6],
            Age = prior_data[entry,8],
            Alien = prior_data[entry,10],
            Naturalized = prior_data[entry,11],
            GPA=prior_data[entry,12],
            SAT=prior_data[entry,13],
            MPE = prior_data[entry,14],
            Continue = prior_data[entry,23],
            EnrollGrad = prior_data[entry,24],
            Transfer = prior_data[entry,26],
            Pretot = prior_data[entry,31],
            Precum = prior_data[entry,32],
            Aid = prior_data[entry,33],
            PELL = prior_data[entry,34],
            TAP = prior_data[entry,35],
            Units = prior_data[entry,36],
            BB = prior_data[entry,37],
            Totalcourse = prior_data[entry,38],
            CINS = prior_data[entry,39],
            KC = prior_data[entry,40],
            Gender_Var = prior_variance[entry,2],
            Asian_Var = prior_variance[entry,3],
            Black_Var = prior_variance[entry,4],
            Hispanic_Var = prior_variance[entry,5],
            White_Var = prior_variance[entry,6],
            Age_Var = prior_variance[entry,8],
            Alien_Var = prior_variance[entry,10],
            Naturalized_Var = prior_variance[entry,11],
            GPA_Var=prior_variance[entry,12],
            SAT_Var=prior_variance[entry,13],
            MPE_Var = prior_variance[entry,14],
            Continue_Var = prior_variance[entry,23],
            EnrollGrad_Var = prior_variance[entry,24],
            Transfer_Var = prior_variance[entry,26],
            Pretot_Var = prior_variance[entry,31],
            Precum_Var = prior_variance[entry,32],
            Aid_Var = prior_variance[entry,33],
            PELL_Var = prior_variance[entry,34],
            TAP_Var = prior_variance[entry,35],
            Units_Var = prior_variance[entry,36],
            BB_Var = prior_variance[entry,37],
            Totalcourse_Var = prior_variance[entry,38],
            CINS_Var = prior_variance[entry,39],
            KC_Var = prior_variance[entry,40])

# Initial values

InitStage <- list(list(Intercept_Coef = 0,
                       Gender_Coef = 0,
                       Asian_Coef  = 0,
                       Black_Coef  = 0,
                       Hispanic_Coef = 0,
                       White_Coef = 0,
                       Age_Coef  = 0,
                       Alien_Coef  = 0,
                       Naturalized_Coef  = 0,
                       GPA_Coef = 0,
                       SAT_Coef = 0,
                       MPE_Coef = 0,
                       Continue_Coef  = 0,
                       EnrollGrad_Coef  = 0,
                       Transfer_Coef = 0,
                       Pretot_Coef = 0,
                       Precumulative_Coef = 0,
                       Aid_Coef  = 0,
                       PELL_Coef = 0,
                       TAP_Coef  = 0,
                       Units_Coef  = 0,
                       BB_Coef = 0,
                       Totalcourse_Coef = 0,
                       CINS_Coef = 0,
                       KC_Coef = 0),
                  list(Intercept_Coef = 0.5,
                       Gender_Coef = 0.5,
                       Asian_Coef  = 0.5,
                       Black_Coef  = 0.5,
                       Hispanic_Coef = 0.5,
                       White_Coef  = 0.5,
                       Age_Coef  = 0.5,
                       Alien_Coef  = 0.5,
                       Naturalized_Coef  = 0.5,
                       GPA_Coef = 0.5,
                       SAT_Coef = 0.5,
                       MPE_Coef = 0.5,
                       Continue_Coef  = 0.5,
                       EnrollGrad_Coef = 0.5,
                       Transfer_Coef = 0.5,
                       Pretot_Coef = 0.5,
                       Precumulative_Coef = 0.5,
                       Aid_Coef  = 0.5,
                       PELL_Coef = 0.5,
                       TAP_Coef  = 0.5,
                       Units_Coef  = 0.5,
                       BB_Coef = 0.5,
                       Totalcourse_Coef = 0.5,
                       CINS_Coef = 0.5,
                       KC_Coef = 0.5))

# Parameters to Estimate (not required for this analysis)

ParsStage <- c('Intercept_Coef',
               'Gender_Coef' ,
               'Asian_Coef',
               'Black_Coef'  ,
               'Hispanic_Coef' ,
               'White_Coef',
               'Age_Coef'  ,
               'Alien_Coef'  ,
               'Naturalized_Coef'  ,
               'GPA_Coef' ,
               'SAT_Coef',
               'MPE_Coef' ,
               'Continue_Coef'  ,
               'EnrollGrad_Coef'  ,
               'Transfer_Coef',
               'Pretot_Coef' ,
               'Precumulative_Coef',
               'Aid_Coef'  ,
               'PELL_Coef' ,
               'TAP_Coef' ,
               'Units_Coef',
               'BB_Coef',
               'Totalcourse_Coef',
               'CINS_Coef',
               'KC_Coef')

# MCMC Specifications - this was run for a larger number of iterations

ni <- 200000  # number of draws from the posterior
nt <- 1    #thinning rate
nb <- 50000  # number to discard for burn-in
nc <- 2  # number of chains

# Execute JAGS

m = jags(inits=InitStage,
         n.chains=nc,
         model.file="L_UniCourse_F1168_Inform.jags",
         working.directory=getwd(),
         data=Dat,
         parameters.to.save=ParsStage,
         n.thin=nt,
         n.iter=ni,
         n.burnin=nb,
         DIC=T)

m <- saveRDS(m,'L_UniCourse_F1168_Inform.rds')