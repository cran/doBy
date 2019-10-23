##
## beets
##

#' @title beets data
#'
#' @description Yield and sugar percentage in sugar beets from a split plot
#'     experiment.  Data is obtained from a split plot experiment. There are 3
#'     blocks and in each of these the harvest time defines the "whole plot" and
#'     the sowing time defines the "split plot". Each plot was \eqn{25 m^2} and
#'     the yield is recorded in kg. See 'details' for the experimental layout.
#'
#' @name data-beets
#' 
#' @docType data
#' @format The format is: chr "beets"
#'
#' @details
#' \preformatted{  
#' Experimental plan
#' Sowing times            1        4. april
#'                         2       12. april
#'                         3       21. april
#'                         4       29. april
#'                         5       18. may
#' Harvest times           1        2. october
#'                         2       21. october
#' Plot allocation:
#'                Block 1     Block 2     Block 3
#'             +-----------|-----------|-----------+
#'       Plot  | 1 1 1 1 1 | 2 2 2 2 2 | 1 1 1 1 1 | Harvest time
#'        1-15 | 3 4 5 2 1 | 3 2 4 5 1 | 5 2 3 4 1 | Sowing time
#'             |-----------|-----------|-----------|
#'       Plot  | 2 2 2 2 2 | 1 1 1 1 1 | 2 2 2 2 2 | Harvest time
#'       16-30 | 2 1 5 4 3 | 4 1 3 2 5 | 1 4 3 2 5 | Sowing time
#'             +-----------|-----------|-----------+  
#' }
#'
#' @references Ulrich Halekoh, Søren Højsgaard (2014)., A Kenward-Roger
#'     Approximation and Parametric Bootstrap Methods for Tests in Linear Mixed
#'     Models - The R Package pbkrtest., Journal of Statistical Software,
#'     58(10), 1-30., \url{http://www.jstatsoft.org/v59/i09/}
#' 
#' @keywords datasets
#'
#' @examples
#' data(beets)
#' 
#' beets$bh <- with(beets, interaction(block, harvest))
#' summary(aov(yield ~ block + sow + harvest + Error(bh), beets))
#' summary(aov(sugpct ~ block + sow + harvest + Error(bh), beets))
#' 
"beets"


#' Gene expression signatures for p53 mutation status in 250 breast cancer
#' samples
#' 
#' 
#' Perturbations of the p53 pathway are associated with more aggressive and
#' therapeutically refractory tumours. We preprocessed the data using Robust
#' Multichip Analysis (RMA). Dataset has been truncated to the 1000 most
#' informative genes (as selected by Wilcoxon test statistics) to simplify
#' computation. The genes have been standardised to have zero mean and unit
#' variance (i.e. z-scored).
#' 
#' The factor \code{code} defines whether there was a mutation in the p53
#' sequence (code=case) or not (code=control).
#' 
#' @name data-breastcancer
#'
#' @docType data
#'
#' @format A data frame with 250 observations on 1001 variables. The
#'     first 1000 columns are numerical variables; the last column
#'     (named \code{code}) is a factor with levels \code{case} and
#'     \code{control}.
#'
#' @references Miller et al (2005, PubMed
#'     ID:16141321)
#'
#' @source Dr. Chris Holmes, c.holmes at stats
#'     dot. ox . ac .uk
#'
#' @keywords datasets
#'
#' @examples
#' 
#' data(breastcancer)
#' bc <- breastcancer
#' pairs(bc[,1:5], col=bc$code)
#'
#' train <- sample(1:nrow(bc), 50)
#' table(bc$code[train])
#' library(MASS)
#' z <- lda(code ~ ., data=bc, prior = c(1,1)/2, subset = train)
#' pc <- predict(z, bc[-train, ])$class
#' pc
#' bc[-train, "code"]
#' table(pc, bc[-train, "code"])
#' 
"breastcancer"


#' budworm data
#' 
#' Effect of Insecticide on survivial of tobacco budworms
#' number of killed budworms exposed to an insecticidepp
#' mortality of the moth tobacco budworm 'Heliothis virescens' for 6 doses of
#' the pyrethroid trans-cypermethrin differentiated with respect to sex
#'
#' @name data-budworm
#' @docType data
#' 
#' @format This data frame contains 12 rows and 4 columns:
#' \describe{
#' \item{sex:}{sex of the budworm}
#' \item{dose:}{dose of the insecticide trans-cypermethrin in [\eqn{\mu}{mu}g]}
#' \item{ndead:}{budworms killed in a trial}
#' \item{ntotal:}{total number of budworms exposed per trial }
#' }
#'
#' @references Venables, W.N; Ripley, B.D.(1999) Modern Applied Statistics with
#' S-Plus, Heidelberg, Springer, 3rd edition, chapter 7.2
#'
#' @source Collet, D. (1991) Modelling Binary Data, Chapman & Hall, London,
#' Example 3.7
#'
#' @keywords datasets
#' @examples
#' 
#' data(budworm)
#' 
#' ## function to caclulate the empirical logits
#' empirical.logit<- function(y, n) {
#'    el <- log((y + 0.5) / (n - y + 0.5))
#'    el
#' }
#' 
#' 
#' # plot the empirical logits against log-dose
#' 
#' log.dose <- log(budworm$dose)
#' emp.logit <- empirical.logit(budworm$ndead, budworm$ntotal)
#' plot(log.dose, emp.logit, type='n', xlab='log-dose', ylab='emprirical logit')
#' title('budworm: emprirical logits of probability to die ')
#' male <- budworm$sex=='male'
#' female <- budworm$sex=='female'
#' lines(log.dose[male], emp.logit[male], type='b', lty=1, col=1)
#' lines(log.dose[female], emp.logit[female], type='b', lty=2, col=2)
#' legend(0.5, 2, legend=c('male', 'female'), lty=c(1, 2), col=c(1, 2))
#' 
#' \dontrun{
#' * SAS example;
#' data budworm;
#' infile 'budworm.txt' firstobs=2;
#' input sex dose ndead ntotal;
#' run;
#' }
#' 
#' 
"budworm"


##
## carcass
##

#' Lean meat contents of 344 pig carcasses
#' 
#' Measurement of lean meat percentage of 344 pig carcasses together with
#' auxillary information collected at three Danish slaughter houses
#' 
#' @name data-carcass
#' 
#' @aliases carcass carcassall
#' @format carcassall: A data frame with 344 observations on the following 17
#' variables.
#'  \describe{
#'   \item{\code{weight}}{Weight of carcass}
#'   \item{\code{lengthc}}{Length of carcass from back toe to head (when
#'     the carcass hangs in the back legs)}
#'   \item{\code{lengthf}}{Length of carcass from back toe to front leg
#'     (that is, to the shoulder)}
#'   \item{\code{lengthp}}{Length of carcass from back toe to the pelvic bone}
#'   \item{\code{Fat02, Fat03, Fat11, Fat12, Fat13, Fat14, Fat16}}{Thickness of fat
#'     layer at different locations on the back of the carcass (FatXX
#'     refers to thickness at (or rather next to) rib no. XX. Notice that
#'     02 is closest to the head}
#'   \item{\code{Meat11, Meat12, Meat13}}{Thickness of meat layer at different
#'     locations on the back of the carcass, see description above}
#'   \item{\code{LeanMeat}}{Lean meat percentage determined by dissection}
#'   \item{\code{slhouse}}{Slaughter house; a factor with levels \code{a} \code{b} \code{c}}
#'   \item{\code{sex}}{Sex of the pig; a factor with \code{a} \code{b}
#'     \code{c}. Notice that it is no an error to have three levels; the
#'     third level refers to castrates}
#' }
#'
#' @note carcass: Contains only the variables Fat11, Fat12, Fat13,
#'     Meat11, Meat12, Meat13, LeanMeat
#' @source Busk, H., Olsen, E. V., Brøndum, J. (1999) Determination of
#'     lean meat in pig carcasses with the Autofom classification
#'     system, Meat Science, 52, 307-314
#' @keywords datasets
#' @examples
#' data(carcass)
#' head(carcass)
#' 
"carcass"
"carcassall"


#' Diet of Atlantic cod in the Gulf of St. Lawrence (Canada)
#' 
#' Stomach content data for Atlantic cod (Gadus morhua) in the Gulf of
#' St.Lawrence, Eastern Canada.  Note: many prey items were of no interest for
#' this analysis and were regrouped into the "Other" category.
#' 
#' Cod are collected either by contracted commerical fishing vessels
#' (\code{ship.type} 90 or 99) or by research vessels.  Commercial vessels are
#' identified by a unique \code{ship.id}.
#' 
#' Either one research vessel or several commercial vessels conduct a survey
#' (\code{trip}), during which a trawl, gillnets or hooked lines are set
#' several times. Most trips are random stratified surveys (depth-based
#' stratification).
#' 
#' Each trip takes place within one of the \code{region}s.  The \code{trip}
#' label is only guaranteed to be unique within a region and the \code{set}
#' label is only guaranteed to be unique within a \code{trip}.
#' 
#' For each fish caught, the \code{fish.length} is recorded and the fish is
#' allocated a \code{fish.id}, but the \code{fish.id} is only guaranteed to be
#' unique within a \code{set}. A subset of the fish caught are selected for
#' stomach analysis (stratified random selection according to fish length; unit
#' of stratification is the set for research surveys, the combination ship.id
#' and stratum for surveys conducted by commercial vessels, although strata are
#' not shown in codstom).
#' 
#' The basic experimental unit in this data set is a cod stomach (one stomach
#' per fish).  Each stomach is uniquely identified by a combination of
#' \code{region}, \code{ship.type}, \code{ship.id}, \code{trip}, \code{set},
#' and \code{fish.id}.
#' 
#' For each prey item found in a stomach, the species and mass of the prey item
#' are recorded, so there can be multiple observations per stomach.  There may
#' also be several prey items with the same \code{prey.type} in the one stomach
#' (for example many \code{prey.types} have been recoded \code{Other}, which
#' produced many instances of \code{Other} in the same stomach).
#' 
#' If a stomach is empty, a single observation is recorded with
#' \code{prey.type} \code{Empty} and a \code{prey.mass} of zero.
#' 
#' @name data-codstom
#' @docType data
#'
#' @format A data frame with 10000 observations on the following 10 variables.
#' \describe{
#'   \item{\code{region}}{a factor with levels \code{SGSL} \code{NGSL} 
#'         representing the southern and northern Gulf of St. Lawrence, respectively}
#'   \item{\code{ship.type}}{a factor with levels \code{2} \code{3} \code{31} 
#'   \code{34} \code{90} \code{99}}
#'   \item{\code{ship.id}}{a factor with levels \code{11558} \code{11712}
#'     \code{136148} \code{136885}
#'     \code{136902} \code{137325} \code{151225} \code{151935} \code{99433}}
#'   \item{\code{trip}}{a factor with levels \code{10} \code{11}
#'     \code{12} \code{179} \code{1999}
#'     \code{2} \code{2001} \code{20020808} \code{3} \code{4} \code{5}
#'     \code{6} \code{7} \code{8}
#'     \code{88} \code{9} \code{95}}     
#'   \item{\code{set}}{a numeric vector}
#'   \item{\code{fish.id}}{a numeric vector}
#'   \item{\code{fish.length}}{a numeric vector, length in mm}
#'   \item{\code{prey.mass}}{a numeric vector, mass of item in stomach, in g}
#'   \item{\code{prey.type}}{a factor with levels \code{Ammodytes_sp}
#'     \code{Argis_dent}
#'     \code{Chion_opil} \code{Detritus} \code{Empty} \code{Eualus_fab}
#'     \code{Eualus_mac} \code{Gadus_mor} \code{Hyas_aran}
#'     \code{Hyas_coar}
#'     \code{Lebbeus_gro} \code{Lebbeus_pol} \code{Leptocl_mac}
#'     \code{Mallot_vil}
#'     \code{Megan_norv} \code{Ophiuroidea} \code{Other} \code{Paguridae}
#'     \code{Pandal_bor} \code{Pandal_mon} \code{Pasiph_mult}
#'     \code{Sabin_sept}
#'     \code{Sebastes_sp} \code{Them_abys} \code{Them_comp} \code{Them_lib}}    
#' }
#' @source Small subset from a larger dataset (more stomachs, more variables,
#'     more \code{prey.types}) collected by D. Chabot and M. Hanson, Fisheries &
#'     Oceans Canada (chabotd@dfo-mpo.gc.ca).
#'
#' @keywords datasets
#' @examples
#' 
#' data(codstom)
#' str(codstom)
#' # removes multiple occurences of same prey.type in stomachs
#' codstom1 <- summaryBy(prey.mass ~ 
#'                       region + ship.type + ship.id + trip + set + fish.id + prey.type,
#'                       data = codstom, 
#'                       FUN = sum) 
#' 
#' # keeps a single line per stomach with the total mass of stomach content
#' codstom2 <- summaryBy(prey.mass ~ region + ship.type + ship.id + trip + set + fish.id,
#'                       data = codstom, 
#'                       FUN = sum) 
#' 
#' # mean prey mass per stomach for each trip
#' codstom3 <- summaryBy(prey.mass.sum ~ region + ship.type + ship.id + trip,
#'                       data = codstom2, FUN = mean) 
#' 
#' \dontrun{          
#' # wide version, one line per stomach, one column per prey type
#' library(reshape)
#' codstom4 <- melt(codstom, id = c(1:7, 9))
#' codstom5 <- cast(codstom4, 
#'                  region + ship.type + ship.id + trip + set + fish.id + fish.length ~ 
#'                  prey.type, sum)
#' k <- length(names(codstom5))
#' prey_col <- 8:k
#' out <- codstom5[,prey_col]
#' out[is.na(out)] <- 0
#' codstom5[,prey_col] <- out
#' codstom5$total.content <- rowSums(codstom5[, prey_col])
#' }
#' 
"codstom"



#' crimeRate
#' 
#' Crime rates per 100,000 inhabitants in states of the USA for different crime
#' types.
#' 
#' @name data-crimeRate
#' 
#' @docType data
#' @format This data frame contains:
#' \describe{
#' \item{State:}{State of the USA}
#' \item{Murder:}{crime of murder}
#' \item{Rape:}{}
#' \item{Robbery:}{}
#' \item{Assault:}{}
#' \item{Burglary:}{residential theft}
#' \item{Larceny:}{unlawful taking of personal property (pocket picking)}
#' \item{AutoTheft:}{}
#' }
#' @keywords datasets
#' @examples
#' 	
#' data(crimeRate)
#' 
"crimeRate"


##
## dietox
##

#' Growth curves of pigs in a 3x3 factorial experiment
#' 
#' The \code{dietox} data frame has 861 rows and 7 columns.
#'
#' @name data-dietox
#' 
#' @details Data contains weight of slaughter pigs measured weekly for 12
#'     weeks. Data also contains the startweight (i.e. the weight at week
#'     1). The treatments are 3 different levels of Evit = vitamin E (dose: 0,
#'     100, 200 mg dl-alpha-tocopheryl acetat /kg feed) in combination with 3
#'     different levels of Cu=copper (dose: 0, 35, 175 mg/kg feed) in the feed.
#'     The cumulated feed intake is also recorded. The pigs are littermates.
#' 
#' 
#' @format This data frame contains the following columns:
#' 
#' \describe{
#' \item{Weight}{Weight}
#' \item{Feed}{Cumulated feed intake}
#' \item{Time}{Time (in weeks) in the experiment}
#' \item{Pig}{Id of each pig}
#' \item{Evit}{Vitamin E dose}
#' \item{Cu}{Copper dose}
#' \item{Start}{Start weight in experiment, i.e. weight at week 1.}
#' \item{Litter}{Id of litter of each pig}
#' }
#' 
#' @source Lauridsen, C., Højsgaard, S.,Sørensen, M.T. C. (1999) Influence of
#'     Dietary Rapeseed Oli, Vitamin E, and Copper on Performance and
#'     Antioxidant and Oxidative Status of Pigs. J. Anim. Sci.77:906-916
#' @keywords datasets
#' @examples
#' 
#' data(dietox)
#' str(dietox) ;
#' plot(dietox)
#' 
#' 
"dietox"


##
## fatacid
##

#' @title Fish oil in pig food
#'
#' @description ...
#'
#' @name fatacid
#' @docType data
#'
#' @format ...
#'
#' @details A fish oil fatty acid \code{X14} has been added in
#'     different concentrations to the food for pigs in a
#'     study. Interest is in studying how much of the fatty acid can
#'     be found in the tissue. The concentrations of \code{x14} in the
#'     food are \code{verb+dose+=\{0.0, 4.4, 6.2, 9.3\}}.
#'
#' The pigs are fed with this food until their weight is 60 kg.  From
#' thereof and until they are slaughtered at 100kg, their food does
#' not contain the fish oil.
#' At 60kg (sample=1) and 100kg (sample=2) muscle
#' biopsies are made and the concentration of x14 is
#' determined.
#' Measurements on the same pig are correlated, and pigs are additionally
#' related through litters.
#'
#' @references Data courtesy of Charlotte Lauridsen, Department of
#'     Animal Science, Aarhus University, Denmark.
"fatacid"


#' @title Forced expiratory volume in children
#'
#' @description Dataset to examine if respiratory function in children
#'   was influenced by smoking.
#'
#' @name fev
#' @docType data
#'
#' @format A data frame with 654 observations on the following 5 variables.
#' \describe{
#'   \item{\code{Age}}{Age in years.}
#'   \item{\code{FEV}}{Forced expiratory volume in liters per second.}
#'   \item{\code{Ht}}{Height in inches}
#'   \item{\code{Gender}}{Gender}
#'   \item{\code{Smoke}}{Smoking}
#' }
#'
#'
#' @references I. Tager and S. Weiss and B. Rosner and F. Speizer (1979). Effect
#' of Parental Cigarette Smoking on the Pulmonary Function of
#' Children.  American Journal of Epidemiology. 110:15-26
#'
#' @examples
#'
#' data(fev)
#' summary(fev)
#' 
"fev"


#' Heat development in cement under hardening.
#' 
#' Heat development in cement under hardening related to the chemical
#' composition.
#' 
#' 
#' @name data-haldCement
#' @docType data
#'
#' @format A data frame with 13 observations on the following 5 variables.
#'   \describe{
#'     \item{\code{x1}}{Percentage (weight) of [3Ca0][Al2O3]}
#'     \item{\code{x2}}{Percentage (weight) of [3Cao][SiO2]}
#'     \item{\code{x3}}{Percentage (weight) of [4Ca0][Al2O3][Fe03]}
#'     \item{\code{x4}}{Percentage (weight) of [2Cao][SiO2]}
#'     \item{\code{y}}{Heat development measured in calories per
#'       gram cement after 180 days}
#'   }
#'
#' @references Anders Hald (1949); Statistiske Metoder; Akademisk Forlag (in
#' Danish), page 509.
#' @keywords datasets
#' @examples
#' 
#' data(haldCement)
#' 
#' if( interactive() ){
#' pairs( haldCement )
#' }
#' m <- lm( y ~ x1 + x2 + x3 + x4, data=haldCement )
#' summary( m )
#' 
#' # Notice: The model explains practically all variation in data;
#' # yet none of the explanatory variables appear to be statistically
#' # significant...
#'
"haldCement"



#' Milk yield data for manually milked cows.
#' 
#' Milk yield data for cows milked manually twice a day (morning and evening).
#' 
#' There are data for 222 cows. Some cows appear more than once in the dataset
#' (in different lactations) and there are 288 different lactations.
#' 
#' @name data-milkman
#' @docType data
#'
#' @format
#' A data frame with 161836 observations on the following 12 variables.
#' \describe{
#'   \item{\code{cowno}}{a numeric vector; cow identification}
#'   \item{\code{lactno}}{a numeric vector; lactation number}
#'   \item{\code{ampm}}{a numeric vector; milking time: 1: morning; 2: evening}
#'   \item{\code{dfc}}{a numeric vector; days from calving}
#'   \item{\code{my}}{a numeric vector; milk yield (kg)}
#'   \item{\code{fatpct}}{a numeric vector; fat percentage}
#'   \item{\code{protpct}}{a numeric vector; protein percentage}
#'   \item{\code{lactpct}}{a numeric vector; lactose percentage}
#'   \item{\code{scc}}{a numeric vector; somatic cell counts}
#'   \item{\code{race}}{a factor with levels \code{RDM} \code{Holstein} \code{Jersey}}
#'   \item{\code{ecmy}}{a numeric vector; energy corrected milk}
#'   \item{\code{cowlact}}{Combination of cowno and lactno; necessary
#'     because the same cow may appear more than once in the dataset (in
#'     different lactations)}
#' }
#'
#' @keywords datasets
#'
#' @references Friggens, N. C.; Ridder, C. and Løvendahl, P. (2007). 
#' On the Use of Milk Composition Measures to Predict the Energy Balance of Dairy Cows.
#' J. Dairy Sci. 90:5453–5467 doi:10.3168/jds.2006-821.
#'
#' This study was part of the Biosens project used data from the
#' “Mælkekoens energibalance og mobilisering” project; both were
#' funded by the Danish Ministry of Food, Agriculture and Fisheries
#' and the Danish Cattle Association.
#' 
#' @examples
#' 
#' data(milkman)
#' 
"milkman"


#' NIRmilk
#' 
#' Near infra red light (NIR) measurments are made at 152 wavelengths on 17
#' milk samples.  While milk runs through a glass tube, infra red light is sent
#' through the tube and the amount of light passing though the tube is measured
#' at different wavelengths.  Each milk sample was additionally analysed for
#' fat, lactose, protein and drymatter.
#' 
#' PCA regression
#' 
#' @name data-NIRmilk
#' @docType data
#' @format This data frame contains 18 rows and 158 columns.  The first column
#'     is the sample number.  The columns Xwww contains the infra red light
#'     amount at wavelength www.  The response variables are fat, protein,
#'     lactose and dm (drymatter).
#' @keywords datasets
#' @examples
#' 	
#' data(NIRmilk)
#' 
"NIRmilk"


#' Weight and size of 20 potatoes
#' 
#' Weight and size of 20 potatoes. Weight in grams; size in milimeter. There
#' are two sizes: \code{length} is the longest length and \code{width} is the
#' shortest length across a potato. #' 
#' 
#' @name data-potatoes
#' @docType data
#'
#' @format A data frame with 20 observations on the following 3 variables.
#'   \describe{
#'     \item{\code{weight}}{a numeric vector}
#'     \item{\code{length}}{a numeric vector}
#'     \item{\code{width}}{a numeric vector}
#'   }
#' 
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @source My own garden; autumn 2015.
#' @keywords datasets
#' @examples
#' 
#' data(potatoes)
#' plot(potatoes) 
#' 
"potatoes"


