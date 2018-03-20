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



