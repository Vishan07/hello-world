\name{cars.lm}
\alias{cars.lm}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(cars.lm)}
\format{
  The format is:
List of 12
 $ coefficients : Named num [1:2] -17.58 3.93
  ..- attr(*, "names")= chr [1:2] "(Intercept)" "speed"
 $ residuals    : Named num [1:50] 3.85 11.85 -5.95 12.05 2.12 ...
  ..- attr(*, "names")= chr [1:50] "1" "2" "3" "4" ...
 $ effects      : Named num [1:50] -303.914 145.552 -8.115 9.885 0.194 ...
  ..- attr(*, "names")= chr [1:50] "(Intercept)" "speed" "" "" ...
 $ rank         : int 2
 $ fitted.values: Named num [1:50] -1.85 -1.85 9.95 9.95 13.88 ...
  ..- attr(*, "names")= chr [1:50] "1" "2" "3" "4" ...
 $ assign       : int [1:2] 0 1
 $ qr           :List of 5
  ..$ qr   : num [1:50, 1:2] -7.071 0.141 0.141 0.141 0.141 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:50] "1" "2" "3" "4" ...
  .. .. ..$ : chr [1:2] "(Intercept)" "speed"
  .. ..- attr(*, "assign")= int [1:2] 0 1
  ..$ qraux: num [1:2] 1.14 1.27
  ..$ pivot: int [1:2] 1 2
  ..$ tol  : num 1e-07
  ..$ rank : int 2
  ..- attr(*, "class")= chr "qr"
 $ df.residual  : int 48
 $ xlevels      : Named list()
 $ call         : language lm(formula = dist ~ speed, data = cars)
 $ terms        :Classes 'terms', 'formula' length 3 dist ~ speed
  .. ..- attr(*, "variables")= language list(dist, speed)
  .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:2] "dist" "speed"
  .. .. .. ..$ : chr "speed"
  .. ..- attr(*, "term.labels")= chr "speed"
  .. ..- attr(*, "order")= int 1
  .. ..- attr(*, "intercept")= int 1
  .. ..- attr(*, "response")= int 1
  .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
  .. ..- attr(*, "predvars")= language list(dist, speed)
  .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. ..- attr(*, "names")= chr [1:2] "dist" "speed"
 $ model        :'data.frame':	50 obs. of  2 variables:
  ..$ dist : num [1:50] 2 10 4 22 16 10 18 26 34 17 ...
  ..$ speed: num [1:50] 4 4 7 7 8 9 10 10 10 11 ...
  ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 dist ~ speed
  .. .. ..- attr(*, "variables")= language list(dist, speed)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "dist" "speed"
  .. .. .. .. ..$ : chr "speed"
  .. .. ..- attr(*, "term.labels")= chr "speed"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
  .. .. ..- attr(*, "predvars")= language list(dist, speed)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "dist" "speed"
 - attr(*, "class")= chr "lm"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(cars.lm)
## maybe str(cars.lm) ; plot(cars.lm) ...
}
\keyword{datasets}
