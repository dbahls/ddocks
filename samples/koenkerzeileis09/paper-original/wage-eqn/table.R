require(Hmisc)
JAEtable <- function(x, FoM= NULL, digits = 3, ...){
	tab <- lapply(x,coef)
	mnames <- sapply(tab, function(x) dimnames(x)[[1]])
	unames <- unique(unlist(mnames))
	nunames <- length(unames)
	index <- sapply(mnames,match,table=unames)
	A <- matrix("", length(unames), length(tab))
	unames <- paste("$",unames,"$",sep="")
	dimnames(A) <- list(unames,paste("M",1:length(tab),sep=""))
	for(i in 1:length(tab)){
   	   vari <- index[[i]]
   	   tabi <- format(round(tab[[i]],digits = digits))
   	   for(j in 1:length(vari)){
		k <- vari[j]
		A[k,i] <- paste("$\\underset{(",tabi[j,2],")}{",tabi[j,1],"}$", sep = "")
		}
	   }
   	FoM <- format(round(FoM,digits = digits))
	for(i in 1:nrow(FoM)){
	   for(j in 1:ncol(FoM)){
		FoM[i,j] <- paste("$",FoM[i,j],"$",sep="")
		}
	    }
	tab <- rbind(A,FoM)
	rowlabel = "Covariates"
	# Cluj-y col.just used to avoid alignment problems for negative entries.
	latex.default(tab, rowlabel = rowlabel, col.just = rep("r",ncol(tab)), 
		n.rgroup = c(nrow(A),nrow(FoM)))
	}
rk <- read.csv("../rk.csv")
names(rk)[2:4] <- c("m", "y", "z")
rk1 <- glm(y ~ log(z), weights = 1/m, family = quasipoisson, data = rk)
rk2 <- glm(y ~ log(z) + I(log(z)^2), weights = 1/m, family = quasipoisson,
  data = rk)
rk3 <- glm(y ~ log(z) + I(log(z)^2), weights = 1/m, family = quasipoisson,
  data = rk, subset = z <= 250000)
rk4 <- glm(y ~ log(log(z)), weights = 1/m, family = quasipoisson, data = rk)
dispersion <- function(object, type = "deviance")
  sum(residuals(object, type = type)^2)/df.residual(object)
rk_nb <- glm.nb(y ~ log(log(z)), data = rk, weights = max(m)/m)


srk <- lapply(list(rk1,rk2,rk3,rk4),function(x) summary(x, dispersion = dispersion(x)))

# Figures of Merit for the four models
dispD  <- unlist(lapply(list(rk1,rk2,rk3,rk4), dispersion ))
dispP  <- unlist(lapply(list(rk1,rk2,rk3,rk4), dispersion, type = "pearson"))
parsity <- c(coef(rk1)[2], coef(rk2)[2] + 2 * coef(rk2)[3] * log(1000),
    coef(rk3)[2] + 2 * coef(rk3)[3] * log(1000), coef(rk4)[2]/log(1000))
names(parsity) <- NULL
FoM <- as.matrix(rbind(dispD, dispP, parsity))
dimnames(FoM)[[1]] <- c( "$\\sigma_D^2$", "$\\sigma_P^2$", "$\\pi (1000)$")

tab <- JAEtable(srk, FoM)
tab$style <- "amsmath"
