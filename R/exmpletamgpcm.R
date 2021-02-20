#############################################################################
# EXAMPLE 27: IRT parameterization for generalized partial credit model (GPCM) in TAM
#############################################################################

#--- read item parameters
pars <- as.numeric(miceadds::scan.vec(
  "0.19029 1.25309 0.51737 -1.77046 0.94803
  0.19407 1.22680 0.34986 -1.57666 1.29726
  -0.00888 1.07093 0.31662 -1.38755 1.14809
  -0.33810 1.08205 0.48490 -1.56696 0.79547
  -0.18866 0.99587 0.37880 -1.37468 0.81114" ))

print(pars)
pars <- matrix( pars, nrow=5, byrow=TRUE)
print(pars)
beta <- pars[,1]
print(beta)
alpha <- pars[,5]
print(alpha)
tau <- pars[,2:4]
print(tau)

#--- data simulation function for GPCM
sim_gpcm_irt_param <- function(alpha, beta, tau, N, mu=0, sigma=1)
{
  theta <- stats::rnorm(N, mean=mu, sd=sigma)
  I <- length(beta)
  K <- ncol(tau)
  dat <- matrix(0, nrow=N, ncol=I)
  colnames(dat) <- paste0("I",1:I)
  for (ii in 1:I){
    probs <- matrix(0, nrow=N, ncol=K+1)
    for (kk in 1:K){
      probs[,kk+1] <- probs[,kk] + alpha[ii]*( theta - beta[ii] - tau[ii,kk] )
    }
    probs <- exp(probs)
    probs <- probs/rowSums(probs)
    rn <- stats::runif(N)
    cumprobs <- t(apply(probs,1,cumsum))
    for (kk in 1:K){
      dat[,ii] <- dat[,ii] + ( rn > cumprobs[,kk] )
    }
  }
  return(dat)
}

#-- simulate data
N <- 20000     # number of persons
set.seed(98)
dat1 <- sim_gpcm_irt_param(alpha=alpha, beta=beta, tau=tau, N=N, mu=0, sigma=1)
head(dat1)

#* generate design matrix for IRT parameterization
A1 <- TAM::.A.PCM2( resp=dat1)

#- estimate GPCM model
xsi1<-beta*alpha
xsi2<-as.vector(t(tau[,1:2]*alpha))
xsi<-c(xsi1,xsi2)
B.fixed<-matrix(0,15,4)
B.fixed[,3]=1
B.fixed[1:3,1]=1
B.fixed[4:6,1]=2
B.fixed[7:9,1]=3
B.fixed[10:12,1]=4
B.fixed[13:15,1]=5
B.fixed[1:3,2]=1:3
B.fixed[4:6,2]=1:3
B.fixed[7:9,2]=1:3
B.fixed[10:12,2]=1:3
B.fixed[13:15,2]=1:3
B.fixed[1:3,4]=(0:2)*alpha[1]
B.fixed[4:6,4]=(0:2)*alpha[2]
B.fixed[7:9,4]=(0:2)*alpha[3]
B.fixed[10:12,4]=(0:2)*alpha[4]
B.fixed[13:15,4]=(0:2)*alpha[5]
print(B.fixed)



print(cbind(1:length(xsi),xsi))
mod1 <- TAM::tam.mml.2pl( resp=dat1, irtmodel="GPCM", A=A1,xsi.fixed = cbind(1:length(xsi),xsi)
                          ,B.fixed = B.fixed)
summary(mod1)

# compare true and estimated slope estimates (alpha)
cbind( alpha, mod1$B[,2,] )

# compare true and estimated item difficulties (beta)
cbind( beta, mod1$xsi$xsi[1:5] / mod1$B[,2,1] )

# compare true and estimated tau parameters
cbind( tau[,-3], matrix( mod1$xsi$xsi[-c(1:5)], nrow=5, byrow=TRUE )  / alpha )

## End(Not run)
