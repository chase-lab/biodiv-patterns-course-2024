##	This code is only suitable for symmetric dispersal.  
##	asymmetric dispersal call function to determine connectivity 
##	matrix each year of a simulation with a new value of adv or
## 	diffusion.


rm(list=ls())
library(MASS);library(geoR)

## demographic parameters
mu.R<-1.9	# mean log(fecundity) in reserves
sig2.R<-2.2	# var(log(fecundity)) in reserves

##	patch parameters
patches<-2					# number of patches
w<-1						# patch width
subpops<-1:patches			# identify subpopulations
d<-seq(0,8,by=0.3)			# distance separating patch edges 
d.Cx<-seq(1,9,by=0.3)		# distance separating patch centres 

## 	oceanographic parameters
adv.d<-0					# advection
diff<-6						# diffusion

##	range parameter for correlation function
phi <- 5	##	gaussian

maxt<-1000	# time period for each simulation
numsim<-100	# number of simulations

##	array to store subpopulations
nmat<-array(dim=c(length(d),maxt,patches,numsim),
            dimnames=list(NULL,NULL,as.character(subpops),NULL))

##	initial subpop sizes
init.subpops<-rep(500,patches)

##	matrix to store long-term metapopulation growth rate
Rhat<-matrix(NA,nrow=numsim,ncol=length(d))

##	Define dispersal functions
Gaussian<-function(x){Dx<-(1/diff)*(2*pi)^(-0.5)*exp(-(x-adv.d)^2/(2*diff^2))}
Laplacian<-function(x){Dx<-exp(-abs(x-adv.d)/diff)/(diff*2)}


##	Function to return connectivity matrix
##	Function accepts the kernel to be integrated, number of 
##	patches, and patch width
conn.mat<-function(disp.fn,patches,w,dist){
	# determine local retention. Larvae released in the centre
	# of each patch
	local.rec<-numeric()
	for(j in 1:patches){
		local.rec[j]<-integrate(disp.fn,-(w/2),(w/2))$value
	}
	# determine successful between patch dispersal (larvae 
	# released in the centre of each patch)
	disp<-numeric()
	for(i in 1:patches){
		disp[i]<-integrate(disp.fn,(w/2)+dist,((3*w)/2)+dist)$value
	}
	cm<-matrix(NA,patches,patches)
	diag(cm)<-local.rec
	cm[1,2]<-disp[1]
	cm[2,1]<-disp[2]
	return(cm)
}

##	test call to connectivity matrix function (will be in loop for
##	increasing distance)
conn.mat(Laplacian,patches,w,d[10])


##	function to determine variance-covariance matrix
##	arguments: distance between patches, variance in each patch,
## 	number of patches
var.cov<-function(dist,variance,patches,phi){
	mat<-matrix(0,patches,patches)
	diag(mat)<-variance
	mat[1,2]<-mat[2,1]<-exp(-(dist/phi)^2)*variance[1]
	return(mat)
}
variance<-rep(sig2.R,patches)
##	call to var.cov function (to be in loop for increasing 
##	spacing)
var.cov(d[21],variance,patches,phi)


##	Loop for increasing reserve spacing
for(j in 1:length(d)){
	
	# calculate dispersal matrix
	x.Dx<-d[j];print(x.Dx)
	conn.matrix<-conn.mat(Laplacian,patches,w,x.Dx)
	
	# calculate environmental covariance of per-capita fecundity in each patch
	x.Cx<-d.Cx[j]
	covar.logf<-var.cov(x.Cx,variance,patches,phi)
	
	for(i in 1:numsim){
	  # for each simulation
		# randomly chose fA and fB
		logf.samp<-mvrnorm(n=maxt,mu=rep(mu.R,2),Sigma=covar.logf)
		
		for(yr in 1:maxt){
		  # for each year, first calculate per-capita fecundity in each patch
			f.A<-exp(logf.samp[yr,1])
			f.B<-exp(logf.samp[yr,2])
			
			# construct projection matrix
			projmat<-matrix(NA,2,2)
			projmat[1,1]<-conn.matrix[1,1]*f.A
			projmat[2,2]<-conn.matrix[2,2]*f.B
			projmat[1,2]<-conn.matrix[1,2]*f.B
			projmat[2,1]<-conn.matrix[2,1]*f.A	
			if(yr==1) nmat[j,yr,,i]<-projmat%*%(init.subpops)
			if(yr>1) nmat[j,yr,,i]<-projmat%*%nmat[j,yr-1,,i] 
		}
		# steps to calculate expected long-term metapopuation growth rate
		years<-1:(maxt-100)
		# discard first 100 years
		nvals<-rowSums(nmat[j,-(1:100),,i]) 
											# years, sum subpops
		# remove zeroes (set to NA)
		nvals[nvals==0]<-NA
		# calculate expected long-term metapopuation growth rate
		fit<-lm(log(nvals)~years)
		# store growth rate for plotting
		Rhat[i,j]<-fit$coef["years"]
	}
}

# calculate the mean expected metapopulation growth rate 
# across all of the simulations, and combine with interpatch distance

optimal_spacing <- cbind.data.frame(d = d,
                                    Rhat = exp(colMeans(Rhat)))

library(ggplot2)
ggplot() +
  stat_smooth(data = optimal_spacing,
              aes(x = d, y = Rhat)) +
  labs(x = 'Distance between patches',
       y = 'Expected metapopulation growth rate')

# Exercise: reproduce the qualitative outcomes shown in Figure 1
# Exercise: extend to varying connectivity (hint: what has to change when connectivity varies?)
