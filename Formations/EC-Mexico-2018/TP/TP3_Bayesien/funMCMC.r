
## MCMC
mcmc.MH=function(n, Y, parinit, dynamics, obsprocess, prior, proposal,
	subsample=1){
	## prior=list(ddistrib,parfunction=list())
	## proposal=list(ddistrib,rdistrib,parfunction=list())
	## dynamics=function(...){ ... }
	## obsprocess=list(ddistrib,parfunction=list()) # ddistrib=densité, rdistrib=échantillons tirés par fonctions R du type rnorm, runif...

	## loglikelihood function
  	if(length(obsprocess$parfunction)==1){
  		loglikelihood=function(u){ sum(obsprocess$ddistrib(Y,obsprocess$parfunction(u),log=TRUE)) } #somme car on est en log, équivaut au produit à l'échelle naturelle
  	} else {
  		if(length(obsprocess$parfunction)==2){
  			loglikelihood=function(u){ 
  				sum(obsprocess$ddistrib(Y,obsprocess$parfunction[[1]](u),
  					obsprocess$parfunction[[2]](u),log=TRUE)) 
  			}
  		}
  	}
  	## logprior function
  	if(length(prior$parfunction)==1){
  		logprior=function(u){ sum(prior$ddistrib(u,prior$parfunction(),log=TRUE)) }
  	} else {
  		if(length(prior$par)==2){
  			logprior=function(u){ 
  				sum(prior$ddistrib(u,prior$parfunction[[1]](),prior$parfunction[[2]](),log=TRUE)) 
  			}
  		}
  	}
  	## logproposal function and generator under the proposal distribution
  	if(length(proposal$parfunction)==1){
  		logproposal=function(par1,par2){ 
  			sum(proposal$ddistrib(par2,proposal$parfunction(par1),log=TRUE)) 
  		}
  		rproposal=function(par){ 
  			proposal$rdistrib(length(par),proposal$parfunction(par)) 
  		}
  	} else {
  		if(length(proposal$parfunction)==2){
  			logproposal=function(par1,par2){ 
  				sum(proposal$ddistrib(par2,proposal$parfunction[[1]](par1),
  					proposal$parfunction[[2]](par1), log=TRUE)) 
  			}
  			rproposal=function(par){ 
  				proposal$rdistrib(length(par),proposal$parfunction[[1]](par),
  					proposal$parfunction[[2]](par)) 
  			}
  		}
  	}
	
	## initialization
  	par.i=parinit
  	lpri=logprior(par.i)
  	dyn=dynamics(parinit)
  	llik=loglikelihood(dyn)
  	k=0
  	save=rbind(c(par.i,llik,lpri,llik+lpri,0))
  	print(c(0,save))
	write.table(save,"save.txt",col.names=FALSE,row.names=FALSE)
  	
  	## MCMC iterations
  	for(i in 2:n){
  		parprop=rproposal(par.i)
  		lpriprop=logprior(parprop)
  		if(lpriprop>-Inf){
  			dynprop=dynamics(parprop)
  			llikprop=loglikelihood(dynprop)
  			lpro=logproposal(par.i,parprop)
  			lproprop=logproposal(parprop,par.i)
    		pr=min(1,exp(llikprop-llik+lpriprop-lpri+lpro-lproprop))
    	} else {
    		pr=0
    	}
    	if(is.na(pr)){ pr=0 }
    	if(runif(1)<pr){ 
    		## if proposed parameters accepted
    		par.i=parprop
    		llik=llikprop
    		lpri=lpriprop
    		k=k+1
    	}
    	if(i/subsample==round(i/subsample)){
    		## save iteration
      		save=rbind(c(par.i,llik,lpri,llik+lpri,k/i))
      		print(c(i,save))
		    write.table(save,"save.txt",append=TRUE,col.names=FALSE,row.names=FALSE)
      	}	
  	}
  	return(NULL)
}
