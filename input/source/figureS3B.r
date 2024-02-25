
n.b = 500
n.k = 5

# libraries
suppressMessages(library(gamlss))

##
## useful functions
##
rmse.fun = function(pred, obs, na.rm = FALSE){
    # Root Mean Squared Error (RMSE)
    sqrt(mean((pred - obs)^2, na.rm = na.rm))
    }
    
rsquare.fun = function (pred, obs, type = "adj", n=NA, p=1, na.rm = FALSE){
    # R-square
    r2 = 1 - (sum((obs -pred)^2, na.rm = TRUE)/((n - 1)*var(obs, na.rm =TRUE)))
    if(type=="adj"){
        #p  = length(fit$coef)-1
        1-(1-r2)*(n-1)/(n-p-1)
    }else{
        r2
    }
}        
# lm
lm.fun = function(formula,train,test,p,logscale=FALSE){
    # formula = formula.m;p=1;logscale=FALSE
    fit  = lm(formula,data=train)
    pred = if(!logscale){predict(fit,newdata=test,type="response")
           }else{exp(predict(fit,newdata=test,type="response"))}
    # crit
    n   = nrow(test)
    c1  = rsquare.fun(pred,test[,colnames(fit[[12]])[1]],n=n,type="raw")
    c2  = rmse.fun(pred,test[,colnames(fit[[12]])[1]])
    # out
    c(c1,c2)
    }
# resample
resample <- function(x, ...) x[sample.int(length(x), ...)]

##
## object to fill
##
ar.crit.mkcb = array(NA,dim=c(n.model,n.k,2,n.b),
                    dimnames=list(.p(id.model$outcome,"-",id.model$predictor),
                                  1:n.k,c("R2","RMSE"),1:n.b))


##
## fill
##
for(bw in 1:n.b){# bw=1

    # simulated lrt under null hypothesis: no effect of predictor
    cat("b =",bw,"\n")
    set.seed(bw)            

    #
    pos.section_rodent = split(dataw$pos,dataw$rodent)
    n.rodent   = length(pos.section_rodent)
    n.rodent_k = n.rodent/n.k


    # choice of stratification
    mx.value.qo = apply(dataw[,id.outcome$id],2,quantile,prob=c(1/3,2/3))
    mx.group.so = matrix(1,nrow=n.dataw,ncol=n.outcome,
                         dimnames=list(dataw$id,id.outcome$id))
    for(qw in 1:nrow(mx.value.qo)){
        for(ow in 1:n.outcome){
            mx.group.so[,ow][dataw[,id.outcome$id[ow]]>mx.value.qo[qw,ow]] = qw+1
            }
        }
    mx.group.so[,1] = c((nrow(mx.value.qo)+1):1)[mx.group.so[,1] ]    
        
    ##
    ## prepare data
    ##
    # select obs per rodent:
    pos.rodent = unlist(sapply(pos.section_rodent,function(x)resample(x,1)))
    quartilew  = quantile(dataw[pos.rodent,id.outcome$id[2]],
                 prob=c(1/3,2/3))
    # group stratification:
    group.rodent = rep(1,length(pos.rodent))
    for(qw in 1:length(quartilew)){
        group.rodent[dataw[pos.rodent,id.outcome$id[2]]>
                     quartilew[qw]] = qw+1
        }
    # assign to k-fold
    posk_quant = matrix(unlist(lapply(split(pos.rodent,group.rodent),
           function(x){
               # x=split(pos.rodent,group.rodent)[[1]]
               n = length(x)
               x = x[order(runif(n))]
               f = floor(n/n.k)
               group.x = rep(NA,n)
               group.x[1:(f*n.k)] = rep(1:n.k,each=f)
               p = which(is.na(group.x))
               group.x[p] = (1:n.k)[order(runif(n.k))][1:length(p)]
               rbind(x,group.x)
               })),
            ncol=2,byrow=TRUE)
        # check: OK!
        # posk_quant = cbind(posk_quant,group.rodent[order(group.rodent)])
        # plot(id.section[posk_quant[,1],id.outcome$id[2]],
        #      col=posk_quant[,3],pch=posk_quant[,2])                 
    pos.rodent_k = split(posk_quant[,1],posk_quant[,2])

    ##
    ## fit
    ##

    # object to fill
    ar.crit.mkc = array(NA,dim=c(n.model,n.k,2),
                        dimnames=list(.p(id.model$outcome,"-",id.model$predictor),
                                      1:n.k,c("R2","RMSE")))
    # for each k
    for(kw in 1:n.k){# kw=1
        # data
        train = dataw[unlist(pos.rodent_k[-kw]),
                      c(id.outcome$id,id.predictor2$id)]
        test  = dataw[unlist(pos.rodent_k[kw]),
                      c(id.outcome$id,id.predictor2$id)]                   
             # check
             all(table(c(rownames(train),rownames(test)))==1)
             
        # formula
        for(mw in 1:n.model){# mw = 1013
            formula.m = as.formula(.p(id.model$outcome[mw],"~",id.model$predictor[mw]))
            temp = try(lm.fun(formula.m,train,test,p=1,logscale=FALSE),silent=TRUE)
            if(class(temp)!="try-error"){ar.crit.mkc[mw,kw,] = temp}
        }# end model
    }# end kw

    # save
    ar.crit.mkcb[,,,bw] = ar.crit.mkc
}


save(ar.crit.mkcb,file=.p(path.rdata,"figureS3B"))    
