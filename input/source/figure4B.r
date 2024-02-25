fit_model = .alrna(n.model)
set.seed(34)
for(mw in 1:n.model){
    formula1 = as.formula(.p(id.model$outcome[mw],"~",id.model$predictor[mw]))
    formula0 = as.formula(.p(id.model$outcome[mw],"~1"))
    restricted = gamlss(formula=formula0,data=dataw,family=id.model$family[mw])    
    fit_model[[mw]]   = gamlss(formula=formula1,data=dataw,family=id.model$family[mw],
                               control=gamlss.control(n.cyc=200))
    id.model$gR2[mw]  = Rsq(fit_model[[mw]])
    id.model$GAIC[mw] = GAIC(fit_model[[mw]])
    id.model$LRT[mw]  = LR.test(restricted,fit_model[[mw]],print=FALSE)$p.val   
    }
save(n.model,id.model,file=.p(path.rdata,"figure4B"))
