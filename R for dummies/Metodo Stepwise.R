ModeloREGRE = function(data){
  
  base1 = data.frame(base)
  
  #DISCRIMINAMOS VARIABLES QUE NO CORRELACIONAN CON LA Y
  
  #---#---#---#---#---#---#
  
  dime = dim(base1)
  
  obs = dime[1]
  
  d = dime[2]
  
  nomb = names(base1)
  
  covaY = matrix(0,1,d)
  
  #obtenemos las correlaciones entre la Y vs. las Xs.
  
  for (i in 1 : d){
    
    covaY[i] = cor(base1[1],base1[i])
    
  }
  
  #Obtenemos las Xs NO correlacionadas
  
  
  k = 0
  
  p = 0.25
  
  max1 = matrix(0,1,d)
  
  
  
  for (i in 1:d){
    
    if (covaY[i] > -p & covaY[i] < p) {
      
      k = k+1
      
      max1[i] = i
      
    }
    
  }
  
  
  
  #Mostramos las Xs NO correlacionadas con Y
  
  max1 = replace(max1,max1==0,NA)
  
  max1 <- max1[is.na(max1) == F]
  
  max1
  
  
  
  covaNOY = matrix(0,1,k)
  
  
  
  for (i in 1:length(max1)){
    
    covaNOY[i] = cor(base1[1],base1[max1[i]])
    
    names(covaNOY[i]) = c(nomb[max1[i]])
    
  }
  
  
  
  #Eliminados las Xs NO correlacionadas de base1
  
  base2 = base1
  
  for (i in 1:length(max1)){
    
    base2 = base2[ ,!colnames(base2)==nomb[max1[i]]]
    
  }
  
  
  
  dim(base2)
  
  names(base2) 
  
  names(base2)[1] = "Y"
  
  
  
  #2°CORREMOS EL MODELO STEPWISE
  
  #---#---#---#---#---#---#
  
  modelostep = lm(formula = Y ~ 1 , data = base2)
  
  modelofull = lm(formula = Y ~ . , data = base2)
  
  
  
  Stepmodel = step(modelostep, scope = list(lower = modelostep, upper = modelofull), direction = "both", trace = 1, steps = 1000)
  
  
  
  summary(Stepmodel)
  
}