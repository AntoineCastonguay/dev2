library(deSolve)

bact <- read.csv("~/R_projet/BIO401/2Bactéries.csv")

# Créer un nouveau graphique avec la première ligne
plot(x = bact$temps, y = bact$bogustonia_proii, type = "l", col = "blue", 
     xlab = "Temps", ylab = "Données", main = "Graphique avec deux lignes")

# Ajouter la deuxième ligne
lines(x = bact$temps, y = bact$aleastonia_predatora, col = "red")

C_RVector <- function(t, vars, parms){
  with(as.list(c(parms, vars)), {
    # Modèle consommateur ressoure
    dR <- alpha*R - beta*R*C # dR/dt
    dC <- gamma*R*C - sigma*C  # dC/dt
    
    # Résultat
    res <- c(dR=dR, dC=dC)
    return(list(res))
  })
}

dessinSol <- function(ic=c(R=25,C=20), times=bact$temps,func=C_RVector, parms=c(alpha=0.5,beta=0.02,gamma=0.005,sigma=0.75)) {
  soln <- ode(ic, times, func, parms)
  
  correlation_R <- cor(bact$bogustonia_proii, soln[,"R"])
  correlation_C <- cor(bact$aleastonia_predatora, soln[,"C"])
  
  if(correlation_C > 0.6 & correlation_R > 0.6){
    print(paste("Ressource :",correlation_R))
    print(paste("Consomateur :",correlation_C))
    
    plot(x = times, y = soln[,"R"], type = "l", col = "blue", 
         xlab = "Temps", ylab = "Données", 
         main = paste("R", round(correlation_R,3),
                      "C", round(correlation_C,3),
                      "a", parms[1],
                      "b", parms[2],
                      "g", parms[3],
                      "s", parms[4]))
    
    # Ajouter la deuxième ligne
    lines(x = times, y = soln[,"C"], col = "red")
  }
}
# for (a in seq(0.355,0.365,by=0.001)) {
#   for (b in seq(0.155,0.165,by=0.001)) {
#     for (g in seq(0.15,0.25,by=0.01)) {
#       for (s in seq(0.85,0.95,by=0.01)) {
#         try(dessinSol(parms = c(alpha=a,beta=b,gamma=g,sigma=s)))
#       }
#     }
#   }
# }
dessinSol(parms = c(alpha=0.36,beta=0.16,gamma=0.18,sigma=0.86))
# 0.4 0.2 0.2 0.9
  # 0.4 0.19 0.19 0.86
  # 0.35 0.16 0.18 0.89
  # 0.36 0.16 0.18 0.86
# 0.4 0.3 0.1 0.9
# 0.5 0.3 0.1 0.7
# 0.6 0.2 0.2 0.6
# 0.7 0.1 0.4 0.7
# 0.9 0.2 0.1 0.3


