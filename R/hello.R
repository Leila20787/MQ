#' number
#' @param data
#' @export

diabete_positive=function(data){
  cpt<-0
  for (i in 1:nrow(data)){
    if (data$Outcome[i] == 1) cpt=cpt+1
  };
  print(paste("le nombre de femme qui ont la diabète est",cpt))
}

#' diabete enceinte
#' @param data
#'  @export

diabete_enceinte=function(data){
  cpt<-0
  for (i in 1:nrow(data)){
    if (data$Outcome[i] == 1 & data$Pregnancies[i]!=0  ) cpt=cpt+1
  };
  print(paste("le nombre de femme qui ont la diabète et enceinte au moins une fois   est",cpt))
}

#' Info
#' @param data
#' @export
info=function(data){
  hist(data, prob=T, main="data");
  densite <- density(data)
  lines(densite, col = "red",lwd=3)
  abline(v=quantile(data),col="green",lwd=3)
}

#' tension
#' @param data
#' @export
tension_ad=function(data){
  cpt<-0
  for (i in 1:nrow(data)){
    if (data$BloodPressure[i] >85) cpt=cpt+1
  };
  print(paste("le nombre de femme qui ont la tension artérielle diastolique supérieure à 85 (mm Hg) est",cpt))
}





