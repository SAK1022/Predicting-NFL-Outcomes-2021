Winner<-function(var1,var2){
  + attach(NFLDataFinal3)
  + BigModel<-lm(W~PPGF+PPGA+YdsTF+YdsTA+Oplays+Dplays+OYperP+OTO+DTO+FDF+CMPF+CMPA+PYF+PYA+PTDF+PTDA+RTDF+ORYperA+DRYperA+PenB+DScPct,data=NFLDataFinal3)
  + Wins=fitted.values(BigModel)
  + Value<-(Wins*1/16)+OppositeSOS+OSnapsRetained+DSnapsRetained
  + comparisontable=cbind(Tm,Value)
  + teamnames=c("Arizona Cardinals", "Atlanta Falcons", "Baltimore Ravens", "Buffalo Bills", "Carolina Panthers", "Chicago Bears", "Cincinnati Bengals" , "Cleveland Browns", "Dallas Cowboys", "Denver Broncos", "Detroit Lions", "Green Bay Packers", "Houston Texans", "Indianapolis Colts", "Jacksonville Jaguars", "Kansas City Chiefs", "Las Vegas Raiders", "Los Angeles Chargers", "Los Angeles Rams", "Miami Dolphins", "Minnesota Vikings", "New England Patriots", "New Orleans Saints", "New York Giants", "New York Jets", "Philadelphia Eagles", "Pittsburgh Steelers", "San Francisco 49ers","Seattle Seahawks", "Tampa Bay Buccaneers", "Tennessee Titans", "Washington Football Team")
  + looktable1=cbind(teamnames, Tm)
  + for (i in 1:length(teamnames)) if (var1==looktable1[i,1]) varone=comparisontable[i,2]
  + for (i in 1:length(teamnames)) if (var2==looktable1[i,1]) vartwo=comparisontable[i,2]
  + if (varone>vartwo) print(paste(var1, "win!")) else if (vartwo>varone) print(paste(var2, "win!") ) else print("It is a tie!")}