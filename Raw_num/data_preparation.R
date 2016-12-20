##### DATA PREPARATION
############################
prefix = "~/Documents/CODING/DATA_ANALYSIS/R2T2/"
#########
##### F2
#########
mdata <-
  read.csv(file.path(prefix, "Data_All_161110_F2/CSV/Sheet_2.csv"))
rdata = mdata[, c(
  "ID.personne.interrogée",
  "ID.collecteur" ,
  "Date.de.début",
  "Date.de.fin" ,
  "Adresse.IP",
  "Âge"
)]
attach(mdata)
stargazer(rdata, title = 'Summary statistics of activities metadata')

mdata <- mdata[, colSums(is.na(mdata)) < nrow(mdata)]

## country
library(devtools)

library(rjson)
rdata$country =  apply(rdata['Adresse.IP'],  1, function(x)
  freegeoip(x)$country_name)

## sexe
rdata$gender <- paste(mdata$Sexe, mdata$X)

merge_same_res <- function(dataf, substringn) {
  df1 <- dataf[, grepl(substringn, names(dataf))]
  df1[, ncol(df1)] = sub("", "CCC", df1[, ncol(df1)])
  ind <- apply(df1 , 1 , function(x)
    any(x > 0))
  sub("CCC", "", do.call(paste, c(as.list(df1[ind ,]), sep = "")))
}

rdata$past_chatt = merge_same_res(mdata, "J.avais.déjà.chatté.sur.Internet")
rdata$past_streaming_video = merge_same_res(mdata, "J.avais.déjà.vu.du.streaming.vidéo")
rdata$past_robot = merge_same_res(mdata, "J.avais.déjà.fait.des.activités.avec.des.robots")

rdata$en_communication = merge_same_res(mdata, "En.communication")
rdata$en_programmation = merge_same_res(mdata, "En.programmation.du.robot")
rdata$en_gestion = merge_same_res(mdata, "En.gestion.de.groupe")
rdata$en_organisation = merge_same_res(mdata, "En.organisation.du.travail")

rdata$prog_ennuyant =  merge_same_res(mdata, "La.programmation.c.est.ennuyant")
rdata$pratik_amel =  merge_same_res(mdata, "En.pratiquant..je.m.améliore.en.programmation")
rdata$meilleur_prog =  merge_same_res(mdata,
                                      "En.comparaison.a.d.autres.étudiants..je.suis.un.bon.progammeur")
rdata$plus_de_robotik =  merge_same_res(mdata,
                                        "Je.souhaiterai.avoir.plus.de.temps.pour.la.robotique.a.l.école")
rdata$filles_moins_prog =  merge_same_res(mdata, "Les.filles.aiment.moins.programmer.que.les.garcons")
rdata$aime_prog =  merge_same_res(mdata, "J.aime.ce.que.je.programme")
rdata$prog_fun_team =  merge_same_res(mdata, "La.progammation.est.plus.amusante.en.équipe")
rdata$robotik_interet = merge_same_res(mdata, "La.robotique.m.intéresse")
rdata$hard_robotik = merge_same_res(mdata, "La.robotique.est.difficile")
rdata$hard_prog = merge_same_res(mdata, "La.programmation.est.difficile")
rdata$useless_challenge = merge_same_res(mdata,
                                         "J.ai.appris.beaucoup.de.choses.inutiles.en.préparant.ce.challenge")
rdata$happy_help = merge_same_res(mdata,
                                  "Je.suis.content.e..de.l.aide.reçu.pour.la.mission.R2T2")
rdata$good_team = merge_same_res(mdata,
                                 "Nous.avons.eu.une.bonne.ambiance.de.groupe.durant.la.mission")
rdata$serious_input = merge_same_res(
  mdata,
  "Mes.suggestions.étaient.prise.au.sérieux.par.les.autres.membres.de.mon.équipe"
)
rdata$ask_adult =  merge_same_res(mdata, "Je.demandais.à.un.adulte.si.j.avais.un.problème")
rdata$ask_teamate =  merge_same_res(mdata,
                                    "Je.discutais.avec.mon.équipe.si.j.avais.un.problème")
rdata$student_help_robotik =  merge_same_res(mdata, "En.robotique.les.étudiants.s.entre.aident")
rdata$influence_teamates =  merge_same_res(mdata, "J.influençais.les.décisions.prises.par.mon.équipe")
rdata$suggestions_followed =  merge_same_res(mdata, "Toutes.mes.suggestions.étaient.suivies")
rdata$take_initiatives =  merge_same_res(mdata, "Je.prenais.des.initiatives")
rdata$solo_worker =  merge_same_res(
  mdata,
  "Je.ne.demandais.pas.toujours.l.avis.de.mes.co.équipiers.pour.faire.quelque.chose"
)
rdata$split_work =  merge_same_res(
  mdata,
  "Nous.nous.sommes.réparti.de.façon.équitable.entre.tous.les.membres.de.l.équipe"
)
rdata$unorganized_team =  merge_same_res(mdata, "Notre.équipe.etait.bruyante.et.désordonée")

rdata$trained_forr2t2 =  merge_same_res(
  mdata,
  "Est.ce.que.tu.t.es.entrainé.e..spécifiquement.pour.cette.journée..en.groupe.ou.individuellement"
)

rdata  =  cbind(rdata, mdata[, c(115:143)])
rdata  =  cbind(rdata, mdata[, c(146:155)])

write.csv(rdata,
          file.path(prefix, "Data_All_161110_F2/CSV/Sheet_2_sorted.csv"))



#########
##### F
#########
mdata <- read.csv(file.path(prefix, "Raw_num/CSV/Sheet_F.csv"))
rdata = mdata[, c(
  "ID.personne.interrogée",
  "ID.collecteur" ,
  "Date.de.début",
  "Date.de.fin" ,
  "Adresse.IP",
  "Âge"
)]
attach(mdata)
stargazer(rdata, title = 'Summary statistics of activities metadata')


mdata <- mdata[, colSums(is.na(mdata)) < nrow(mdata)]

## country
library(devtools)

library(rjson)
rdata$country =  apply(rdata['Adresse.IP'],  1, function(x)
  freegeoip(x)$country_name)

## sexe
rdata$gender <- paste(mdata$Sexe, mdata$x, sep = "")
rdata$gender <- sub("NA", "", rdata$gender)
rdata$gender <- sub("1", "male", rdata$gender)
rdata$gender <- sub("2", "female", rdata$gender)

merge_same_res <- function(dataf, substringn) {
  df1 <- dataf[, grepl(substringn, names(dataf))]
  df1[, ncol(df1)] = gsub("NA", "", df1[, ncol(df1)])
  ind <- apply(df1 , 1 , function(x)
    any(x > 0))
  gsub("NA", "", do.call(paste, c(as.list(df1[ind ,]), sep = "")))
}

rdata$past_chatt =  merge_same_res(mdata, "J.avais.déjà.chatté.sur.Internet")
rdata$past_streaming_video = merge_same_res(mdata, "J.avais.déjà.vu.du.streaming.vidéo")
rdata$past_robot = merge_same_res(mdata, "J.avais.déjà.fait.des.activités.avec.des.robots")

rdata$en_communication = sub("NA", "", merge_same_res(mdata, "En.communication"))
rdata$en_programmation = merge_same_res(mdata, "En.programmation.du.robot")
rdata$en_gestion = merge_same_res(mdata, "En.gestion.de.groupe")
rdata$en_organisation = merge_same_res(mdata, "En.organisation.du.travail")

rdata$prog_ennuyant =  merge_same_res(mdata, "La.programmation.c.est.ennuyant")
rdata$pratik_amel =  merge_same_res(mdata, "En.pratiquant..je.m.améliore.en.programmation")
rdata$meilleur_prog =  merge_same_res(mdata,
                                      "En.comparaison.a.d.autres.étudiants..je.suis.un.bon.progammeur")
rdata$plus_de_robotik =  merge_same_res(mdata,
                                        "Je.souhaiterai.avoir.plus.de.temps.pour.la.robotique.a.l.école")
rdata$filles_moins_prog =  merge_same_res(mdata, "Les.filles.aiment.moins.programmer.que.les.garcons")
rdata$aime_prog =  merge_same_res(mdata, "J.aime.ce.que.je.programme")
rdata$prog_fun_team =  merge_same_res(mdata, "La.progammation.est.plus.amusante.en.équipe")
rdata$robotik_interet = merge_same_res(mdata, "La.robotique.m.intéresse")
rdata$hard_robotik = merge_same_res(mdata, "La.robotique.est.difficile")
rdata$hard_prog = merge_same_res(mdata, "La.programmation.est.difficile")
rdata$useless_challenge = merge_same_res(mdata,
                                         "J.ai.appris.beaucoup.de.choses.inutiles.en.préparant.ce.challenge")
rdata$happy_help = merge_same_res(mdata,
                                  "Je.suis.content.e..de.l.aide.reçu.pour.la.mission.R2T2")
rdata$good_team = merge_same_res(mdata,
                                 "Nous.avons.eu.une.bonne.ambiance.de.groupe.durant.la.mission")
rdata$serious_input = merge_same_res(
  mdata,
  "Mes.suggestions.étaient.prise.au.sérieux.par.les.autres.membres.de.mon.équipe"
)
rdata$ask_adult =  merge_same_res(mdata, "Je.demandais.à.un.adulte.si.j.avais.un.problème")
rdata$ask_teamate =  merge_same_res(mdata,
                                    "Je.discutais.avec.mon.équipe.si.j.avais.un.problème")
rdata$student_help_robotik =  merge_same_res(mdata, "En.robotique.les.étudiants.s.entre.aident")
rdata$influence_teamates =  merge_same_res(mdata, "J.influençais.les.décisions.prises.par.mon.équipe")
rdata$suggestions_followed =  merge_same_res(mdata, "Toutes.mes.suggestions.étaient.suivies")
rdata$take_initiatives =  merge_same_res(mdata, "Je.prenais.des.initiatives")
rdata$solo_worker =  merge_same_res(
  mdata,
  "Je.ne.demandais.pas.toujours.l.avis.de.mes.co.équipiers.pour.faire.quelque.chose"
)
rdata$split_work =  merge_same_res(
  mdata,
  "Nous.nous.sommes.réparti.de.façon.équitable.entre.tous.les.membres.de.l.équipe"
)
rdata$unorganized_team =  merge_same_res(mdata, "Notre.équipe.etait.bruyante.et.désordonée")




rdata$trained_forr2t2 =  merge_same_res(
  mdata,
  "Est.ce.que.tu.t.es.entrainé.e..spécifiquement.pour.cette.journée..en.groupe.ou.individuellement"
)

rdata  =  cbind(rdata, mdata[, c(114:142)])
rdata  =  cbind(rdata, mdata[, c(145:154)])

write.csv(rdata, file.path(prefix, "Raw_num/CSV/Sheet_F_sorted.csv"))



#########
##### E
#########
mdata <- read.csv(file.path(prefix, "Raw_num/CSV/Sheet_E.csv"))
rdata = mdata[, c(1:5, 12)]
attach(mdata)
stargazer(rdata, title = 'Summary statistics of activities metadata')

## country
library(devtools)

library(rjson)
rdata$country =  apply(rdata['IP.Address'],  1, function(x)
  freegeoip(x)$country_name)


## sexe
rdata$gender <- paste(mdata$Gender, mdata$X, sep = "")
rdata$gender <- sub("NA", "", rdata$gender)
rdata$gender <- sub("1", "male", rdata$gender)
rdata$gender <- sub("2", "female", rdata$gender)

merge_same_res <- function(dataf, substringn) {
  df1 <- dataf[, grepl(substringn, names(dataf))]
  df1[, ncol(df1)] = gsub("NA", "", df1[, ncol(df1)])
  ind <- apply(df1 , 1 , function(x)
    any(x > 0))
  gsub("NA", "", do.call(paste, c(as.list(df1[ind ,]), sep = "")))
}

rdata$past_chatt =  merge_same_res(mdata, ".already.did.some.chat.on.Internet")
rdata$past_streaming_video = merge_same_res(mdata, "I.already.saw.video.streaming.events")
rdata$past_robot = merge_same_res(mdata, "I.had.already.done.activities.with.robots")

rdata$en_communication = sub("NA", "", merge_same_res(mdata, "In.communication"))
rdata$en_programmation = merge_same_res(mdata, "In.programming.the.robot")
rdata$en_gestion = merge_same_res(mdata, "In.managing.the.team")
rdata$en_organisation = merge_same_res(mdata, "In.organizing.the.work")

rdata$prog_ennuyant =  merge_same_res(mdata, "Programming.is.boring")
rdata$pratik_amel =  merge_same_res(mdata, "When.I.practise.my.programming.improves")
rdata$meilleur_prog =  merge_same_res(mdata, "Compared.to.other.students.I.am.a.good.programmer")
rdata$plus_de_robotik =  merge_same_res(mdata, "I.wish.I.had.more.time.for.robotics.at.school")
rdata$filles_moins_prog =  merge_same_res(mdata, "Girls.enjoy.programming.less.than.boys.do")
rdata$aime_prog =  merge_same_res(mdata, "I.like.what.I.program")
rdata$prog_fun_team =  merge_same_res(mdata, "Programming.is.more.fun.when.it.s.in.team")
rdata$robotik_interet = merge_same_res(mdata, "I.m.interested.in.robotics")
rdata$hard_robotik = merge_same_res(mdata, "Robotics.is.difficult")
rdata$hard_prog = merge_same_res(mdata, "Programming.is.difficult")
rdata$useless_challenge = merge_same_res(mdata,
                                         "I.ve.learned.a.lot.of.unnecessary.things.preparing.the.challenge")
rdata$happy_help = merge_same_res(mdata,
                                  "I.am.pleased.with.the.support.I.have.received.for.the.R2t2.mission")
rdata$good_team = merge_same_res(mdata, "We.have.a.nice..positive.group.environment")
rdata$serious_input = merge_same_res(mdata,
                                     "My.suggestions.are.taken.seriously.by.other.members.of.my.team")
rdata$ask_adult =  merge_same_res(mdata, "I.spoke.to.the.teacher.if.I.had.a.problem")
rdata$ask_teamate =  merge_same_res(mdata, "I.discussed.with.my.teammates.if.I.had.a.problem")
rdata$student_help_robotik =  merge_same_res(mdata,
                                             "Robotics.is.a.subject.where.students.help.each.other")
rdata$influence_teamates =  merge_same_res(mdata, "I.was.influencing.decision.taken.in.my.team")
rdata$suggestions_followed =  merge_same_res(mdata, "All.my.suggestions.were.followed")
rdata$take_initiatives =  merge_same_res(mdata, ".was.taking.initiatives.in.the.group")
rdata$solo_worker =  merge_same_res(mdata,
                                    "I.was.not.always.asking.my.teammates.their.opinion.in.what.I.was.doing")
rdata$split_work =  merge_same_res(mdata,
                                   "We.splitted.the.work.equally.between.the.team.members")
rdata$unorganized_team =  merge_same_res(mdata, "Our.team.was.noisy.and.disordely")

rdata$trained_forr2t2 =  merge_same_res(
  mdata,
  "Have.you.got.some.specific.training..programming.robotics..for.this.day..in.a.group.or.individually"
)

rdata  =  cbind(rdata, mdata[, c(143:172)])
rdata  =  cbind(rdata, mdata[, c(175:184)])

write.csv(rdata, file.path(prefix, "Raw_num/CSV/Sheet_E_sorted.csv"))



############
### MERGE
############
edata <-
  read.csv(file.path(prefix, "Raw_num/CSV/Sheet_E_sorted.csv"))
fdata <-
  read.csv(file.path(prefix, "Raw_num/CSV/Sheet_F_sorted.csv"))

edata$language = 'english'
fdata$language = 'french'

library(plyr)
tdata = rbind(fdata, setNames(edata, names(fdata)))




write.csv(tdata,
          file.path(prefix, "Raw_num/CSV/Sheet_all_sorted_bis.csv"))

#### fix not same num
tdata <-
  read.csv(file.path(prefix, "Raw_num/CSV/Sheet_all_sorted.csv"))
edata = subset.data.frame(tdata, language == "english")
fdata = subset.data.frame(tdata, language == "french")
library(plyr)
# Strongly agree	, Agree	Programming , Disagree	, Strongly , Don’t know
revalue(x,
        c(
          "1" = "strg_agree",
          "2" = "agree",
          "3" = "disagree",
          "4" = "strg_disagree",
          "5" = "NA"
        ))
summary(as.factor(edata$useless_challenge))
for (i in names(edata[18:40])) {
  x = factor(edata[[names(edata[i])]])
  print(levels(as.factor(x)))
  print(names(edata[i]))
  if ("5" %in% levels(x)) {
    x =  revalue(x, c("5" = "NA"))
  }
  if ("1" %in% levels(x)) {
    x =  revalue(x, c("1" = "strg_agree"))
  }
   if ("2" %in% levels(x)) {
     x =revalue(x, c("2" = "agree"))
  }
  if ("3" %in% levels(x)) {
    x= revalue(x, c("3" = "disagree"))
  }
  if ("4" %in% levels(x)) {
    x= revalue(x, c("4" = "strg_disagree"))
  }

  print(levels(as.factor(x)))
  
   if ("strg_agree" %in% levels(x)) {
     x =  revalue(x, c("strg_agree" = 4))
  }
   if ("agree" %in% levels(x)) {
     x =  revalue(x, c("agree" = 3))
  }
 if ("disagree" %in% levels(x)) {
   x =  revalue(x, c("disagree" = 2))
  }
  if ("strg_disagree" %in% levels(x)) {
    x =  revalue(x, c("strg_disagree" = 1))
  }
  #x =             if ("NA" %in% levels(x)) {
  #  revalue(x, c("NA" = "5"))
  #}
  print(levels(as.factor(x)))
  #x = ifelse(length(levels(x)) ==5,
  #revalue(x, c("1"="strg_agree", "2"="agree", "3"="disagree", "4"="strg_disagree", "5"="NA" )),
  #revalue(x, c("1"="strg_agree", "2"="agree", "3"="disagree", "4"="strg_disagree" )))
  #x = revalue(x, c("strg_agree"="4", "agree"="3", "disagree"="2", "strg_disagree"="1", "5"="NA" ))
  edata[[names(edata[i])]] = x
}
summary(as.factor(edata$useless_challenge))


tdata = rbind(fdata, setNames(edata, names(fdata)))
write.csv(tdata,
          file.path(prefix, "Raw_num/CSV/Sheet_all_sorted_bis.csv"))
