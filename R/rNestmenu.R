rNestmenu <-
function(lang="english"){
     if(lang=="english"){
      warninglabl<-"Warning"
      warningtextlabl<-paste0("The technical information contained in rNest is general information that constitutes advice only. All persons must adhere to all pertinent laws (for example provincial or territorial laws), regulations and permit requirements including but not restricted to the Migratory Birds Convention Act, 1994 (MBCA) and the Migratory Birds Regulations (MBR). It is important to note that some species of birds protected under the MBCA have also been listed in Schedule 1 of the Species at Risk Act (SARA). These species receive protection from both the MBCA and SARA. This information does not provide an authorization for harming or killing migratory birds or for the disturbance, destruction or taking of nests or eggs as prohibited under the MBR. This information does not provide a guarantee that the activities will avoid contravening the MBR or other laws and regulations. This is general information not intended to be relied on as official advice concerning the legal consequences of any specific activity. It is not a substitute for the MBCA, the MBR, or any other legislation.","\n","\n","It is the responsibility of individuals and companies to assess their risk with regards to migratory birds and design relevant avoidance and mitigation measures to reduce risks. It is possible that local nesting periods could have a different starting date and/or duration than dates provided in rNest due to micro-climatic conditions in specific areas (e.g. high elevation sites or coastal sites) as well as inter-annual variation due to factors such as early spring or cold, wet summer. rNest will be updated as new data become available, which could result in the changing of dates. Any time nests of migratory bird containing eggs or young are encountered, the immediate area should be avoided until the young have naturally left the vicinity of the nest. This protection measure should be taken even if the nest has been found outside the dates of the general nesting period for the area provided in rNest. Once out of the nest, young birds are still vulnerable therefore precautionary measures are recommended.","\n","\n","For more detailed Avoidance Guidelines, please visit Environment Canada’s website at www.ec.gc.ca/paom-itmb")
      loadingmsglabl="Data and rNest interface loading..."
     }else{
      warninglabl<-"Avertissement"
      warningtextlabl<-paste0("L'information technique contenue dans rNest est de l'information générale ne constituant uniquement qu'un avis. Tous doivent se conformer aux lois pertinentes (e.g. lois provinciales ou territoriales), aux règlements et aux permis requis, incluant entre autre la Loi de 1994 sur la convention concernant les oiseaux migrateurs (LCOM) et le Règlement sur les oiseaux migrateurs (ROM). Il est important de noter que certaines espèces d'oiseaux protégées sous la LCOM sont également listées sous l'annexe 1 de la Loi sur les espèces en péril (LEP). Ces espèces sont protégées à la fois sous la LCOM et la LEP. L'information présentée ici ne fournit aucune autorisation à blesser ou à tuer des oiseaux migrateurs ou à perturber, détruire ou prélever des nids ou des oeufs tel qu'il l'est interdit sous la LCOM. L'information présentée ne fournit pas de garantie que les activités vont éviter de contrevenir au ROM ou aux autres lois et règlements en vigueur. Il s'agit d'information générale qui ne doit pas être considérée comme étant un avis officiel concernant les conséquences légales de toutes activités spécifiques. Il ne s'agit pas d'un substitut à la LCOM, le ROM ou toute autre législation en vigueur.","\n","\n","C'est la responsabilité des individus et des entreprises d'évaluer leurs risques en regards aux oiseaux migrateurs et d'établir des mesures d'évitements et d'atténuations appropriées afin de réduire ces risques. Il est possible que les dates des périodes de nidification locales diffèrent en début ou en durée que ce qui est fourni par rNest en raison des conditions micro-climatiques d'un endroit spécifique (e.g. site en altitude ou site côtier) ou en raison de la variation inter-annuelle associée aux variations climatiques. Le programme rNest sera mis à jour en fonction des nouvelles informations disponibles et les dates fournies pourraient varier. Dès qu'un nid contenant des oeufs ou des jeunes d'oiseaux migrateurs est trouvé, l'endroit immédiat devrait être évité jusqu'à ce que les jeunes quittent les environs du nid de façon naturelle. Cette mesure de protection devrait être appliquée même si un nid est trouvé hors de la période générale de nidification indiquée par rNest. Une fois hors du nid, les jeunes oiseaux sont toujours vulnérables, par conséquent des mesures de précautions sont recommandées.","\n","\n","Pour de plus amples renseignements concernant les Règles en matière d'évitement, consultez le site d'Environnement Canada au www.ec.gc.ca/paom-itm")
      loadingmsglabl="Chargement des données et de l'interface rNest..."
     }
     tkmessageBox(title=warninglabl,message=warningtextlabl, icon="info",type="ok")
     cat("\n","  ",loadingmsglabl,"\n")
     flush.console()
     data(nestwatch)
     data(nestingparameters)
     data(birdgroupings)
     data(ecodistrictscsv)
     data(predictions)
     data(specodistricts)
     x<-dlply(ecodistrictscsv,.(group),function(i){Polygon(i[c(nrow(i),1:nrow(i)),1:2],hole=i$hole[1])})
     x<-sapply(unique(ecodistrictscsv$id),function(i){
      res<-x[names(x)%in%ecodistrictscsv$group[which(ecodistrictscsv$id==i)]]
      Polygons(res,ID=i)
     })
     dat<-ecodistrictscsv[match(names(x),ecodistrictscsv$id),]
     dat<-as.data.frame(do.call("rbind",strsplit(dat$id,"_")))
     names(dat)<-c("PROVINCE","ecodistrict","breeding_zone2","breeding_zone","provincesEN","provincesFR")
     dat<-dat[,-1]
     dat$ecodistrict<-as.numeric(as.character(dat$ecodistrict))
     x<-SpatialPolygons(x,proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
     x<-SpatialPolygonsDataFrame(x,data=dat,match.ID=FALSE)
     ecodistricts<-x
     rNest:::tk.nesting.calendar(ecodistricts=ecodistricts,lang=lang)
     #winMenuAddItem("rNest","single nest","rNesttest:::tk.single.nest()")
     #winMenuAddItem("rNest","single nest test","rNesttest:::tk.single.nest.test()")
     #winMenuAddItem("rNest","nesting calendar","rNesttest:::tk.nesting.calendar()")
     #winMenuAddItem("rNest","explore data","rNesttest:::tk.data()")
     }
