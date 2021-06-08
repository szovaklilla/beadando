# beadando

setwd("C:/Users/lilla/Desktop")
suicide <- read.csv("master.csv", encoding="UTF-8")
str(suicide)

table(suicide$year)

suicide100_by_year<-aggregate(suicides.100k.pop ~ year, suicide, mean, na.rm=T)

aggregate(suicides.100k.pop ~ year, suicide, mean)

aggregate(suicides.100k.pop ~ year, suicide[16183:16564, ], mean)

country_factor <- factor(suicide$X.U.FEFF.country)

ize <- aggregate(year ~ country_factor, suicide, sum)

which(suicide == "Brazil")

mexico <- aggregate(suicides.100k.pop ~ year, suicide[16565:16936, ], mean)
argentina <- aggregate(suicides.100k.pop ~ year, suicide[589:960, ], mean)
brazil <- aggregate(suicides.100k.pop ~ year, suicide[4173:4544, ], mean)
chile <- aggregate(suicides.100k.pop ~ year, suicide[5265:5636, ], mean)
ecuador <- aggregate(suicides.100k.pop ~ year, suicide[7695:8066, ], mean)
colombia <- aggregate(suicides.100k.pop ~ year, suicide[5637:6008, ], mean)




			plot(ecuador, type="o", col="red", xlab="Évek", ylab="100 ezer főre jutó öngyilkosság", ylim=c(3,18),main="100 ezer főre jutó öngyilkosságok átlaga dél-amerikai országokban (1985-2015)", pch=16, lwd=1.6, cex.lab=1.5, cex.axis=1.5)
			lines(colombia, type = "o", col = "blue", pch=16, lwd=1.9)
			lines(brazil, type="o", col="green", pch=16, lwd=1.9)
			lines(mexico, type="o", col="orange", pch=16, lwd=1.9)
			lines(chile, type="o", col="magenta", pch=16, lwd=1.9)
			lines(argentina, type="o", col="aquamarine", pch=16, lwd=1.9)
			legend("topleft", legend=c("Ecuador","Kolumbia","Brazília","Mexikó","Chile","Argentína"), col=c("red","blue","green","orange","magenta","aquamarine"), cex=0.8, title="Országok", text.font=2, pch=16, lwd=2.7, box.lty=1, inset=.06)

nemek<-aggregate(suicides.100k.pop ~ year + sex, suicide, mean)


mexico_nemek <- aggregate(suicides.100k.pop ~ year+sex, suicide[16565:16936, ], mean)
ecuador_nemek<-aggregate(suicides.100k.pop ~ year + sex, suicide[7695:8066, ], mean)
brazil_nemek <- aggregate(suicides.100k.pop ~ year+sex, suicide[4173:4544, ], mean)
colombia_nemek <- aggregate(suicides.100k.pop ~ year+sex, suicide[5637:6008, ], mean)
chile_nemek <- aggregate(suicides.100k.pop ~ year + sex, suicide[5265:5636, ], mean)
argentina_nemek <- aggregate(suicides.100k.pop ~ year + sex, suicide[589:960, ], mean)

str(mexico_nemek)

mexico_ferfiak<-mexico_nemek[32:62, ]
ecuador_ferfiak<-ecuador_nemek[32:62, ]
brazil_ferfiak<-brazil_nemek[32:62, ]
colombia_ferfiak<-colombia_nemek[32:62, ]
chile_ferfiak<-chile_nemek[32:62, ]
argentina_ferfiak<-argentina_nemek[32:62, ]


plot(x=ecuador_ferfiak[ ,1], y=ecuador_ferfiak[ ,3], type="o",xlab="Évek", ylab="100 ezer főre jutó öngyilkosság", ylim=c(5.5,20), pch=16, lwd=2, cex.axis=0.8, cex.lab=0.9, col="red", main="100 ezer főre jutó öngyilkosság átlaga dél-amerikai országokban a férfiak körében(1985-2015)")
			lines(x=colombia_ferfiak[ ,1], y=colombia_ferfiak[ ,3], type="o", col = "blue", pch=16, lwd=2)
			lines(x=brazil_ferfiak[ ,1],y=brazil_ferfiak[ ,3], type="o", col="green", pch=16, lwd=2)
			lines(x=mexico_ferfiak[ ,1],y=mexico_ferfiak[ ,3], type="o", col="orange", pch=16, lwd=2)
			lines(x=argentina_ferfiak[ ,1],y=argentina_ferfiak[ ,3], type="o", col="aquamarine", pch=16, lwd=2)
			lines(x=chile_ferfiak[ ,1],y=chile_ferfiak[ ,3], type="o", col="magenta", pch=16, lwd=2)
			legend("topleft", legend=c("Ecuador","Kolumbia","Brazília","Mexikó","Argentína","Chile"), col=c("red","blue","green","orange","aquamarine","magenta"), cex=0.8, title="Országok", text.font=2, pch=16, lwd=2.5, box.lty=1, inset=.06)
			

mexico_nok<-mexico_nemek[1:31, ]
ecuador_nok<-ecuador_nemek[1:31, ]
brazil_nok<-brazil_nemek[1:31, ]
colombia_nok<-colombia_nemek[1:31, ]
chile_nok<-chile_nemek[1:31, ]
argentina_nok<-argentina_nemek[1:31, ]


plot(x=ecuador_nok[ ,1], y=ecuador_nok[ ,3], type="o",xlab="Évek", ylab="100 ezer főre jutó öngyilkosság", ylim=c(0,15), pch=16, lwd=2, cex.axis=0.8, cex.lab=0.9, col="red", main="100 ezer főre jutó öngyilkosság átlaga dél-amerikai a nők körében(1985-2015)")
			lines(x=colombia_nok[ ,1], y=colombia_nok[ ,3], type="o", col = "blue", pch=16, lwd=2)
			lines(x=brazil_nok[ ,1],y=brazil_nok[ ,3], type="o", col="green", pch=16, lwd=2)
			lines(x=mexico_nok[ ,1],y=mexico_nok[ ,3], type="o", col="orange", pch=16, lwd=2)
			lines(x=chile_nok[ ,1],y=chile_nok[ ,3], type="o", col="magenta", pch=16, lwd=2)
			lines(x=argentina_nok[ ,1],y=argentina_nok[ ,3], type="o", col="aquamarine", pch=16, lwd=2)
			legend("topleft", legend=c("Ecuador","Kolumbia","Brazília","Mexikó","Chile","Argentína"), col=c("red","blue","green","orange","magenta","aquamarine"), cex=0.8, title="Országok", text.font=2, pch=16, lwd=2.5, box.lty=1, inset=.06)

generaciok<-aggregate(suicides.100k.pop ~ year + generation, suicide, mean)

ggplot(data = generaciok,aes(x=year,y=suicides.100k.pop,col=generation, group=generation))+ geom_line()+ geom_point() + labs(title="100 ezer főre jutó öngyilkosságok száma generációk szerint (1985-2015)", y="100 ezer főre jutó öngyilkosság", x="Évek")+
coord_cartesian(xlim=c(1985, 2015)) + theme_classic() 


#sorrendbe rendezése az öngyilkosságoknak 

suicide100_by_year[order(suicide100_by_year$suicides.100k.pop, ]

leg6 <- aggregate(suicides.100k.pop ~ X.U.FEFF.country + year, suicide, mean)

leg6[order(leg6$suicides.100k.pop), ]

Kazakhstan        30.5112821
12                       Belarus        31.0759127
41                       Hungary        32.7615161
76            Russian Federation        34.8923765
88                     Sri Lanka        35.2951515
53                     Lithuania

belarus<-subset(leg6, X.U.FEFF.country=="Belarus")
hungary<-subset(leg6, X.U.FEFF.country=="Hungary")
russia<-subset(leg6, X.U.FEFF.country=="Russian Federation")
srilanka<-subset(leg6, X.U.FEFF.country=="Sri Lanka")
litvania<-subset(leg6, X.U.FEFF.country=="Lithuania")
kazakhstan<-subset(leg6, X.U.FEFF.country=="Kazakhstan")

legnagyobb6<- rbind(belarus, hungary, russia, srilanka, litvania, kazakhstan)

ggplot(legnagyobb6, aes(x=X.U.FEFF.country, y=suicides.100k.pop, group=year, fill=year)) + 
geom_bar(position="dodge" ,stat="identity") + scale_y_continuous(expand=c(0,0)) + theme_classic() + 
labs(fill="Országok", y="100 ezer főre jutó öngyilkosságok", x="Országok", title="100 ezer főre jutó öngyilkosságok átlaga országonként évszámok szerint")+ 
scale_fill_gradient(low="pink4", high="moccasin") + 
scale_x_discrete(labels=c("Fehéroroszország", "Magyarország", "Kazahsztán", "Litvánia", "Oroszország", "Sri Lanka"))


# generációk szerint
leg6_gen<- aggregate(suicides.100k.pop ~ X.U.FEFF.country + generation, suicide, mean)

belarus_gen<-subset(leg6_gen, X.U.FEFF.country=="Belarus")
hungary_gen<-subset(leg6_gen, X.U.FEFF.country=="Hungary")
russia_gen<-subset(leg6_gen, X.U.FEFF.country=="Russian Federation")
srilanka_gen<-subset(leg6_gen, X.U.FEFF.country=="Sri Lanka")
litvania_gen<-subset(leg6_gen, X.U.FEFF.country=="Lithuania")
kazakhstan_gen<-subset(leg6_gen, X.U.FEFF.country=="Kazakhstan")

legnagyobb6_gen <- rbind(belarus_gen, hungary_gen, russia_gen, srilanka_gen, litvania_gen, kazakhstan_gen)

ggplot(legnagyobb6_gen, aes(x=X.U.FEFF.country, y=suicides.100k.pop, group=generation, fill=generation)) + 
geom_bar(position="dodge" ,stat="identity") + scale_y_continuous(expand=c(0,0)) + theme_classic() + 
labs(fill="Országok", y="100 ezer főre jutó öngyilkosságok", x="Országok", title="100 ezer főre jutó öngyilkosságok átlaga országonként generációk szerint")+  
scale_x_discrete(labels=c("Fehéroroszország", "Magyarország", "Kazahsztán", "Litvánia", "Oroszország", "Sri Lanka"))

###LEFUTTATÁSOK#####

png(file = "ongyilk_orszag.png", width = 2048, height = 768, units = "px")


plot(ecuador, type="o", col="red", xlab="Évek", ylab="100 ezer főre jutó öngyilkosság", ylim=c(3,18),main="100 ezer főre jutó öngyilkosságok átlaga dél-amerikai országokban (1985-2015)", cex.main=2, pch=19, lwd=2.2, cex.lab=1.5, cex.axis=1.5)
			lines(colombia, type = "o", col = "blue", pch=19, lwd=2.2)
			lines(brazil, type="o", col="green", pch=19, lwd=2.2)
			lines(mexico, type="o", col="orange", pch=19, lwd=2.2)
			lines(chile, type="o", col="magenta", pch=19, lwd=2.2)
			lines(argentina, type="o", col="aquamarine", pch=19, lwd=2.2)
			legend("topleft", legend=c("Ecuador","Kolumbia","Brazília","Mexikó","Chile","Argentína"), col=c("red","blue","green","orange","magenta","aquamarine"), cex=1.4, title="Országok", text.font=2, pch=16, lwd=2.7, box.lty=1, inset=.06, horiz=TRUE)

dev.off()

png(file = "ongyilk_orszag_ferfi.png", width = 2048, height = 768, units = "px")

plot(x=ecuador_ferfiak[ ,1], y=ecuador_ferfiak[ ,3], type="o",xlab="Évek", ylab="100 ezer főre jutó öngyilkosság", ylim=c(5.5,25), pch=19, lwd=2.2, cex.axis=1.5, cex.lab=1.5, col="red", main="100 ezer főre jutó öngyilkosság átlaga dél-amerikai országokban a férfiak körében(1985-2015)", cex.main=2)
			lines(x=colombia_ferfiak[ ,1], y=colombia_ferfiak[ ,3], type="o", col = "blue", pch=19, lwd=2.2)
			lines(x=brazil_ferfiak[ ,1],y=brazil_ferfiak[ ,3], type="o", col="green", pch=19, lwd=2.2)
			lines(x=mexico_ferfiak[ ,1],y=mexico_ferfiak[ ,3], type="o", col="orange", pch=19, lwd=2.2)
			lines(x=argentina_ferfiak[ ,1],y=argentina_ferfiak[ ,3], type="o", col="aquamarine", pch=19, lwd=2.2)
			lines(x=chile_ferfiak[ ,1],y=chile_ferfiak[ ,3], type="o", col="magenta", pch=19, lwd=2.2)
			legend("topleft", legend=c("Ecuador","Kolumbia","Brazília","Mexikó","Chile","Argentína"), col=c("red","blue","green","orange","magenta","aquamarine"), cex=1.4, title="Országok", text.font=2, pch=19, lwd=2.7, box.lty=1, inset=.06, horiz=TRUE)

dev.off()

png(file = "ongyilk_orszag_nok.png", width = 2048, height = 768, units = "px")

plot(x=ecuador_nok[ ,1], y=ecuador_nok[ ,3], type="o",xlab="Évek", ylab="100 ezer főre jutó öngyilkosság", ylim=c(0,15), pch=19, lwd=2.2, cex.axis=1.5, cex.lab=1.5, col="red", cex.main=2, main="100 ezer főre jutó öngyilkosság átlaga dél-amerikai országokban a nők körében(1985-2015)")
			lines(x=colombia_nok[ ,1], y=colombia_nok[ ,3], type="o", col = "blue", pch=19, lwd=2.2)
			lines(x=brazil_nok[ ,1],y=brazil_nok[ ,3], type="o", col="green", pch=19, lwd=2.2)
			lines(x=mexico_nok[ ,1],y=mexico_nok[ ,3], type="o", col="orange", pch=19, lwd=2.2)
			lines(x=chile_nok[ ,1],y=chile_nok[ ,3], type="o", col="magenta", pch=19, lwd=2.2)
			lines(x=argentina_nok[ ,1],y=argentina_nok[ ,3], type="o", col="aquamarine", pch=19, lwd=2.2)
			legend("topleft", legend=c("Ecuador","Kolumbia","Brazília","Mexikó","Chile","Argentína"), col=c("red","blue","green","orange","magenta","aquamarine"), cex=1.4, title="Országok", text.font=2, pch=19, lwd=2.7, box.lty=1, inset=.06, horiz=TRUE)
			
png(file = "ongyilk_evek_gen.png", width = 2048, height = 768, units = "px")
			
ggplot(data = generaciok,aes(x=year,y=suicides.100k.pop,col=generation, group=generation))+ 
geom_line(size=1.5)+ geom_point(size=4) + labs(title="100 ezer főre jutó öngyilkosságok átlaga generációk szerint (1985-2015)", y="100 ezer főre jutó öngyilkosság", x="Évek")+
coord_cartesian(xlim=c(1985, 2015)) + theme_classic() + 
theme(
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 20),
  axis.title.y = element_text(size = 20), 
  axis.text.y = element_text(size = 20),
  plot.title = element_text(size = 25),
  legend.title = element_text(size = 18),
  legend.text = element_text(size = 18))

dev.off()

png(file = "ongyilk_leg6_evek.png", width = 2048, height = 768, units = "px")
  
ggplot(legnagyobb6, aes(x=X.U.FEFF.country, y=suicides.100k.pop, group=year, fill=year)) + 
geom_bar(position="dodge" ,stat="identity") + scale_y_continuous(expand=c(0,0)) + theme_classic() + 
labs(fill="Országok", y="100 ezer főre jutó öngyilkosságok", x="Országok", title="100 ezer főre jutó öngyilkosságok átlaga országonként évszámok szerint")+ 
scale_fill_gradient(low="pink4", high="moccasin") + 
scale_x_discrete(labels=c("Fehéroroszország", "Magyarország", "Kazahsztán", "Litvánia", "Oroszország", "Sri Lanka"))+
theme(
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 20),
  axis.title.y = element_text(size = 20), 
  axis.text.y = element_text(size = 20),
  plot.title = element_text(size = 25),
  legend.title = element_text(size = 18),
  legend.text = element_text(size = 18))  
dev.off()

png(file = "ongyilk_leg6_gen.png", width = 2048, height = 768, units = "px")
ggplot(legnagyobb6_gen, aes(x=X.U.FEFF.country, y=suicides.100k.pop, group=generation, fill=generation)) + 
geom_bar(position="dodge" ,stat="identity") + scale_y_continuous(expand=c(0,0)) + theme_classic() + 
labs(fill="Országok", y="100 ezer főre jutó öngyilkosságok", x="Országok", title="100 ezer főre jutó öngyilkosságok átlaga országonként generációk szerint")+  
scale_x_discrete(labels=c("Fehéroroszország", "Magyarország", "Kazahsztán", "Litvánia", "Oroszország", "Sri Lanka")) + 
theme(
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 20),
  axis.title.y = element_text(size = 20), 
  axis.text.y = element_text(size = 20),
  plot.title = element_text(size = 25),
  legend.title = element_text(size = 18),
  legend.text = element_text(size = 18))
dev.off()


#ÖSSZESÍTÉS#
par(mfrow=c(3,1))

plot(ecuador, type="o", col="red", xlab="Évek", ylab="100 ezer főre jutó öngyilkosság", ylim=c(3,18),main="100 ezer főre jutó öngyilkosságok átlaga dél-amerikai országokban (1985-2015)", pch=16, lwd=1.9, cex.lab=1.5, cex.axis=1.5)
			lines(colombia, type = "o", col = "blue", pch=16, lwd=1.9)
			lines(brazil, type="o", col="green", pch=16, lwd=1.9)
			lines(mexico, type="o", col="orange", pch=16, lwd=1.9)
			lines(chile, type="o", col="magenta", pch=16, lwd=1.9)
			lines(argentina, type="o", col="aquamarine", pch=16, lwd=1.9)
			legend("topleft", legend=c("Ecuador","Kolumbia","Brazília","Mexikó","Chile","Argentína"), col=c("red","blue","green","orange","magenta","aquamarine"), cex=0.8, title="Országok", text.font=2, pch=16, lwd=2.7, box.lty=1, inset=.06)
			
plot(x=ecuador_ferfiak[ ,1], y=ecuador_ferfiak[ ,3], type="o",xlab="Évek", ylab="100 ezer főre jutó öngyilkosság", ylim=c(5.5,25), pch=19, lwd=2, cex.axis=1.5, cex.lab=1.5, col="red", main="100 ezer főre jutó öngyilkosság átlaga dél-amerikai országokban a férfiak körében(1985-2015)", cex.main=2)
			lines(x=colombia_ferfiak[ ,1], y=colombia_ferfiak[ ,3], type="o", col = "blue", pch=19, lwd=2)
			lines(x=brazil_ferfiak[ ,1],y=brazil_ferfiak[ ,3], type="o", col="green", pch=19, lwd=2)
			lines(x=mexico_ferfiak[ ,1],y=mexico_ferfiak[ ,3], type="o", col="orange", pch=19, lwd=2)
			lines(x=argentina_ferfiak[ ,1],y=argentina_ferfiak[ ,3], type="o", col="aquamarine", pch=19, lwd=2)
			lines(x=chile_ferfiak[ ,1],y=chile_ferfiak[ ,3], type="o", col="magenta", pch=19, lwd=2)
			legend("topleft", legend=c("Ecuador","Kolumbia","Brazília","Mexikó","Chile","Argentína"), col=c("red","blue","green","orange","magenta","aquamarine"), cex=1.4, title="Országok", text.font=2, pch=19, lwd=2.7, box.lty=1, inset=.06, horiz=TRUE)

plot(x=ecuador_nok[ ,1], y=ecuador_nok[ ,3], type="o",xlab="Évek", ylab="100 ezer főre jutó öngyilkosság", ylim=c(0,15), pch=16, lwd=2, cex.axis=0.8, cex.lab=0.9, col="red", main="100 ezer főre jutó öngyilkosság átlaga dél-amerikai a nők körében(1985-2015)")
			lines(x=colombia_nok[ ,1], y=colombia_nok[ ,3], type="o", col = "blue", pch=16, lwd=2)
			lines(x=brazil_nok[ ,1],y=brazil_nok[ ,3], type="o", col="green", pch=16, lwd=2)
			lines(x=mexico_nok[ ,1],y=mexico_nok[ ,3], type="o", col="orange", pch=16, lwd=2)
			lines(x=chile_nok[ ,1],y=chile_nok[ ,3], type="o", col="magenta", pch=16, lwd=2)
			lines(x=argentina_nok[ ,1],y=argentina_nok[ ,3], type="o", col="aquamarine", pch=16, lwd=2)
			legend("topleft", legend=c("Ecuador","Kolumbia","Brazília","Mexikó","Chile","Argentína"), col=c("red","blue","green","orange","magenta","aquamarine"), cex=0.8, title="Országok", text.font=2, pch=16, lwd=2.5, box.lty=1, inset=.06)
