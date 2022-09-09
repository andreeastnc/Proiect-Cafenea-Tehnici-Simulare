# install the sets package
install.packages("sets") 
# install the ggplot2 package
install.packages("ggplot2") 

library(sets)
library(ggplot2)



#aplicam metoda inversa pe densitatea data 


inversa2 <-function(){
  
  U <-runif(1,0,1)
  x <- -1
  
  if(U > 0 && U < 1){
    u <- exp(-(x^3)/5)
    x <- (log(1/(1-U)) * 5)^(1/3)
    return (x)
  }
  if(U == 0)
    x <- 0
  return (x)
  
}


#Cazul v.a. discrete
#simulam valori din v.a. Y1 data folosind metoda inversa
inversa1 <- function() {
  U <- runif(1, 0, 1)
  result <- 0
  
  #returnam rezultatul in functie de valoarea generata
  if(U<0.1){
    result <- 1
    return(result)
  }
  
  
  if(TRUE && U<0.6 && TRUE && U >=0.1){
    result <- 2
    return(result)
  }
  if(TRUE && U<0.65 && TRUE && U >=0.6){
    result <- 3
    return(result)
  }
  if(TRUE && U<0.825 && TRUE && U >=0.65){
    result <- 4
    return(result)
  }
  if(TRUE && U<1 && TRUE && U >=0.825){
    result <- 5
    return(result)
  }
  
}



#functie care returneaza bacsisul aleator
functie_bacsis <- function() {
  U <- runif(1, 0, 1)
  result <- 0
  
  #returnam rezultatul dupa valoarea generata
  if(U<0.1){
    result <- 1
    return(result)
  }
  if(TRUE && U<0.5 && TRUE && U >=0.1){
    result <-1.5
    return(result)
  }
  if(TRUE && U<0.7 && TRUE && U >=0.5){
    result <- 2.5
    return(result)
  }
  if(TRUE && U<0.85 && TRUE && U >=0.7){
    result <- 3
    return(result)
  }
  if(TRUE && U<1 && TRUE && U >=0.85){
    result <- 3.5
    return(result)
  }
  
}




#functia in care returnam "limita rabdarii" pe care o are clientul cu v.a.
functie_rabdare <- function() {
  U <- runif(1, 0, 1)
  result <- 0
  
  #Analizam variabila aleatoare pe ramuri pentru a genera rezultatul
  if(U<0.1){
    result <- 3.4
    return(result)
  }
  if(TRUE && U<0.4 && TRUE && U >=0.1){
    result <-3.3
    return(result)
  }
  if(TRUE && U<0.65 && TRUE && U >=0.4){
    result <- 4.8
    return(result)
  }
  if(TRUE && U<0.825 && TRUE && U >=0.65){
    result <- 4.5
    return(result)
  }
  if(TRUE && U<1 && TRUE && U >=0.825){
    result <- 6
    return(result)
  }
  
}

#functie care returneaza valoarea platii in functie de timpul de asteptare
#la coada serverului 2
functie_cash <- function(a) {
  
  U <- a
  result <- 0
  #returnam rezultatul
  if(U<1){
    result <- 3
    return(result)
  }
  if(TRUE && U<2 && TRUE && U >=1){
    result <-4
    return(result)
  }
  if(TRUE && U<3 && TRUE && U >=2){
    result <- 5
    return(result)
  }
  if(TRUE && U<4 && TRUE && U >=3){
    result <- 6
    return(result)
  }
  if(TRUE && U<7 && TRUE && U >=4){
    result <- 8
    return(result)
  }
  
}

#dupa algoritmul de generare Ts
generareTs <- function(s)
{
  t <- s
  ok <- 1
  while(ok == 1){
    
    U1 <- runif(1, 0, 1)
    U2 <- runif(1, 0, 1)
    lambdaT <- 0
    
    #am dat valoarea maxima pe care o poate lua functia 
    lambda <- 31.33
    
    #lambaT primeste valoarea in functie de ramura pe care se afla t in functia data
    if(TRUE && t <=3 && TRUE && 0<=t){
      lambdaT <- cos(2*t) + sin(5*t) + 5*t
    }
    
    if(TRUE && t < 12 && TRUE && 3 < t){
      lambdaT <- 4^(0.2*t) + 5*t^0.3 + 14
    }
    
    if(12 <= t){
      lambdaT <- 20
    }
    
    t <- t - (1/lambda) * (log(U1))
    
    if(U2 <= lambdaT/lambda)
    {
      Ts <- t
      ok <- 0
      return(Ts)
    }
  }
}


#salvam pentru plot valorile de timp sosire si plecare
#pentru fiecare server 
TTimpiSosireServer1 <- rep(0,0)
TTimpiPlecareServer1 <- rep(0,0)
primulClientPierdut <-0

TTimpiSosireServer2 <- rep(0,0)
TTimpiPlecareServer2 <- rep(0,0)

#date pentru creare plot venit pe zi,
#bacsis, numarul de clienti pierduti si numarul clientilor pe zi

TVenitMediuZilnic <- rep(0,0)
TBacsisZilnic <- rep(0,0)
TClientiPierduti <- rep(0,0)
NrClientiZilnici <- rep(0,0)
mediuClientiPierdutiServer1 <- 0
mediuClientiPierdutiServer2 <- 0
#Facem analiza cafenelei pe o durata de 6 luni 
numarZile = 180

#timpul minim maxim, mediue de asteptare la fiecare server
medieServer1 <- 0
medieServer2 <- 0
minServer1 <- 5
minServer2 <- 5
maxServer1 <- 0
maxServer2 <- 0

for (contor in 1:numarZile)
{
  #Initializam
  
  t <- 0 #Var de timp
  nA <- 0 #var contor
  nD <- 0 #var contor
  n1 <- 0 #nr de clienti de la serverul 1
  n2 <- 0 #nr de clienti de la serverul 2
  
  
  A1 <- rep(0,0) #timpul la care soseste clientul in serverul 1
  A2 <- rep(0,0) #timpul la care soseste clinetul in serverul 2
  D <- rep(0,0) # timpul la care clientul paraseste sistemul
  
  
  tA <- generareTs(0) # momentul sosirii urmatorului client
  #in cazul in care nu exista niciun client la serverul i, valoarea timpului va fi reprezentata cu 21474
  t1 <- 30000 #timpul cand clientul va finaliza intercatiunea cu serverul 1, init cu inf
  t2 <- 30000 #timpul cand clientul va finaliza intercatiunea cu serverul 2, init cu inf
  
  #EL lista de evenimente
  EL <- 1:3
  EL[1] <- tA
  EL[2] <- t1
  EL[3] <- t2
  
  timpiSosireServer1 <- rep(0,0) #salvam momentul sosirii unui client la server 1
  timpiPlecareServer1 <- rep(0,0) #salvam momentul plecarii unui client la server 1
  countServer1 <- 1 #numarul de persoane care trec prin serverul 1
  
  T <- 900 # T este timpul de lucru pe zi, transformat in minute, programul fiind de 15 ore pe zi
  
  tVenitMediu <- rep(0,0)
  tBacsis <- 0
  
  
  #incrementam timpul de sosire pentru prima persoana la serverul 1
  timpiSosireServer1[countServer1] = tA
  
  #venit mediu zilnic
  venitMediu <- 0
  clientiPierduti <-0
  
  #cat timp nu s-a terminat programul
  while (t < T){
    
    #cazul 1 : soseste un client nou si verificam daca serverul 1 este liber, altfel trebuie pus in coada de asteptare
    # si n1 nu depepaseste capacitatea maxima de 10
    
    if(tA == min(EL)){
      t <-  tA
      nA <- nA + 1
      n1 <- n1 + 1
      tA <- generareTs(t)
      EL[1]<-tA
      
      #clientul care tocmai a sosit va fi servit imediat
      if(n1 == 1){
        
        y1 <- inversa1()
        
        rabdare <- functie_rabdare()
        #daca timpul de asteptare generat este mai mare decat rabdarea pe care o are clientul
        #el pleaca
        if(y1 > rabdare){
          if(primulClientPierdut == 0){
            primulClientPierdut = t + rabdare
          }
          clientiPierduti <- clientiPierduti + 1
          mediuClientiPierdutiServer1 <- mediuClientiPierdutiServer1 + 1
          t1 <- t + rabdare
          EL[2] <- t1
        }
        else{
          t1 <- t + y1
          EL[2] <- t1
        }
        #nu depasim limita de 10 persoane in coada 
        if(n1 >= 10)
        {
          while(n1 >= 10){
            n1 <- n1-1
          }
        }
        
        
      }
      A1[nA] <- t
    }
    
    #cazul 2 : serverul 1 se elibereaza inainte de sosirea unui client nou
    if(TRUE && t1 < tA && TRUE && t1 <= t2 && TRUE){
      t<- t1
      
      #salvam timpii de sosire plecare
      timpiPlecareServer1[countServer1] <- t1
      timpiSosireServer1[countServer1 + 1] <- tA
      countServer1 <- countServer1 + 1
      
      n1 <- n1 - 1
      n2 <- n2 + 1
      
      if(n1 == 0){
        t1 <- 30000
      }
      else {
        y1 <- inversa1()
        
        #daca timpul de asteptare generat este mai mare decat rabdarea pe care o are clientul
        #el pleaca
        rabdare <- functie_rabdare()
        if(y1 > rabdare){
          if(primulClientPierdut == 0){
            primulClientPierdut = t + rabdare
          }
          t1 <- t + rabdare
          clientiPierduti <- clientiPierduti + 1
          #cati clienti pierdem in medie pe server
          mediuClientiPierdutiServer1 <- mediuClientiPierdutiServer1 + 1
        }
        else t1 <- t + y1
      }
      
      
      if(n2 == 1){
        y2 <- inversa2()
        #daca timpul de asteptare generat este mai mare decat rabdarea pe care o are clientul
        #el pleaca
        rabdare <- functie_rabdare()
        if(y2 > rabdare){
          if(primulClientPierdut == 0){
            primulClientPierdut = t + rabdare
          }
          t2 <- t + rabdare
          clientiPierduti <- clientiPierduti + 1
          #cati clienti pierdem in medie pe server
          mediuClientiPierdutiServer2 <- mediuClientiPierdutiServer2 + 1
        }
      }
      else {
        if(n2 <=10){
          t2 <- t + y2
          #venit mediu cu banii generati dat de client v.a.
          #plus bacsis generat v.a.
          venitMediu <- venitMediu + functie_cash(t2-t)
          tBacsis <- tBacsis + functie_bacsis()
        }
        else{
          while(n2 > 10){
            n2 <- n2 - 1
          }
          
        }
      }
      
      EL[2] <- t1
      EL[3] <- t2
      
      A2[nA - n1] <- t
      
    }
    
    
    
    #cazul 3 : serverul 2 se elibereaza inainte de a sosi un client nou
    #si inainte de finalizarea activitatii la serverul 1
    if(TRUE && t2 < tA && TRUE && t2 <t1){
      t<- t2
      #print(n2)
      nD <- nD + 1
      n2 <- n2 - 1
      
      if(n2 == 0){
        t2 <- 30000
      }
      if(n2 == 1){
        y2 <- inversa2()
        #daca timpul de asteptare generat este mai mare decat rabdarea pe care o are clientul
        #el pleaca
        rabdare <- functie_rabdare()
        if(y2 > rabdare){
          if(primulClientPierdut == 0){
            primulClientPierdut = t + rabdare
          }
          t2 <- t + rabdare
          #cati clienti pierdem in medie pe server
          clientiPierduti <- clientiPierduti + 1
          mediuClientiPierdutiServer2 <- mediuClientiPierdutiServer2 + 1
        }
        else{
          t2 <- t + y2
          #venit mediu cu banii generati dat de client v.a.
          #plus bacsis generat v.a.
          venitMediu <- venitMediu + functie_cash(t2 - t)
          tBacsis <- tBacsis + functie_bacsis()
        }
        
      }
      
      EL[3] <- t2
      
      D[nD] <- t
      
    }
    
  }
  #pentru plotare
  TVenitMediuZilnic[contor] <- venitMediu
  TBacsisZilnic[contor] <- tBacsis
  TClientiPierduti[contor] <- clientiPierduti
  TTimpiSosireServer1 <- timpiSosireServer1
  TTimpiPlecareServer1 <- timpiPlecareServer1
  
  TTimpiSosireServer2 <- A2
  TTimpiPlecareServer2 <- D
  
  lungimeMin1 = min(length(timpiPlecareServer1), length(timpiSosireServer1))
  lungimeMin2 = min(length(D), length(A2))
  
  NrClientiZilnici[contor] <- length(D)
  
  minValServer1 <- timpiPlecareServer1[1:lungimeMin1] - timpiSosireServer1[1:lungimeMin1]
  minValServer2 <- D[1:lungimeMin2] - A2[1:lungimeMin2]
  medieServer1 <- mean(minValServer1)
  medieServer2 <- mean(minValServer2)
  
  minim1 <- min(minValServer1)
  minim2 <- min(minValServer2)
  maxim1 <- max(minValServer1)
  maxim2 <- max(minValServer2)
  
  #Calculam minimul si maximul pentru serverul 1 respectiv serverul 2
  if (minim1 < minServer1){
    minServer1 <- minim1
  }
  if (maxim1 > maxServer1){
    maxServer1 <- maxim1
  }
  if (minim2 < minServer2){
    minServer2 <- minim2
  }
  if (maxim2 > maxServer2){
    maxServer2 <- maxim2
  }
  
}

mediuClientiPierdutiServer1 <- mediuClientiPierdutiServer1/numarZile
mediuClientiPierdutiServer2 <- mediuClientiPierdutiServer2/numarZile

medieNrClienti <- round(mean(NrClientiZilnici),3)
medieClientiPierduti <- round(mean(TClientiPierduti),3)
medieVenitZilnic <- round(mean(TVenitMediuZilnic),3)
medieBacsis <- round(mean(TBacsisZilnic),3)
print(mean(NrClientiZilnici))
print(primulClientPierdut)
print(mean(TClientiPierduti))
print(mean(TVenitMediuZilnic))
print(mean(TBacsisZilnic))

minServer1 <- round(minServer1,3)
maxServer1 <- round(maxServer1,3)
minServer2 <- round(minServer2,3)
maxServer2 <- round(maxServer2,3)

print(mediuClientiPierdutiServer1)
print(mediuClientiPierdutiServer2)

print(minServer1)
print(maxServer1)
print(minServer2)
print(maxServer2)
print(medieServer1)
print(medieServer2)

# Sosiri si Plecari Server 1

#graficul sosirilor desenat cu linie albastra continua
plot(TTimpiSosireServer1, type="l", col="blue", ylim=c(0,50), xlim=c(0,15))

# graficul plecarilor cu linie punctata rosie
lines(TTimpiPlecareServer1, type="l", pch=22, lty=2, col="red")

# Create a title with a red, bold/italic font
title(main="Sosiri si Plecari - Server 1", col.main="red", font.main=4)

legend(1, 50, c("Timpi Sosire","Timpi Plecare"), cex=0.8, col=c("blue","red"), pch=21:22, lty=1:2)


##################################

# Sosiri si Plecari Server 2
plot(TTimpiSosireServer2, type="l", col="blue", ylim=c(0,50), xlim=c(0,15))

lines(TTimpiPlecareServer2, type="l", pch=22, lty=2, col="red")

title(main="Sosiri si Plecari - Server 2", col.main="red", font.main=4)

legend(1, 50, c("Timpi Sosire","Timpi Plecare"), cex=0.8, col=c("blue","red"), pch=21:22, lty=1:2)

##################################

# Performanta Server 1 vs Server 2
plot(minValServer1, type="l", col="blue", ylim=c(0,20), xlim=c(0,80))

lines(minValServer2, type="l", pch=22, lty=2, col="red")

title(main="Server1 - Server 2 Comparatie", col.main="red", font.main=4)

legend(1, 8, c("Server 1","Server 2"), cex=0.8, col=c("blue","red"), pch=21:22, lty=1:2)

##################################

# Nr Clienti veniti zilnic
plot(NrClientiZilnici, type="l", col="blue", ylim=c(300,450), xlim=c(0,180))
title(main="Clienti Veniti Zilnic", col.main="red", font.main=4)



# Venit mediu pe an
plot(TVenitMediuZilnic, type="l", col="blue", ylim=c(1750,2200), xlim=c(0,200))
title(main="Venit Mediu", col.main="red", font.main=4)


# Bacsis zilnic
plot(TBacsisZilnic, type="l", col="blue", ylim=c(800,1200), xlim=c(0,200))
title(main="Bacsis", col.main="red", font.main=4)


# Clienti Pierduti zilnic
plot(TClientiPierduti, type="l", col="blue", ylim=c(50,120), xlim=c(0,200))
title(main="Clienti Pierduti Zilnic", col.main="red", font.main=4)








#################################################################################
#################################################################################



# COD SHINY WEB APP


#install.packages("shinythemes")
#install.packages("shinyjs")

library(shiny)
library(shinythemes)
library(shinyjs)

ui <- fluidPage(theme=shinytheme("cosmo"),
                
                #functie necesara pentru a apela functii din libraria Shinyjs
                useShinyjs(),
                
                # Titlu 
                titlePanel(h1("Cafenea Drive-Through")),
                
                
                #Meniul din stanga
                sidebarLayout(
                  
                  sidebarPanel(
                    h2("Acestea sunt veniturile si performantele saptamanale ale cafenelei noastre, la serverele 1 si 2"),
                    
                    # un slider care primeste un input ce significa nr de zile (maxim 180 zile = 6 luni) pe care histogramele sa 
                    #afiseze evolutia serverelor cafenelei
                    sliderInput(inputId = "bins", label = h4("Alegeti pentru cate zile din 6 luni ati dori sa vedeti performantele"), min = 1, 
                                max = 180, value = 30),
                    
                    br(),
                    #buton care sa ne afiseze un div cu mai multe informatii
                    actionButton("button", "Mai multe informatii"),
                    
                    br(),
                    br(),
                    
                    #div cu informatii suplimentare determinate in codul principal
                    
                    tags$div(class="info", id="info2", checked=NA,
                             h3("Performantele serverului 1"),
                             p("Timpul MINIM petrecut de clienti la serverul 1: ", minServer1, "min"),
                             p("Timpul MAXIM petrecut de clienti la serverul 1: ", maxServer1, "min"),
                             br(),
                             h3("Performantele serverului 2"),
                             p("Timpul MINIM petrecut de clienti la serverul 2 : ", minServer2, "min"),
                             p("Timpul MAXIM petrecut de clienti la serverul 2 : ", maxServer2, "min"),
                             br(),
                             br(),
                             h4("Media clientilor veniti pe zi : ", medieNrClienti, "RON"),
                             h4("Media clientilor pierduti pe zi : ", medieClientiPierduti, "RON"),
                             h4("Venitul mediu zilnic : ", medieVenitZilnic, "RON"),
                             h4("Bacsisul primit zilnic de angajati : ", medieBacsis, "RON")
                             
                             
                    )
                  ), 
                  
                  #panoul mare din dreapta
                  mainPanel(
                    
                    #taburi pentru fiecare din tipurile de grafice de le vrem afisate
                    tabsetPanel(
                      tabPanel(h4("Histograme Sosiri/Plecari"),
                               
                               plotOutput("histoSosireServer1"), # se apeleaza o functie specifica pentru fiecare plot
                               br(), #lasa spatiu intre grafice
                               br(),
                               plotOutput("histoSosireServer2")
                      ), 
                      tabPanel(h4("Grafice Sosiri/Plecari"),
                               plotOutput("plotSosirePlecareServer1"),
                               br(),
                               br(),
                               plotOutput("plotSosirePlecareServer2")
                      ),
                      tabPanel(h4("Alte grafice"),
                               plotOutput("plotNrClientiZilnici"),
                               br(),
                               br(),
                               plotOutput("plotVenitMediu"),
                               br(),
                               br(),
                               plotOutput("plotBacsis"),
                               br(),
                               br(),
                               plotOutput("plotClientiPierduti")
                               
                      )
                    ),
                    
                    
                  )
                ),
                
                # customizare HTML si CSS
                
                tags$head(
                  
                  tags$style(HTML("

                    body {
                      background-color: khaki;
                      background-image: url(coffee-shop.png);
                      background-size: 1920px 1080px;
                      color: black;
                    }
                     
                      h1{
                      font-size: 60px;
                      color: white;
                      text-align: center;
                      font-weight: 900;
                      font-family:monospace;
                      }
                     
                     h2{
                     color: brown;
                     font-weight: bold;
                     }
                     
                      h3{
                      font-size: 25px;
                      font-weight: bold;
                      color: brown;
                      }
                      h4{
                      font-weight: bold;
                      color: black;
                      }
                      
                      .info{
                      padding: 10px;
                      border-style: dotted;
                      border-color:black;
                      border-width: 10px;
                      display: none;
                      }
                    
                  "))
                  
                )
                
                
                
)

server <- function(input, output, session) {
  
  #event listener pentru apasarea butonului. La apasare va aparea panoul cu informatii aditionale
  
  onclick("button",runjs("var x=document.getElementById('info2');
  if(x.style.display==='none')
        x.style.display='block';
    else x.style.display='none';

   "))
  
  
  output$histoSosireServer1 <- renderPlot({
    
    #input pentru slider-ul histogramelor 
    bins <- seq(min(TTimpiSosireServer1), max(TTimpiSosireServer1), length.out = input$bins + 1)
    
    # deseneaza histograma cu atatea diviziuni cate am selectat pe slider
    
    hist(TTimpiSosireServer1, 
         main="Timpi Sosire si Plecare la Serverul 1", 
         xlab="Timpi Sosire", 
         border="#ffcc80", 
         col="black", 
         xlim=c(0,500), 
         las=1, 
         breaks=bins, 
         prob = TRUE)
    
    # o a doua histograma (pentru plecari) desenata peste precedenta pentru sosiri
    hist(TTimpiPlecareServer1, 
         main="Timpi Plecare la Serverul 1", 
         xlab="Timpi Plecare", 
         border="black", 
         col="#ffcc80", 
         xlim=c(0,500), 
         las=1, 
         breaks=bins, 
         prob = TRUE, add = T)
    
    #legenda histogramelor
    legend(0, 0.0005, c("Timpi Sosire","Timpi Plecare"), cex=0.8, col=c("#ffcc80","black"), pch=21:22, lty=1:2)
  })
  
  output$histoSosireServer2 <- renderPlot({
    
    bins <- seq(min(TTimpiSosireServer2), max(TTimpiSosireServer2), length.out = input$bins + 1)
    
    hist(TTimpiSosireServer2, 
         main="Timpi Sosire si Plecare la Serverul 2", 
         xlab="Timpi Sosire", 
         border="#ffcc80", 
         col="black", 
         xlim=c(0,500), 
         las=1, 
         breaks=bins, 
         prob = TRUE)
    
    hist(TTimpiPlecareServer2, 
         main="Timpi Plecare la Serverul 2", 
         xlab="Timpi Plecare", 
         border="black", 
         col="#ffcc80", 
         xlim=c(0,500), 
         las=1, 
         breaks=bins, 
         prob = TRUE, add = T)
    
    legend(0, 0.0005, c("Timpi Sosire","Timpi Plecare"), cex=0.8, col=c("#ffcc80","black"), pch=21:22, lty=1:2)
    
  })
  
  
  
  output$plotSosirePlecareServer1 <- renderPlot({
    
    
    #plotare cu doua linii pe grafic, continua pentru sosiri si intrerupta pentru plecari
    
    plot(TTimpiSosireServer1, type="l", col="brown", ylim=c(0,50), xlim=c(0,15))
    lines(TTimpiPlecareServer1, type="l", pch=22, lty=2, col="black")
    
    #titlu
    title(main="Sosiri si Plecari - Server 1", col.main="brown", font.main=4)
    
    #legenda
    legend(1, 50, c("Timpi Sosire","Timpi Plecare"), cex=0.8, col=c("brown","black"), pch=21:22, lty=1:2)
  })
  
  
  
  output$plotSosirePlecareServer2 <- renderPlot({
    
    
    plot(TTimpiSosireServer2, type="l", col="brown", ylim=c(0,50), xlim=c(0,15))
    lines(TTimpiPlecareServer2, type="l", pch=22, lty=2, col="black")
    title(main="Sosiri si Plecari - Server 2", col.main="brown", font.main=4)
    legend(1, 50, c("Timpi Sosire","Timpi Plecare"), cex=0.8, col=c("brown","black"), pch=21:22, lty=1:2)
  })
  
  output$plotVenitMediu <- renderPlot({
    
    
    # Grafic Venit mediu pe an
    plot(TVenitMediuZilnic, type="l", col="black", ylim=c(1700,2200), xlim=c(0,180))
    title(main="Venit Mediu Zilnic pe perioada a 6 luni", col.main="brown", font.main=4)
    
  })
  
  output$plotBacsis <- renderPlot({
    
    
    # Grafic Bacsis zilnic
    plot(TBacsisZilnic, type="l", col="black", ylim=c(850,1200), xlim=c(0,180))
    title(main="Bacsis Primit Zilnic pe perioada a 6 luni", col.main="brown", font.main=4)
    
  })
  
  output$plotClientiPierduti <- renderPlot({
    
    # Grafic Clienti Pierduti zilnic
    plot(TClientiPierduti, type="l", col="black", ylim=c(40,100), xlim=c(0,180))
    title(main="Clienti Pierduti Zilnic pe perioada a 6 luni", col.main="brown", font.main=4)
    
  })
  
  
  output$plotNrClientiZilnici <- renderPlot({
    
    
    # Grafic Clienti veniti zilnic
    plot(NrClientiZilnici, type="l", col="black", ylim=c(320,380), xlim=c(0,180))
    title(main="Clienti Veniti Zilnic pe perioada a 6 luni", col.main="brown", font.main=4)
    
  })
  
  
  
}


#rulam aplicatia
shinyApp(ui = ui, server = server)
