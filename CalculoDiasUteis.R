#R version 3.4.4 

#Dados de feriados
feriado_dia <- c("01/01/2020","24/02/2020","25/02/2020","10/04/2020",
       "21/04/2020","01/05/2020","11/06/2020","07/09/2020",
       "12/10/2020","02/11/2020","15/11/2020","25/12/2020")
feriado_nome <- c("Confraternização Universal","Carnaval","Carnaval",
       "Paixão de Cristo","Tiradentes","Dia do Trabalho",
       "Corpus Christi","Independência do Brasil","N.Sra.Aparecida",
       "Finados","Proclamação da República","Natal")
feriados <- data.frame(feriado_dia,feriado_nome)

#Função de pesquisa nos dados
pesquisa <- function(a){
    dia <- as.character(a,"%d/%m/%Y")
    for(i in 1:length(feriados$feriado_dia)){
        if(feriados$feriado_dia[i]==dia){
           return(FALSE)
        }
    }
    return(TRUE)
}

# Função para dias úteis
dias_uteis <- function(ini,fim){
    ini <- as.Date(ini, "%d/%m/%Y")
    fim <- as.Date(fim, "%d/%m/%Y")
    dif <- fim-ini
    if(dif<0) return(NULL)
    dia <- ini
    contador <- 0
    for(i in 0:dif){
        dia_semana <- weekdays(dia)
        if(dia_semana!="Sunday" & dia_semana!="Saturday" & pesquisa(dia)){
            contador <- contador +1
            }
        dia <- dia + 1
    }
    return(contador)
}

#Entrada de argumentos
#input1 <- "16/11/2020"
#input2 <- "15/11/2020"

# Executar
output <- dias_uteis(ini=input1,fim=input2)
#output

