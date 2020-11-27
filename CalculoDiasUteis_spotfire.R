#JWD

# Descrição
#Jurandi: Working Days 
#Função para calcular dias úteis em 2020 
#Uso:  JWD( [data_inicio] , [data_final] )

# Função: Coluna
# Retorno: Real
# Categoria: Data e Hora


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
    if(dif<0) return(NA)
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

JWD <- function(ini,fim){
	saida<-c()
	for(i in 1:length(ini)){
			tmp1 <- ini[i]
			tmp2 <- fim[i]
			if(is.na(tmp1) | is.na(tmp2) ){
				saida <- c(saida,NA)
			}else{
				saida <- c(saida,dias_uteis(tmp1,tmp2))
			}
	}
	return(saida)
}

output <- JWD(ini=input1,fim=input2)