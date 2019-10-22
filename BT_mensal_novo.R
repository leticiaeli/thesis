library(ggplot2)

# FUNCTIONS ----

## this function round the number for a the nearest ten or hundred

nearest <- function(x)
{
  round(x+5,-1)
}

## this function is used in df.balanco function

sumcols = function(df, col_list, output_type){
  first = TRUE
  for(c in col_list){
    if(first == TRUE){
      main_col = df[,grepl(paste0(c,output_type),colnames(df))]
      first = FALSE
    }else{
      main_col = main_col + df[,grepl(paste0(c,output_type),colnames(df))]
    }
  }
  return(main_col)
}

## this function create the thermal balance data.frame

df.balanco <- function(df, zt, zt1, internal_walls, external_walls, floor, roof, windows, doors, energy_unit='kWh'){
  
  if(energy_unit == 'J'){
    divisor = 1
  }else{
    if(energy_unit == 'kJ'){
      divisor = 1000
    }else{
      if(energy_unit == 'Wh'){
        divisor = 3600
      }else{
        if(energy_unit == 'kWh'){
          divisor = 3600000
        }
      }
    }
  }
  
  df_balanco = data.frame(
    date = df$Date.Time,
    mes = as.numeric(substr(df$Date.Time, 2,3)),
    dia = as.numeric(substr(df$Date.Time, 5,6)),    
    hora = as.POSIXct(strptime(df$Date.Time," %m/%d  %H:%M:%S")),
    temp_op = df[,grepl(paste0(zt,'.Zone.Operative.Temperature..C..'),colnames(df))],
    people = df[,grepl(paste0(zt1,'.People.Occupant.Count..'),colnames(df))],
    internal_gains = df[,grepl(paste0(zt,'.Zone.Total.Internal.Convective.Heating.Energy..J..'),colnames(df))]/divisor, 
    windows = -sumcols(df, windows, '.Surface.Inside.Face.Convection.Heat.Gain.Energy..J..')/divisor,
    internal_walls = -sumcols(df, internal_walls, '.Surface.Inside.Face.Convection.Heat.Gain.Energy..J..')/divisor,
    external_walls = -sumcols(df, external_walls, '.Surface.Inside.Face.Convection.Heat.Gain.Energy..J..')/divisor,
    doors = -sumcols(df, doors, '.Surface.Inside.Face.Convection.Heat.Gain.Energy..J..')/divisor, 
    floor = -sumcols(df, floor, '.Surface.Inside.Face.Convection.Heat.Gain.Energy..J..')/divisor,
    roof = -sumcols(df, roof, '.Surface.Inside.Face.Convection.Heat.Gain.Energy..J..')/divisor
  )
  
  if(any(grepl('Zone.Air.System.Sensible',colnames(df)))){ 
    if((sum(df[,grepl(paste0(zt,'.Zone.Air.System.Sensible.Cooling.Energy..J.'),colnames(df))]) != 0)){
      df_balanco$cooling = -df[,grepl(paste0(zt,'.Zone.Air.System.Sensible.Cooling.Energy..J.'),colnames(df))]/divisor
    }
    if((sum(df[,grepl(paste0(zt,'.Zone.Air.System.Sensible.Heating.Energy..J.'),colnames(df))])) != 0){
      df_balanco$heating = df[,grepl(paste0(zt,'.Zone.Air.System.Sensible.Heating.Energy..J..'),colnames(df))]/divisor
    }
  }
  if(any(grepl('Infiltration',colnames(df)))){
    df_balanco$vn_loss = -df[,grepl(paste0(zt,'.AFN.Zone.Infiltration.Sensible.Heat.Loss.Energy..J..'),colnames(df))]/divisor
    df_balanco$vn_gain = df[,grepl(paste0(zt,'.AFN.Zone.Infiltration.Sensible.Heat.Gain.Energy..J..'),colnames(df))]/divisor
  }
  
  return(df_balanco)
}

## plot month and two columns with thermal balance for Ideal Loads use and natural ventilation use (NBR models)

# plot.month = function(df, file_name, width = 32, height = 18, units = "cm", dpi = 500, energy_unit='kJ', title=TRUE, period=c(c(1,1),c(12,31))){
#   
#   dfplot = data.frame('ganho_perda'=colnames(df)[4:length(colnames(df))], 'valor' = apply(df[,4:length(colnames(df))], 2, sum), 
#                       'ganho_perda_pt'=c("Carga Interna","Janelas","Paredes Internas","Paredes Externas","Portas","Piso","Cobertura","Perdas por VN","Ganhos por VN"))
#   
#   dfplot$ganho_perda_pt <- factor(dfplot$ganho_perda_pt, levels=c("Carga Interna","Paredes Internas","Paredes Externas","Piso","Cobertura","Portas","Janelas","Perdas por VN","Ganhos por VN")) ## transform in factor
#   
#   max = max(dfplot$valor)
#   min = min(dfplot$valor)
#   
#   c = as.numeric(ifelse(max > min*(-1), max, min*(-1)))
#   
#   c1 = as.numeric(foo(c))
#   
#   b = round(sum(df$vn),0)
#   
#   plot = ggplot(dfplot, aes(x=ganho_perda_pt, y=valor, fill=ganho_perda_pt)) +
#     geom_bar(stat="identity")+
#     geom_text(aes(label=round(valor,0)), vjust=-0.3, size=3.5)+
#     annotate("text", x = 8.25, y = c1, label = paste0("Resíduo: ",round(sum(df$valor),0),' [', energy_unit, ']'))+
#     labs(x="Ganhos e Perdas", y=paste0('Ganhos e Perdas no Ar da Zona [',energy_unit,']'))+
#     theme(legend.title = element_blank(),
#           axis.text.x = element_blank(),
#           axis.title.y = element_text(size=12, vjust = 2),
#           axis.text.y = element_text(size=10),
#           legend.text = element_text(size=10),
#           panel.background = element_rect(fill = "#F4F6F6",
#                                           colour = "#F4F6F6",
#                                           size = 0.5, linetype = "solid"),
#           panel.grid.major = element_line(size = 0.5, linetype = 'solid',
#                                           colour = "#FEFEFE"), 
#           panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
#                                           colour = "#FEFEFE"))+
#     scale_fill_manual(values=c("#EDE21C","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A"))+
#     ylim(-c1,c1)
#   #+
#   # scale_fill_manual(values=c("#FFFF99","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A"))#+  # esse eh o Paired
#   # ylim(-1500,1500)
#   if(title){
#     plot = plot +
#       ggtitle('Balanço Térmico Anual')
#   }
#   #show(plot)
#   ggsave(paste0(file_name,'.png'),plot=plot, width=width, height=height, units=units, dpi=dpi)
# }

# FILES ----

## DORMITÓRIO 1 ----

# colnames(df)
zt = 'DORM1'
zt1 = 'DORMITORIO1' ## this output have a different zone name in my .idf
internal_walls = c('PARINT1_DORM1SALA','PARINT2_DORM1DORM2')
external_walls = c('PAREXT_LESTE_DORM1','PAREXT_SUL_DORM1')
floor = c('PISO_QUARTO1')
roof = c('FORRO_DORM1ATICO')
windows = c('JANQUARTO1_SUL')
doors = c('PORTAINT_DORM1SALA')

setwd("D:/Frentes de Trabalho/simu_NBR/balanco_termico/NBR/BRA_sp_Sao.Paulo-Congonhas.AP.837800_TMYx.2003-2017/")

vn = read.csv("vn_Caso1_sp_sta_co_1.csv")

df_vn = df.balanco(vn, zt, zt1, internal_walls, external_walls, floor, roof, windows, doors)
df_vn$cooling = 0 # if I don't but these columns df_vn and df_ac will have a different lenght 
df_vn$heating = 0

setwd("D:/Frentes de Trabalho/simu_NBR/balanco_termico/NBR/HVAC/BRA_SP_Sao.Paulo-Congonhas.AP.837800_TMYx.2003-2017/")

ac = read.csv("ac1_Caso1_sp_sta_co_1.csv")

df_ac = df.balanco(ac, zt, zt1, internal_walls, external_walls, floor, roof, windows, doors)

## thermal balance for the time when zone is occupied for which model (natural ventilation and ideal loads) 

## calculate which time zone needs cooling or heating (the range is a and b)
a = as.numeric(26)
b = as.numeric(18)

df_ac$temp_vn = df_vn$temp_op ## put the column for operative temperature from the model with natural ventilation

df_vn_ocupp = subset(df_vn, (df_vn$people > 0) & (df_vn$temp_op < a) & (df_vn$temp_op > b)) ## subset df for occupied hours and with the range that we need use VN
df_ac_ocupp = subset(df_ac, (df_ac$people > 0) & ((df_ac$temp_vn >= a) | (df_ac$temp_vn <= b))) ## subset df for occupied hours and with the range that we need use AC

## create the df with gain and loss for which month

ganho_perda = c("internal_gains","windows","internal_walls","external_walls","doors","floor","roof","vn_loss","vn_gain", "cooling", "heating")
mes = c(1:12)

df_vn_graph = data.frame(mes=mes, ganho_perda=rep(ganho_perda,each=12),valor = c(1:1))
df_ac_graph = data.frame(mes=mes, ganho_perda=rep(ganho_perda,each=12),valor = c(1:1))

for(n in mes){
  for(k in ganho_perda){
    for(i in 1:nrow(df_vn_graph)){
        df_vn_graph$valor[df_vn_graph$mes == n & df_vn_graph$ganho_perda == k] = sum(df_vn_ocupp[,k][df_vn_ocupp$mes == n])
    }
    for(i in 1:nrow(df_ac_graph)){
      df_ac_graph$valor[df_ac_graph$mes == n & df_ac_graph$ganho_perda == k] = sum(df_ac_ocupp[,k][df_ac_ocupp$mes == n])
    }
  }
}

# calculate the percentage for model using natural ventilation and ideal loads for each month

library(plyr)
# library(dplyr)
days = count(df_vn$mes)/24 ## count the days for each month (each day have 24 hours)
days$mes = c(1:12)
VN = count(df_vn_ocupp$mes)/10 ## bedroom is occupied for 10 hours
AC = count(df_ac_ocupp$mes)/10
# 
# df_month_vn = data.frame(mes=mes, dias=days$freq, porcentagem=VN$freq, group=c("VN"))
# df_month_ac = data.frame(mes=mes, dias=days$freq, porcentagem=AC$freq, group=c("AC"))
# 
# df_month_vn$freq = round(((df_month_vn$porcentagem/df_month_vn$dias)*100),0) # divide days for VN / month days
# df_month_ac$freq = round(((df_month_ac$porcentagem/df_month_ac$dias)*100),0)
# 
# df_month_vn$freq1 = paste(df_month_vn$freq, "%")
# df_month_ac$freq1 = paste(df_month_ac$freq, "%")
# 
# df_month_ac$mes = c(0.75,1.75,2.75,3.75,4.75,5.75,6.75,7.75,8.75,9.75,10.75,11.75)
# df_month_vn$mes = c(1.25,2.25,3.25,4.25,5.25,6.25,7.25,8.25,9.25,10.25,11.25,12.25)

df_year = data.frame(VN=round(((sum(VN$freq)/365)*100),0),AC=round(((sum(AC$freq)/365)*100),0))

df_year$VN = paste0(df_year$VN, "%")
df_year$AC = paste0(df_year$AC, "%")

# df_teste = data.frame(teste=c(1:132))
# 
# df_teste = data.frame(teste=df_month_vn$freq)
# 
# df_teste1 = data.frame(teste=1:120)
# df_teste1$teste = NA
# 
# x = rbind(df_teste, df_teste1)
# 
# df_teste = data.frame(teste=df_month_ac$freq)
# y = rbind(df_teste, df_teste1)
# 
# 
# df_month = rbind(df_month_vn, df_month_ac)
#  
# df_month$freq = round((df_month$freq*100),0)

## plot

setwd("D:/Frentes de Trabalho/simu_NBR/balanco_termico/resultados/mensal/nbr/")

library(ggplot2)

# create the groups (for the plot) in each df

df_vn_graph$group = "VN"
# df_vn_graph$freq = x$teste
#df_vn_graph$freq = df_month_vn$freq
df_ac_graph$group = "AC"
# df_ac_graph$freq = y$teste
#df_ac_graph$freq = df_month_ac$freq


df_graph = rbind(df_vn_graph, df_ac_graph)

# put the names in portuguese

df_graph$portugues = rep(c("Carga Interna","Janelas","Paredes Internas","Paredes Externas","Portas","Piso","Cobertura","Perdas por VN","Ganhos por VN","Refrigeração","Aquecimento"),each=12)
df_graph$mes.abb_pt = c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")

# ggplot plot in alphabetic order so I need transform in factor the gains and losses names

df_graph$portugues <- factor(df_graph$portugues, levels=c("Carga Interna","Paredes Internas","Paredes Externas","Piso","Cobertura","Portas","Janelas","Perdas por VN","Ganhos por VN","Refrigeração","Aquecimento"))
df_graph$mes.abb_pt = factor(df_graph$mes.abb_pt, levels=c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez"))

# set the plot name and image

graph_title = "Balanço Térmico da Referência - Dormitório 1"
graph_png = "SP_D1_Ref_r01.png"

# set the graph limit

max = max(df_graph$valor)
min = min(df_graph$valor)

c = as.numeric(ifelse(max > min*(-1), max, min*(-1)))

c1 = as.numeric(nearest(c))

# the plot :)

png(filename = paste0(graph_png), width = 25, height = 15, units = "cm", res = 500)
plot(
  ggplot(df_graph, aes(x=mes.abb_pt, y=valor, fill=portugues, group=group)) +
    geom_bar(stat="identity", position=position_dodge(), color="gray")+
    labs(title=paste0(graph_title), y="Ganhos e Perdas no Ar da Zona [kWh]")+
    #annotate("text", x = 1.5, y = -c1-1, label = "% DE DIAS NO MÊS COM AC ou VN:")+
    # annotate("text", x = 1, y = -c1-5, label = "% dias no mês com AC:")+
    #annotate("text", x = df_month_vn$mes, y = -c1-5, label = df_month_vn$freq1)+
    #annotate("text", x = df_month_ac$mes, y = -c1-5, label = df_month_ac$freq1)+
        #geom_text(aes(label=freq), vjust=-1.6, color="black",
    #          position = position_dodge(0.9), size=3.5)+
    annotate("text",x=4, y = c1, label = paste0("VN no ano: ",df_year$VN))+
    annotate("text",x=9, y = c1, label = paste0("AC no ano: ",df_year$AC))+
    theme(legend.title = element_blank(),
          axis.text.x = element_text(size=12),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size=12, vjust = 2),
          axis.text.y = element_text(size=10),
          legend.text = element_text(size=10),
          panel.background = element_rect(fill = "#F4F6F6",
                                          colour = "#F4F6F6",
                                          size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "#FEFEFE"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "#FEFEFE"))+
    scale_fill_manual(values=c("#FFFF99","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#A6CEE3","#1F78B4", "#BC9EC1", "#E3BAC6"))+  # esse eh o Paired
   ylim(-c1,c1)
)
dev.off()

graph_png = "SP_D1_Ref_r02.png"

group.labs = c()

group.labs <- c(paste0("VN: ", df_year$VN, " dias no ano"), paste0("AC: ", df_year$AC, " dias no ano"))
names(group.labs) <- c("VN", "AC")

png(filename = paste0(graph_png), width = 25, height = 15, units = "cm", res = 500)
plot(
  ggplot(df_graph, aes(x=mes.abb_pt, y=valor, fill=portugues)) +
    geom_bar(stat="identity", color="gray")+
    labs(title=paste0(graph_title), y="Ganhos e Perdas no Ar da Zona [kWh]")+
    #annotate("text", x = 1.5, y = -c1-1, label = "% DE DIAS NO MÊS COM AC ou VN:")+
    # annotate("text", x = 1, y = -c1-5, label = "% dias no mês com AC:")+
    #annotate("text", x = df_month_vn$mes, y = -c1-5, label = df_month_vn$freq1)+
    #annotate("text", x = df_month_ac$mes, y = -c1-5, label = df_month_ac$freq1)+
    #geom_text(aes(label=freq), vjust=-1.6, color="black",
    #          position = position_dodge(0.9), size=3.5)+
    #annotate("text",x=4, y = c1, label = paste0("VN no ano: ",df_year$VN))+
    #annotate("text",x=9, y = c1, label = paste0("AC no ano: ",df_year$AC))+
    theme(legend.title = element_blank(),
          axis.text.x = element_text(size=12),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size=12, vjust = 2),
          axis.text.y = element_text(size=10),
          legend.text = element_text(size=10),
          panel.background = element_rect(fill = "#F4F6F6",
                                          colour = "#F4F6F6",
                                          size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "#FEFEFE"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "#FEFEFE"))+
    scale_fill_manual(values=c("#FFFF99","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#A6CEE3","#1F78B4", "#BC9EC1", "#E3BAC6"))+  # esse eh o Paired
    ylim(-c1,c1)+
    facet_grid(group~.,
               labeller = labeller(group = group.labs))
)
dev.off()

#SALA ----

#colnames(df)
zt = 'SALA'
zt1 = 'SALA1' ## this output have a different zone name in my .idf
internal_walls = c('PARINT2_SALABWC','PARINT1_SALABWC','PARINT1_SALADORM2','PARINT2_SALADORM1')
external_walls = c('PAREXT_SUL_SALA','PAREXT_OESTE_SALA','PAREXT_LESTE_SALA','PAREXT_NORTE_SALA')
floor = c('PISO_SALA')
roof = c('FORRO_SALAATICO1','FORRO_SALAATICO2')
windows = c('JANSALA_OESTE','JANSALA_SUL')
doors = c('PORTAEXT_NORTE','PORTAINT_SALABWC','PORTAINT_SALADORM1','PORTAINT_SALADORM2','PORTAEXT_LESTE')


setwd("D:/Frentes de Trabalho/simu_NBR/balanco_termico/NBR/BRA_sp_Sao.Paulo-Congonhas.AP.837800_TMYx.2003-2017/")

vn = read.csv("vn_Caso1_sp_sta_co_1.csv")

df_vn = df.balanco(vn, zt, zt1, internal_walls, external_walls, floor, roof, windows, doors)
df_vn$cooling = 0 # if I don't but these columns df_vn and df_ac will have a different lenght 
df_vn$heating = 0

setwd("D:/Frentes de Trabalho/simu_NBR/balanco_termico/NBR/HVAC/BRA_SP_Sao.Paulo-Congonhas.AP.837800_TMYx.2003-2017/")

ac = read.csv("ac1_Caso1_sp_sta_co_1.csv")

df_ac = df.balanco(ac, zt, zt1, internal_walls, external_walls, floor, roof, windows, doors)

## thermal balance for the time when zone is occupied for which model (natural ventilation and ideal loads) 

## calculate which time zone needs cooling or heating (the range is a and b)
a = as.numeric(26)
b = as.numeric(18)

df_ac$temp_vn = df_vn$temp_op ## put the column for operative temperature from the model with natural ventilation

df_vn_ocupp = subset(df_vn, (df_vn$people > 0) & (df_vn$temp_op < a) & (df_vn$temp_op > b)) ## subset df for occupied hours and with the range that we need use VN
df_ac_ocupp = subset(df_ac, (df_ac$people > 0) & ((df_ac$temp_vn >= a) | (df_ac$temp_vn <= b))) ## subset df for occupied hours and with the range that we need use AC

## create the df with gain and loss for which month

ganho_perda = c("internal_gains","windows","internal_walls","external_walls","doors","floor","roof","vn_loss","vn_gain", "cooling", "heating")
mes = c(1:12)

df_vn_graph = data.frame(mes=mes, ganho_perda=rep(ganho_perda,each=12),valor = c(1:1))
df_ac_graph = data.frame(mes=mes, ganho_perda=rep(ganho_perda,each=12),valor = c(1:1))

for(n in mes){
  for(k in ganho_perda){
    for(i in 1:nrow(df_vn_graph)){
      df_vn_graph$valor[df_vn_graph$mes == n & df_vn_graph$ganho_perda == k] = sum(df_vn_ocupp[,k][df_vn_ocupp$mes == n])
    }
    for(i in 1:nrow(df_ac_graph)){
      df_ac_graph$valor[df_ac_graph$mes == n & df_ac_graph$ganho_perda == k] = sum(df_ac_ocupp[,k][df_ac_ocupp$mes == n])
    }
  }
}

# calculate the percentage for model using natural ventilation and ideal loads for each month

library(plyr)
# library(dplyr)
days = count(df_vn$mes)/24 ## count the days for each month (each day have 24 hours)
days$mes = c(1:12)
VN = count(df_vn_ocupp$mes)/10 ## bedroom is occupied for 10 hours
AC = count(df_ac_ocupp$mes)/10
# 
# df_month_vn = data.frame(mes=mes, dias=days$freq, porcentagem=VN$freq, group=c("VN"))
# df_month_ac = data.frame(mes=mes, dias=days$freq, porcentagem=AC$freq, group=c("AC"))
# 
# df_month_vn$freq = round(((df_month_vn$porcentagem/df_month_vn$dias)*100),0) # divide days for VN / month days
# df_month_ac$freq = round(((df_month_ac$porcentagem/df_month_ac$dias)*100),0)
# 
# df_month_vn$freq1 = paste(df_month_vn$freq, "%")
# df_month_ac$freq1 = paste(df_month_ac$freq, "%")
# 
# df_month_ac$mes = c(0.75,1.75,2.75,3.75,4.75,5.75,6.75,7.75,8.75,9.75,10.75,11.75)
# df_month_vn$mes = c(1.25,2.25,3.25,4.25,5.25,6.25,7.25,8.25,9.25,10.25,11.25,12.25)

df_year = data.frame(VN=round(((sum(VN$freq)/365)*100),0),AC=round(((sum(AC$freq)/365)*100),0))

df_year$VN = paste0(df_year$VN, "%")
df_year$AC = paste0(df_year$AC, "%")

# df_teste = data.frame(teste=c(1:132))
# 
# df_teste = data.frame(teste=df_month_vn$freq)
# 
# df_teste1 = data.frame(teste=1:120)
# df_teste1$teste = NA
# 
# x = rbind(df_teste, df_teste1)
# 
# df_teste = data.frame(teste=df_month_ac$freq)
# y = rbind(df_teste, df_teste1)
# 
# 
# df_month = rbind(df_month_vn, df_month_ac)
#  
# df_month$freq = round((df_month$freq*100),0)

## plot

setwd("D:/Frentes de Trabalho/simu_NBR/balanco_termico/resultados/mensal/nbr/")

library(ggplot2)

# create the groups (for the plot) in each df

df_vn_graph$group = "VN"
# df_vn_graph$freq = x$teste
#df_vn_graph$freq = df_month_vn$freq
df_ac_graph$group = "AC"
# df_ac_graph$freq = y$teste
#df_ac_graph$freq = df_month_ac$freq


df_graph = rbind(df_vn_graph, df_ac_graph)

# put the names in portuguese

df_graph$portugues = rep(c("Carga Interna","Janelas","Paredes Internas","Paredes Externas","Portas","Piso","Cobertura","Perdas por VN","Ganhos por VN","Refrigeração","Aquecimento"),each=12)
df_graph$mes.abb_pt = c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")

# ggplot plot in alphabetic order so I need transform in factor the gains and losses names

df_graph$portugues <- factor(df_graph$portugues, levels=c("Carga Interna","Paredes Internas","Paredes Externas","Piso","Cobertura","Portas","Janelas","Perdas por VN","Ganhos por VN","Refrigeração","Aquecimento"))
df_graph$mes.abb_pt = factor(df_graph$mes.abb_pt, levels=c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez"))

# set the plot name and image

graph_title = "Balanço Térmico da Referência - Sala"
graph_png = "SP_SALA_Ref_r01.png"

# set the graph limit

max = max(df_graph$valor)
min = min(df_graph$valor)

c = as.numeric(ifelse(max > min*(-1), max, min*(-1)))

c1 = as.numeric(nearest(c))

# the plot :)

png(filename = paste0(graph_png), width = 25, height = 15, units = "cm", res = 500)
plot(
  ggplot(df_graph, aes(x=mes.abb_pt, y=valor, fill=portugues, group=group)) +
    geom_bar(stat="identity", position=position_dodge(), color="gray")+
    labs(title=paste0(graph_title), y="Ganhos e Perdas no Ar da Zona [kWh]")+
    #annotate("text", x = 1.5, y = -c1-1, label = "% DE DIAS NO MÊS COM AC ou VN:")+
    # annotate("text", x = 1, y = -c1-5, label = "% dias no mês com AC:")+
    #annotate("text", x = df_month_vn$mes, y = -c1-5, label = df_month_vn$freq1)+
    #annotate("text", x = df_month_ac$mes, y = -c1-5, label = df_month_ac$freq1)+
    #geom_text(aes(label=freq), vjust=-1.6, color="black",
    #          position = position_dodge(0.9), size=3.5)+
    annotate("text",x=4, y = c1, label = paste0("VN no ano: ",df_year$VN))+
    annotate("text",x=9, y = c1, label = paste0("AC no ano: ",df_year$AC))+
    theme(legend.title = element_blank(),
          axis.text.x = element_text(size=12),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size=12, vjust = 2),
          axis.text.y = element_text(size=10),
          legend.text = element_text(size=10),
          panel.background = element_rect(fill = "#F4F6F6",
                                          colour = "#F4F6F6",
                                          size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "#FEFEFE"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "#FEFEFE"))+
    scale_fill_manual(values=c("#FFFF99","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#A6CEE3","#1F78B4", "#BC9EC1", "#E3BAC6"))+  # esse eh o Paired
    ylim(-c1,c1)
)
dev.off()

graph_png = "SP_SALA_Ref_r02.png"

group.labs = c()

group.labs <- c(paste0("VN: ", df_year$VN, " dias no ano"), paste0("AC: ", df_year$AC, " dias no ano"))
names(group.labs) <- c("VN", "AC")

png(filename = paste0(graph_png), width = 25, height = 15, units = "cm", res = 500)
plot(
  ggplot(df_graph, aes(x=mes.abb_pt, y=valor, fill=portugues)) +
    geom_bar(stat="identity", color="gray")+
    labs(title=paste0(graph_title), y="Ganhos e Perdas no Ar da Zona [kWh]")+
    #annotate("text", x = 1.5, y = -c1-1, label = "% DE DIAS NO MÊS COM AC ou VN:")+
    # annotate("text", x = 1, y = -c1-5, label = "% dias no mês com AC:")+
    #annotate("text", x = df_month_vn$mes, y = -c1-5, label = df_month_vn$freq1)+
    #annotate("text", x = df_month_ac$mes, y = -c1-5, label = df_month_ac$freq1)+
    #geom_text(aes(label=freq), vjust=-1.6, color="black",
    #          position = position_dodge(0.9), size=3.5)+
    #annotate("text",x=4, y = c1, label = paste0("VN no ano: ",df_year$VN))+
    #annotate("text",x=9, y = c1, label = paste0("AC no ano: ",df_year$AC))+
    theme(legend.title = element_blank(),
          axis.text.x = element_text(size=12),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size=12, vjust = 2),
          axis.text.y = element_text(size=10),
          legend.text = element_text(size=10),
          panel.background = element_rect(fill = "#F4F6F6",
                                          colour = "#F4F6F6",
                                          size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "#FEFEFE"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "#FEFEFE"))+
    scale_fill_manual(values=c("#FFFF99","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#A6CEE3","#1F78B4", "#BC9EC1", "#E3BAC6"))+  # esse eh o Paired
    ylim(-c1,c1)+
    facet_grid(group~.,
               labeller = labeller(group = group.labs))
)
dev.off()

