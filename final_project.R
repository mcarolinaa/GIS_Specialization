## Código usado para analisar os dados

#### **Chuvas durante os anos de avaliação**

    library(tidyverse)

	# dados meteorológicos - previamente importados de Power Nasa
	met_single <- read.csv(path,
                        skip = 13)
						
  head(met_single)
	glimpse(met_single)
	
	
	# separação nas 3 épocas avaliadas
	# La nina: 2010, El nino: 2011, Neutral: 2012

    data_nina <-
        met_single %>%
        filter (YEAR == 2010)
    
    data_nino <-
        met_single %>%
        filter (YEAR == 2011)
    
    data_neutr <-
        met_single %>%
        filter (YEAR == 2012)
		
		
	# Agrupamento e resumo para cada evento
		
     nina <-
     data_nina %>%
     group_by(MO) %>%
     summarise(sum(PRECTOT)) %>%
     mutate(EV = rep("nina", 12))
    
	as.data.frame(nina)

    nino <-
        data_nino %>%
        group_by(MO) %>%
        summarise(sum(PRECTOT)) %>%
        mutate(EV = rep("nino", 12))
    
    as.data.frame(nino)
    
    neutr <-
        data_neutr %>%
        group_by(MO) %>%
        summarise(sum(PRECTOT)) %>%
        mutate(EV = rep("neutr", 12))
    
    as.data.frame(neutr)
	
	
	years.evs <- rbind (nina, nino, neutr)
	
	years.evs <-
    years.evs %>%
    mutate(RAIN = `sum(PRECTOT)`)
	
	# Visualização
	
	years.evs %>%
    ggplot(aes(x=as.factor(MO), y=RAIN, fill=RAIN))+
    geom_col(position = "dodge",  color = "black")+
    facet_grid(.~EV)+
    theme_bw()+
    scale_fill_viridis_c(direction=-1)+
    scale_y_continuous(breaks = seq(0, 600, by=40))+
    ylab("Monthly rain (mm)")+ xlab("Month of the year")

#### **Áreas das classes de variação de NDVI em áreas agrícolas** 

Aqui mostro a *quantificação* das áreas de variação, ao associar os pixels de cada classe com seu respectivo tamanho.

    # dados exportados do ArcGIS
	
    results2 <- read.csv("results2.csv", sep = ";")
	
	results2 <-
    results2 %>%
    mutate(AREA_HA = AREA/10000)
	
	results2 %>%
    mutate(CLASS = factor(CLASS,
                          levels = c("SIG_LOSS", "LOSS", "UNCHG", "GAIN", "SIG_GAIN"))) %>%
    ggplot(aes(x = CLASS, y = AREA_HA, fill=CLASS))+
    geom_col(position = "dodge",  color = "black")+
    facet_grid(.~EVENT)+
    theme_bw()+
    scale_fill_viridis_d(direction=-1)+
    scale_y_continuous(breaks = seq(1000, 50000, by=2000))+
    xlab("Class of agricultural NDVI variation")+ ylab("Area(ha)")
