# Creamos objeto que cuenta escuelas nuevas o antiguas por región
regiones <- eadata %>%
  group_by(REGIÓN, NUEVA) %>%
  summarise(counts = n()) 


ggplot(regiones, aes(y = REGIÓN)) +
  geom_bar(aes(fill = NUEVA), position = position_stack(reverse = TRUE), label = regiones$counts) +
  theme(legend.position = "bottom") +
  ggtitle("Distribución de establecimientos inscritos por región") +
  theme_minimal()+
  xlab("Número de Establecimientos") + 
  ylab("Regiones") +
  scale_fill_manual(values=c("#2b83ba",
                             "#a6d96a"))
  
