library(ggplot2)

ggplot(data = VIS2_COVID_19_Oct2021) +
  geom_line(aes(x = Statistikdatum, 
                y = Totalt_antal_fall,
                group = 1),
            size = 1,
            color = "#1E3C90") +
  theme_bw() +  
  scale_x_datetime(date_breaks = "1 month", date_labels = "%y %b") + 
  labs(y = "Antal fall",  
       x = "Datum",  
       caption = "Källa: Folkhälsomyndigheten (12-Okt-2021)", 
       title = "Antal bekräftade fall av COVID-19 i Sverige")  +
  theme(plot.title = element_text(hjust = 0.5,
                                  size = 15,
                                  face = "bold"), 
        axis.title.y = element_text(face = "bold",  
                                    angle = 0,
                                    vjust = 0.5,
                                    size = 13), 
        axis.title.x = element_text(face = "bold",
                                    size = 13), 
        plot.caption = element_text(size = 9,  
                                    face = "italic"),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        panel.grid.major.x = element_line(color = "darkgray"),
        panel.grid.major.y = element_line(color = "darkgray")) + 
  scale_y_continuous(breaks = seq(from = 0, to = 12000, by = 1000))