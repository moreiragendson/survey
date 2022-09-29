
barh <- function(data, x, y){
  
  data %>% 
    ggplot(aes(  fct_reorder(.f={{x}},   .x =   {{y}}  )  ,
                 {{y}}  ))+
    geom_col(fill="navyblue", colour="navyblue")+
    geom_text(aes(label =  str_replace(
                              round({{y}}, 2),
                              pattern = "\\.",
                              replacement = ",") %>% 
                           str_c("%")),
              hjust=-.01)+
    coord_flip()+
    theme_minimal()+
    theme(axis.text.x = element_blank())
  
}
