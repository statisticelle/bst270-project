gen_fig2 <- function(df){
  #' Figure 2: Proportion of generation by year
  #' Get summaries required for Figure 2, and recreate plot using ggplot2.
  #' Input: dataframe
  #' Output: ggplot2 object
  
  # Get proportion of congress members by generation and year
  fig2_df <- df %>%
    group_by(year, generation_ord) %>%
    summarize(n_gen = n()) %>%
    mutate(prop_gen = round(n_gen / sum(n_gen), 3)*100) %>%
    ungroup()
  
  # Specify plot captions from 538
  title2 = "Baby boomers are the biggest generation in Congress today"
  subtitle2 = "Share of members in Congress from each generation, 1919 to 2023"
  caption2 = {
    "Birth years for the Greatest Generation to Generation Z are based on Pew Research Center definitions. 
For earlier generations, definitions are based on Strauss and Howe (1991). They are: Gilded (1822-1842),
Progressive (1843-1859), Missionary (1860-1882), Lost (1883-1900), Greatest (1901-1927), Silent (1928-1945),
baby boomer (1946-1964), Generation X (1965-1980), millennial (1981-1996), Generation Z (1997-2012)."
  }
  
  # Construct ggplot2 object
  fig2 <- fig2_df %>%
    ggplot(aes(x=year, y=prop_gen, fill=generation_ord)) +
    geom_area(position='stack', alpha=0.75) +
    theme_bw() +
    scale_x_continuous(breaks = seq(1920, 2020, 20), limits = c(1919, 2023)) +
    scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 101)) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(face = "bold"),
          legend.position = 'top', 
          legend.justification = 'left',
          plot.caption = element_text(hjust=0)) +
    labs(x='', y='', fill='',
         title = title2, subtitle = subtitle2, caption = caption2) 
  
  # Return Figure 2
  return(fig2)
}