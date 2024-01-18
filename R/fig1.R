gen_fig1 <- function(df){
  #' Figure 1: Median age by year and chamber
  #' Get summaries required for Figure 1, and recreate plot using ggplot2.
  #' Input: dataframe
  #' Output: ggplot2 object
  
  # Get median age by year and chamber
  # Year previously extracted from start_date
  fig1_df <- df %>% 
    group_by(year, chamber) %>%
    summarize(median_age = median(age_years)) %>%
    ungroup()
  
  # Specify plot captions from 538
  title1 = "The House and Senate are older than ever before"
  subtitle1 = "Median age of the U.S. Senate and U.S. House by Congress, 1919 to 2023"
  caption1 = {
    "Data is based on all members who served in either the Senate or House in each Congress,
which is notated by the year in which it was seated. Any member who served in both chambers
in the same Congress was assigned to the chamber in which they cast more votes."
  }
  
  # Construct ggplot2 object
  fig1 <- fig1_df %>%
    ggplot(aes(x=year, y=median_age, col=chamber)) +
    geom_step(linewidth=0.75) +
    theme_bw() +
    scale_x_continuous(breaks = seq(1920, 2020, 20), limits = c(1919, 2023)) +
    scale_y_continuous(breaks = seq(45, 65, 5), limits = c(45, 66)) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(face = "bold"),
          legend.position = 'top', 
          legend.justification = 'left',
          plot.caption = element_text(hjust=0)) +
    labs(x='', y='', col='',
         title=title1, subtitle=subtitle1, caption=caption1) 
  
  # Return Figure 1
  return(fig1)
}