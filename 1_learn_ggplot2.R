# Learn ggplot2 in <1 hour!

# Run this line first
# If you get an error about there being no package run
# install.packages('ggplot2')
library(ggplot2)

# Now run this block of code

# Read in the data, remove first row, identify variable types, and missing values
glob_temp = read.csv('https://data.giss.nasa.gov/gistemp/tabledata_v3/GLB.Ts+dSST.csv',
                     skip = 1,
                     colClasses = 'numeric',
                     na.strings = '***')

# Add in the yearly standard deviation
glob_temp$J.D.sd = apply(glob_temp[,2:13], 1, 'sd', na.rm = TRUE)
# Add in lower and upper CIs
glob_temp$lower = with(glob_temp, J.D - 2 * J.D.sd)
glob_temp$upper = with(glob_temp, J.D + 2 * J.D.sd)

# Missing one obs - full 2016 data
ggplot(glob_temp, aes(x = Year, y = J.D, colour = J.D)) +
  geom_line(size = 1) + # Add thicker line
  theme_bw() + # Nicer theme
  scale_x_continuous(breaks = seq(1880, 2020, by = 10)) + # Better x-axis every 10 years
  ylab('Temperature\nanomaly in C') + # Proper axis label with
  ggtitle('NASA global surface temperature data (mean +/- 2 standard deviations)') +
  theme(axis.title.y = element_text(angle = 0, vjust = 1, hjust = 0)) + # Put y-axis label correctly
  theme(legend.position="none") + # Remove legend
  geom_errorbar(aes(ymin = lower, ymax = upper)) + # Add in vertical error bars
  geom_smooth(se = FALSE, method = 'loess') # Add in a smooth

# Behold the glory of what you have created!


# Back to the beginning ---------------------------------------------------

# Detail of this command
glob_temp = read.csv('https://data.giss.nasa.gov/gistemp/tabledata_v3/GLB.Ts+dSST.csv',
                     skip = 1,
                     colClasses = 'numeric',
                     na.strings = '***')
# Visit the URL above and look at the format
# Look at the help file for read.csv to see these arguments mean
# Use the command str(glob_temp) to see what the data set looks like
# What other types of data might you read in?

# Now these commands - use str(glob_temp) after running each to see what's changed
# Look up apply. 
# What does 2:13 mean and the square brackets?
# What does sd mean?
# Why is na.rm = TRUE?
# What does with do? 
glob_temp$J.D.sd = apply(glob_temp[,2:13], 1, 'sd', na.rm = TRUE)
glob_temp$lower = with(glob_temp, J.D - 2 * J.D.sd)
glob_temp$upper = with(glob_temp, J.D + 2 * J.D.sd)

# On to the graph ---------------------------------------------------------

# What does ggplot do?
# What is aes?
# What is geom_line?
p = ggplot(glob_temp, aes(x = Year, y = J.D, colour = J.D)) +
  geom_line(size = 1) + # Add thicker line
print(p)  
  
# Now successively add these in and see their effect
p = p + theme_bw()
p = p + scale_x_continuous(breaks = seq(1880, 2020, by = 10))
p = p + ylab('Temperature\nanomaly in C')
p = p + ggtitle('NASA global surface temperature data (mean +/- 2 standard deviations)')
p = p + theme(axis.title.y = element_text(angle = 0, vjust = 1, hjust = 0))
p = p + theme(legend.position="none")
p = p + geom_errorbar(aes(ymin = lower, ymax = upper))
p = p + geom_smooth(se = FALSE, method = 'loess')


# What else can I do? -----------------------------------------------------

# Lots of resources that go further than this in case you want to find out more:
# The ggplot2 documentation at: https://ggplot2.tidyverse.org
# Data visualisation chapter: https://r4ds.had.co.nz/data-visualisation.html
# The introduction guide by Matloff: http://heather.cs.ucdavis.edu/~matloff/GGPlot2/GGPlot2Intro.pdf
# Stack overflow ggplot2: http://stackoverflow.com/questions/tagged/ggplot2
# Rstudio cheat sheet: https://github.com/rstudio/cheatsheets/blob/master/data-visualization-2.1.pdf
# R-bloggers: https://www.r-bloggers.com/search/ggplot2
# ggplot2 extensions: https://exts.ggplot2.tidyverse.org


# An exercise -------------------------------------------------------------

#  Using the global temperature data can you re-create the plot called 'Monthly Mean Global Surface Temperature' on this page exactly: https://data.giss.nasa.gov/gistemp/graphs/? The data for the plot are at: http://data.giss.nasa.gov/gistemp/graphs/graph_data/Monthly_Mean_Global_Surface_Temperature/graph.csv