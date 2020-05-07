# Before using this code, load the "data_brexit_referendum_adjusted.csv"
# file from Blackboard. Read through "brexit example.R" for full details
# on how this file was generated

# the main purpose of principal component analysis is to:
# identify hidden patterns in a dataset,
# reduce the dimensionnality of the data by removing the noise and redundancy in the data,
# identify correlated variables


data_file <- read.csv("data_brexit_referendum_adjusted.csv")

# Principal Component Analysis (PCA) wrks best with numerical data
# so I'm checking that all data is now numeric first

data_numeric_variables <- sapply(data_file, is.numeric)
data_numeric_variables

# In the earlier lecture I created a "Proportion" variable using
# the variables "NVotes" and "Leave". Both these variables are correlated
# with "Proportion" and will produce false relationships in PCA.
data_numeric_variables["NVotes"] <- FALSE
data_numeric_variables["Leave"] <- FALSE

# Now I'll remove all non-numeric data and the other 2 variables
data_file_adjusted <- data_file[, data_numeric_variables]

# Passing this numeric data (23 variables) into the prcomp() function
# and setting two arguments, center and scale, to be TRUE. 
# Then we can have a peek at the PCA object with summary().
pca <- prcomp(data_file_adjusted, center = TRUE, scale. = TRUE)
summary(pca)

# We obtain 23 principal components, which are called PC1-23. Each of these 
# explains a percentage of the total variation in the dataset. 
# That is to say: PC1 explains 40% of the total variance, which means that 
# nearly half of the information in the dataset (23 variables) can be 
# encapsulated by just that one Principal Component. 
# PC2 explains 26% of the variance. So, by knowing the position of 
# a sample in relation to just PC1 and PC2, you can get a very 
# accurate view on where it stands in relation to other samples, 
# as just PC1 and PC2 can explain 66% of the variance.

# Variables that do not correlated with any PC or correlated with 
# the last dimensions are variables with low contribution and might 
# be removed to simplify the overall analysis.

# Let's call str() to have a look at your PCA object.
# The center point ($center), 
# scaling ($scale), 
# standard deviation(sdev) of each principal component
# The relationship (correlation or anticorrelation, etc) between the 
# initial variables and the principal components ($rotation)
# The values of each sample in terms of the principal components ($x)

str(pca)

# ---------------------------------------------------------------------
# Eigenvalues / Variances
# ---------------------------------------------------------------------

# eigenvalues measure the amount of variation retained by each principal component. 
# Eigenvalues are large for the first PCs and small for the subsequent PCs. 
# That is, the first PCs corresponds to the directions with the maximum amount of
# variation in the data set.
# We examine the eigenvalues to determine the number of principal components to be considered. 
# The eigenvalues and the proportion of variances (i.e., information) retained by the 
# principal components (PCs) can be extracted using the function get_eigenvalue() 
# from the factoextra package.
install.packages("factoextra")
library("factoextra")
eig_values <- get_eigenvalue(pca)
eig_values

# Eigenvalues can be used to determine the number of principal components 
# to retain after PCA (Kaiser 1961): 
# See Kaiser, Henry F. 1961. “A Note on Guttman’s Lower Bound for the Number 
# of Common Factors.” British Journal of Statistical Psychology 14: 1–2.

# We can also use the FactoMineR package to display this information
library("FactoMineR")
pca2 <- PCA(data_file_adjusted, graph = FALSE)
print(pca2)
pca2_eig_values <- get_eigenvalue(pca2)
pca2_eig_values

# Unfortunately, there is no well-accepted objective way to decide how many principal components are enough. 
# This will depend on the specific field of application and the specific data set. In practice, we tend to 
# look at the first few principal components in order to find interesting patterns in the data.

# In our analysis, the first three principal components explain 74% of the variation. This is an acceptably large percentage.

# An alternative method to determine the number of principal components is to look at a Scree Plot, 
# which is the plot of eigenvalues ordered from largest to the smallest. The number of component 
# is determined at the point, beyond which the remaining eigenvalues are all relatively small and of 
# comparable size (Jollife 2002, Peres-Neto, Jackson, and Somers (2005)).
# See Jollife, I.T. 2002. Principal Component Analysis. 2nd ed. New York: Springer-Verlag. https://goo.gl/SB86SR.

fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50))
# From the plot, we might want to stop at the fifth principal component. 
# 87% of the information (variances) contained in the data are retained by the first five principal components. 

# A simple method to extract the results, for variables, from a PCA output 
# is to use the function get_pca_var() from the factoextra package. 
# This function provides a list of matrices containing all the results 
# for the active variables (coordinates, correlation between variables and axes, squared cosine and contributions)

# The components of the get_pca_var() can be used in the plot of variables as follow:
# var$coord: coordinates of variables to create a scatter plot
# var$cos2: represents the quality of representation for variables on the factor map. 
# It’s calculated as the squared coordinates: var$cos2 = var$coord * var$coord.
# var$contrib: contains the contributions (in percentage) of the variables to 
# the principal components. The contribution of a variable (var) to a given principal 
# component is (in percentage) : (var.cos2 * 100) / (total cos2 of the component).

pca_for_variables <- get_pca_var(pca)
pca_for_variables



# -----------------------------------------------------------------------
# Using Correlation plot
# -----------------------------------------------------------------------

# We can look at the "cumulative proportion" line to see this
# value across all variables eg PC1-3 = 75% of data

# This plot shows the variamces in squared standard deviations
# from the summary() results.
# We can see how each subsequent principal component captures a lower
# amount of total variance.

# It is possible to use the function corrplot() function  to highlight 
# the most contributing variables for each dimension:
library("corrplot")
corrplot(pca_for_variables$cos2, is.corr = FALSE)

# Looks like "AdultMeanAge", "Owned", "OenedOutrght" etc all
# contribute to PC 1.

# A variable correlation plots shows the relationships between 
# all variables. It can be interpreted as follows;

# Positively correlated variables are grouped together.
# Negatively correlated variables are positioned on opposite sides of the plot origin (opposed quadrants).
# The distance between variables and the origin measures the quality of the variables on 
# the factor map. Variables that are away from the origin are well represented on the factor map.

fviz_pca_var(pca, col.var = "black")

# -----------------------------------------------------------------------
# Cos2 - quality of representation
# -----------------------------------------------------------------------
# The quality of representation of the variables on factor map is called cos2 (square cosine, squared coordinates). 
# We can access to the cos2 as follows:
head(pca_for_variables$cos2, 10)

# We can show a bar plot of variables cos2 using the function fviz_cos2()
# from the in factoextra library.

# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(pca, choice = "var", axes = 1:2)

# Note that, A high cos2 indicates a good representation of the variable 
# on the principal component. In this case the variable is positioned close 
# to the circumference of the correlation circle (shown above).

# A low cos2 indicates that the variable is not perfectly represented by the PCs. 
# In this case the variable is shown close to the centre of the circle eg ID

# Note - the sum of all cos2 values across all PC's = 1. For example, if a variable
# is perfectly represented by only two principal components (Dim.1 & Dim.2), 
# the sum of the cos2 on these two PCs is equal to one. Therefore the variables will be positioned 
# on the circle of correlations.
#  The closer a variable is to the circle of correlations, the better its representation 
# on the factor map (and the more important it is to interpret these components)
# Variables that are closed to the centre of the plot are less important for the first components. 

# -----------------------------------------------------------------------
# Biplot
# -----------------------------------------------------------------------

# A biplot is a type of plot that will allow you to visualise how 
# the samples relate to one another in our PCA (which samples are 
# similar and which are different) and will simultaneously reveal how 
# each variable contributes to each principal component.

# It’s possible to colour variables by their cos2 values using the argument col.var = "cos2". 
# This produces a gradient of colours. In this case, the argument gradient.cols can be used 
# to provide a custom colour.

# variables with low cos2 values will be colored in red
# variables with mid cos2 values will be colored in blue
# variables with high cos2 values will be colored in green

# Colour by cos2 values: quality on the factor map
fviz_pca_var(pca, col.var = "cos2",
             gradient.cols = c("red", "Blue", "Green"), 
             repel = TRUE # Avoid text overlapping
)  

# Contribution of variables to each PC
# The larger the value of the contribution, the more the variable contributes to the component. 
head(pca_for_variables$contrib, 20)

# The most important (or, contributing) variables can be highlighted on the correlation plot as follows
fviz_pca_var(pca, col.var = "contrib",
             gradient.cols = c("red", "Blue", "Green"),
)


# We can use the function fviz_contrib() from the factoextra package
# to draw a bar plot of variable contributions. If your data 
# contains many variables, you can decide to show only the top 
# contributing variables. This code shows the top 20 variables 
# contributing to the principal components:
# Contributions of variables to PC1
install.packages("factoextra")
library(factoextra)
?fviz_contrib
fviz_contrib(pca, choice = "var", axes = 1, top = 20)

# Contributions of variables to PC2
fviz_contrib(pca, choice = "var", axes = 2, top = 20)
# Contribution to PC1 - PC5
fviz_contrib(pca, choice = "var", axes = 1:5, top = 20)

# The red dashed line on the graphs indicate the expected 
# average contribution. If the contribution of the variables were 
# uniform, the expected value would be 1/length(variables) = 1/23 = 4.3%. 
# For a given component, a variable with a contribution larger than this 
# cutoff could be considered as important in contributing to the component. 

# We can see that the following  "Owned", "Age_45_Above", "OwnedOutright" "Unemp", "UnempRate_EA"
# contribute most to Dim1 - same as findings in cor_plot.
# "Low_education_level", "High_Education_Level", "Proportion" and "HighOccup" contribute most to PC 2.

# Colour output by groups 
# We can colour individual elements by group. 
# And we can add concentration ellipses and confidence ellipses by groups.

fviz_pca_ind(pca,
             axes = c(1, 2),
             geom.ind = "point", # show points only (but not "text values")
             col.ind = data_file$Vote, # colour by groups
             palette = c("Red", "Green"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Vote"
)

# Graphical parameters
# We can change the graphical parameters using the function ggpar() from the ggpubr package.

biplot <- fviz_pca_ind(pca, geom = "point", col.ind = data_file$Vote)
ggpubr::ggpar(biplot,
              title = "Principal Component Analysis",
              subtitle = "Brexit dataset",
              caption = "Source: BBC",
              xlab = "PC 1", ylab = "PC 2",
              legend.title = "Vote", legend.position = "top",
              ggtheme = theme_gray(), palette = "jco")

# Lets see how PC 3 and PC 4 represent voters data.
biplot <- fviz_pca_ind(pca, 
                       axes = c(3, 4),
                       geom = "point", 
                       col.ind = data_file$Vote)
ggpubr::ggpar(biplot,
              title = "Principal Component Analysis",
              subtitle = "Brexit dataset",
              caption = "Source: BBC",
              xlab = "PC 3", ylab = "PC 4",
              legend.title = "Vote", legend.position = "top",
              ggtheme = theme_gray(), palette = "jco")
