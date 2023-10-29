# r_machine_learning

Code in R that demonstrates different ML analyses: Support Vector Machines, Principal Components Analysis (PCA), K-Means Clustering, and Hierarchical Clustering.

For regression modeling, the code showcases techniques such as polynomial regression, piecewise constant functions, and splines. It fits polynomial models of different degrees to the "Wage" dataset, uses cut function to create bins for age, and fits piecewise constant functions. Additionally, it applies cubic splines with user-defined knots and natural cubic splines with knots chosen by R.

The code also includes examples of classification trees and regression trees. It builds classification trees using the "Heart.csv" dataset and regression trees using the "Boston" dataset. It provides summaries, graphical representations, and performs cross-validation to determine optimal tree sizes. Pruning is applied to improve the models.

Bagging and random forests are demonstrated using the randomForest package on the "Boston" dataset. The code calculates out-of-bag mean squared error and variable importance measures.

Boosting is showcased using the gbm package on the "Boston" dataset. The code builds a gradient boosting model, provides a summary, makes predictions, and calculates mean squared error.

In addition to regression and classification techniques, the code also covers binary classification using Support Vector Machines with linear and radial kernels. It evaluates model performance using confusion matrices.

Principal Components Analysis (PCA) is performed on the USArrests dataset, calculating mean, variance, loading vectors, and principal component scores. A biplot is created for visualization.

K-Means Clustering is applied to simulated datasets with different numbers of clusters. The results are plotted and compared to the true clusters. The effect of the number of initial cluster assignments is also examined.

Hierarchical Clustering is conducted using different linkage methods on simulated datasets. Dendrograms are plotted, clusters are cut at different levels, and scaling is applied to variables before conducting hierarchical clustering.

Overall, this code provides a comprehensive overview of various machine learning analyses in R, covering both regression and classification techniques as well as clustering and dimensionality reduction methods.

Binary classification using a linear kernel. Generates a dataset, fits an SVM model with a linear kernel, and evaluates the model's performance using a confusion matrix. It then extends the previous binary classification scenario by using a radial kernel. The code generates another dataset with two distinct classes, fits an SVM model with a radial kernel, and evaluates the model's performance using a confusion matrix.

PCA is performed on the USArrests dataset, which is loaded into the environment. The mean and variance of the variables are calculated, and then PCA is conducted with scaling. The principal component loading vectors and matrix of principal component scores are then extracted and printed. A biplot is also created to visualize the results of the PCA.

K-Means Clustering is performed on a simulated dataset with two clusters, and then with three clusters. The results are plotted and compared to the true clusters. The effect of the number of initial cluster assignments is also examined.

Hierarchical Clustering is conducted on the simulated dataset with three different linkage methods: complete, average, and single. Dendrograms are plotted for each method, and the clusters are cut at different levels to compare to the true clusters. The variables are also scaled before conducting hierarchical clustering, and a dendrogram is plotted for this scaled data as well.
