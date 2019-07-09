# Interpretation with dependent features


- Dependent or correlated features complicate interpretation of machine learning and statistical models
- Survey of approaches and proposals how to improve methods
- One problem: Using marginal distribution instead of conditional distribution
- Issues that occur:
  - Extrapolation
  - Reduced importance of correlated features in random forests
  - estimation problems in linear models
  - Precision of estimated regression coefficients decreases (linear model)
  - Can decrease predictive performance
- Things to improve: 
  - Permutation feature importance
  - PDP
  - LIME
  - Shapley
  - ICE
  - Causal interpretation
- Tools to analyse:
  - Variance inflatiion factor (VIF) for linear models
  - Correlation matrix (pearson, spearman, kendall)
  - dependence measures (predict each from the other)
  - Dimensionality reduction tools like (sparse) PCA
  - for linear models: https://onlinelibrary.wiley.com/doi/full/10.1111/j.1600-0587.2012.07348.x
- Tools to fix:
  - Removing dependence before modeling
    - remove feature
    - replace feature with e.g. PCA features
    - orthogonalize features (e.g. with sequential regression or residual regression)
  - Modelling with latent variables
    - Principal component regression (PCR)
    - Regularization. e.g. for linear models.
  - Distributional models, e.g. trees
  - correctly handling conditional distribution, e.g. strobl random  forest
  - PCA 
- Issue will always remain when dependent features studied in isolation
- Possible fixes:
  - Multi-way interaction analysis
  - Manipulate both features simultaneously
  - Causal interpretation, DAG
  - Dimensionality reduction, PCA

"We do not think that the problem of collinearity can be solved, for logical reasons: without mechanistic ecological understanding, collinear variables cannot be separated by statistical means." - https://onlinelibrary.wiley.com/doi/full/10.1111/j.1600-0587.2012.07348.x

TODO:

- Checkout how problems are handled in linear model (see multicollinearity)
- 

## Novelties

Looks at more common case of dependence instead of correlation / multicollinearity.
Looks at interpretation of machine learning.
Provides solutions.

## Correlation and Dependence

Corrleation is linear dependence.
In linear model, mainly correlation is relevant.
But when model can handle more complex relationships, also other dependencies between features is relevant.

## Mutlicollinearity in linear models

Multicollinearity is when one feature can be linearly predicted with some accuracy from the other features.

## Marginal vs. Conditional Distribution

The problem of correlated features can be seen as the problem from moving from a marginal to a conditional distribution.

When a feature is independent from all other features, its marginal distribution $P(X_j)$ is the same as the distribution conditional on all other features $P(X_j|X_C)$.

When moving from independent to dependent feature, many interpretation methods such as permutation feature importance do not work any longer, since they rely on the marginal distribution, but now we have to use the conditional distribution.



