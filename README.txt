All relevant files are located in 'rudolf-experimentation'

analyse_cgt_bayesian_loss.R <- the current main file that drives the models.

cgt_romeu2020_model12_combined.stan <- the model code for the winning model (using a 2x2 data structure)

generate_synthetic_cambridge_gamble_data_combined.R <- a generative model that produces mock data for the winning model

distribution_graph.R <- this and similarly named were used for producing some graphs



'fitcache' <- folder where model fits go. github version is empty due to very large files.
'graphs' <- graphs produced by some of the scripts
'Non-winning models' <- a collection of various model files that were used in the model comparison stage
'output' <- .txt and hand-modified .xlsx output files after the model runs to completion
'synthetic_data <- in reality includes BOTH synthetic and real data. Real data is marked by 'real_data'