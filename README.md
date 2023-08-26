# Kaggle Projects
Below is a list of data science projects that I have completed on Kaggle.

## Ecuador Store Sales
### [Source](https://www.kaggle.com/competitions/store-sales-time-series-forecasting) | [Project](https://www.kaggle.com/code/chongzhenjie/ecuador-store-sales-global-forecasting-lightgbm)
__Description:__ Predict grocery sales.
* Analyzed dataset involving daily sales of 1000+ store-product pairs over 5 years for forecasting.
* Examined the impact of holiday effects and zero sales problem, and built global LightGBM models to minimize RMSLE.

## Photo to Monet
### [Source](https://www.kaggle.com/competitions/gan-getting-started) | [Project](https://www.kaggle.com/code/chongzhenjie/monet-style-transfer-cyclegan-pytorch-lightning)
__Description:__ Use GANs to generate Monet-style images.
* Applied image augmentation to dataset consisting of 7038 photos and 300 Monet paintings, and implemented CycleGAN in PyTorch Lightning to perform photo-to-Monet translation.
* Built CycleGAN using U-Net generators and PatchGAN discriminators.

## NLP with Disaster Tweets
### [Source](https://www.kaggle.com/competitions/nlp-getting-started) | [Project](https://www.kaggle.com/code/chongzhenjie/disaster-tweets-basic-network-embeddings-bert)
__Description:__ Predict which Tweets are about real disasters and which ones are not.
* Preprocessed dataset of 10,000 tweets and learned our own word embeddings using neural networks in TensorFlow Keras for text classification.
* Compared performance against BERT, a pre-trained language model which generates word embeddings that are context-sensitive. Fine-tuning the BERT model achieved better results in terms of F1 score, showing the benefits of transfer learning.

## Predicting House Prices
### [Source](https://www.kaggle.com/competitions/house-prices-advanced-regression-techniques) | [Project](https://www.kaggle.com/code/chongzhenjie/house-prices-kernel-methods-tree-models)
__Description:__ Predict sales price for each house.
* Analyzed dataset containing 79 explanatory variables describing almost every aspect of residential homes in Ames, Iowa with the goal of predicting the sales price.
* Used univariate statistical tests (F-test statistics and mutual information) to help in feature selection and compared the performance of kernel methods and tree models in terms of RMSE.

## Walmart Store Sales
### [Source](https://www.kaggle.com/c/walmart-recruiting-store-sales-forecasting/overview) | [Project](https://www.kaggle.com/code/chongzhenjie/store-sales-time-series-forecasting-in-r)
__Description:__ Use historical data to predict store sales.
* Analyzed dataset involving weekly sales of 3000+ store-department pairs over 100+ weeks for forecasting.
* Transformed irregular time series, handled seasonality with STL decomposition, and built univariate time series models to minimize weighted MAE.

## Human or Robot
### [Source](https://www.kaggle.com/competitions/facebook-recruiting-iv-human-or-bot) | [Project](https://www.kaggle.com/code/chongzhenjie/human-or-robot-random-forest)
__Contributors:__ [Chong Zhen Kang (Shane)](https://github.com/shaneczk) and [Chong Zhen Jie](https://github.com/chongzhenjie).

__Description:__ Predict if an online bid is made by a machine or a human.
* Analyzed dataset of more than 7 million online auction bids to identify bids placed by robots.
* Carried out feature engineering, performed over-sampling on imbalanced data, and built ensemble models to maximize ROC AUC score.
