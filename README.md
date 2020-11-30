# HotelReviewMachineLearning

This project uses hotel reviews from an open source location. The positive and negative reviews are seperated in two CSV files and saved to a MongoDB instance.

## Goal
The goal is to load reviews from hotels of different cities and countries and show their ratings in an interactive map. Along with this is a implementation of the NaiveBayes machine learning alghorithm that decides if a written review (of your own input) is a positive or negative review. 

## MongoDB
For this project I'm using different queries in Mongo to determine the sentiment of reviews before using it in the application. This is done because the dataset consists of 515K reviews, retrieving all of it and processing it would be too much and would not help the algorithm either.


