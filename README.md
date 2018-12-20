# Bitcoin Price Prediction

## Project description:
A set of transformed cryptocurrency's price is given so there is no way of knowing which one it is. The goal is to construct 60 days ahead forecast using the giving historical data.

## Criteria for the project results:
1. Root Mean Squared Error (RMSE) for the predicted values. We try to find the smallest RMSE.
2. Calibration of the 95% prediction intervals. Your constructed 95% prediction intervals should on average contain a value as closest as possible to 95% of observations.
3. Sharpness of the 95% prediction intervals. If your 95% prediction intervals are generally calibrated, you also would like them to be shorter.

## Approaches:
1. Detect seasonality and trend of the data. Remove them if needed.
2. Simple ARMA.
3. ARFIMA.
4. Add Garch to above time series models for more accurate prediction.
5. Long short-term memory neural network(1 day prediciton window vs 60 days prediction window).

## Results:
Deep learning technique Long Short-Term Memory didn't reduce the RMSE on validation data as much as I thought. In the end ARMA(5,14)+GARCH(10,2) is chosen to make the 60 days prediction for submission. Achieved second place in the class.

## Improvements:
The cryptocurrency turns out to be bitcoin. The first place team used the last 1/3 of the data, so they were able tp pick up the mostly upward trend in 2017 and made a better prediction.
