import numpy as np
from sklearn.model_selection import KFold
from sklearn.linear_model import LogisticRegression

def Bias_left(X, y, X_left, n_splits=10, max_iter=1000):

    kf = KFold(n_splits=int(n_splits),random_state=0,shuffle=True)

    y_pred = np.zeros(len(y))
    y_pred_left  = np.zeros(len(y))


    for train_index,test_index in kf.split(X,y):
        X_train,y_train= X[train_index],y[train_index]
        X_test,y_test = X[test_index],y[test_index]
        X_test_left = X_left[test_index]


        # Training and testing LogRegModel
        LogRegModel = LogisticRegression(max_iter=max_iter, random_state=1).fit(X_train,y_train)
        y_pred[test_index] = LogRegModel.predict(X_test)
        y_pred_left[test_index] = LogRegModel.predict(X_test_left)

    
    return y_pred,y_pred_left

def Model_left(X, y, n_splits=10, max_iter=1000):

    kf = KFold(n_splits=int(n_splits),random_state=0,shuffle=True)

    y_pred = np.zeros(len(y))

    for train_index,test_index in kf.split(X,y):
        X_train,y_train= X[train_index],y[train_index]
        X_test,y_test = X[test_index],y[test_index]


        # Training and testing LogRegModel
        LogRegModel = LogisticRegression(max_iter=max_iter, random_state=1).fit(X_train,y_train)
        y_pred[test_index] = LogRegModel.predict(X_test)

    
    return y_pred
