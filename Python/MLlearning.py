#%%
import numpy as np
from sklearn.model_selection import train_test_split,KFold
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import RandomForestClassifier
from toolbox_02450.statistics import mcnemar
from sklearn.datasets import make_blobs



X, y = make_blobs(n_samples=50, centers=5, n_features=3, random_state=0)

#%%


def MLlearning(X, y, n_trees=[10,20], n_splits_outer=5,n_splits_inner=10, test_size=0.1 , max_iter=1000):

    kf_outer = KFold(n_splits=n_splits_outer,random_state=0,shuffle=True)
    kf_inner = KFold(n_splits=n_splits_inner,random_state=0,shuffle=True)

    y_pred_MLR = np.zeros(len(y))
    y_pred_RF = np.zeros((len(n_trees),len(y)))
    
    mc_inner = []
    y_true_inner = []
    y_pred_RF_inner = []
    for train_index,test_index in kf_outer.split(X,y):
        X_cross,y_cross = X[train_index],y[train_index]
        X_test,y_test = X[test_index],y[test_index]

        # Setting array for storing predections of order.
        predictions_RF  = np.empty((len(n_trees),len(y_cross)))
        predictions_true = np.empty(len(y_cross))


        
        # Initiating Kfolds loop optimization of RandomForestClassifier
        for train_index_inner,test_index_inner in kf_inner.split(X_cross,y_cross):
            X_train = X_cross[train_index_inner]
            y_train = y_cross[train_index_inner]
            X_vali  = X_cross[test_index_inner]
            y_vali  = y_cross[test_index_inner]
            
            # Train and test for each nr_trees
            for idx,n in enumerate(n_trees):
                RF_model = RandomForestClassifier(n_estimators=int(n)).fit(X_train,y_train)
                predictions_RF[idx,test_index_inner] = RF_model.predict(X_vali)
            
            predictions_true[test_index_inner] = y_vali
        

        mc_inner.append(mcnemar(y_true=y_cross,yhatA=predictions_RF[0,:],yhatB=predictions_RF[1,:]))
        y_pred_RF_inner.append(predictions_RF)
        y_true_inner.append(y_cross)




        # Training and testing LogRegModel
        LogRegModel = LogisticRegression(max_iter=max_iter, random_state=0).fit(X_cross,y_cross)
        y_pred_MLR[test_index] = LogRegModel.predict(X_test)

        # Predictions and testing for RandomForestClassifier

        for idx,n in enumerate(n_trees):
            RF_model = RandomForestClassifier(n_estimators=int(n)).fit(X_cross,y_cross)
            y_pred_RF[idx,test_index] =  RF_model.predict(X_test)
        
    return y_pred_MLR,y_pred_RF,y_pred_RF,mc_inner



test = MLlearning(X,y)
#test