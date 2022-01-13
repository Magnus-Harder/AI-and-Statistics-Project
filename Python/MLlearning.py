import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.naive_bayes import MultinomialNB
from sklearn.datasets import make_blobs
X, y = make_blobs(n_samples=50, centers=5, n_features=3, random_state=0)

def MLlearning(X, y, model1=LogisticRegression, model2=MultinomialNB):
    # model1 and model2 are sklearn functions

    # Split into test and training set
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=42)
    
    # Potentially do some cross validation?


    # models
    m1 = model1(max_iter=1000, random_state=42).fit(X_train,y_train)
    print(m1.predict(X_test))
    m2 = model2().fit(X_train-np.min(X_train),y_train)
    print(m2.predict(X_test))

    # predictions
    m1_pred_train = m1.predict_proba(X_train)
    m1_pred_test = m1.predict_proba(X_test)

    m2_pred_train = m2.predict_proba(X_train)
    m2_pred_test = m2.predict_proba(X_test)

    return(y_train, y_test, m1_pred_train, m1_pred_test, m2_pred_train, m2_pred_test, m1.coef_, m2.coef_)
