#%%
import numpy as np

def logitscore(x,W,b):
    z = W@x+b
    return z

def softmax(z):
    props = np.exp(z)/sum(np.exp(z))
    return props

def multinomialLogReg(X,W,b):
    z_vals = np.array([logitscore(x,W,b) for x in X])
    Probabilities = np.array([softmax(z) for z in z_vals])
    Predictions = np.array([np.argmax(prob) for prob in Probabilities])
    return Probabilities,Predictions

def crossEntropyLoss(Probabilities,Targets):
    CEloss = 0
    for prob,target in zip(Probabilities,Targets):
        CEloss += -np.log(prob[target])
    CEloss /= len(Targets)
    return CEloss

def stochGradDes(learningRate,epochs,target,features,weights,biases):

    target = target.astype(int)
    loss_list = np.zeros(epochs) #initiating an empty array

    for epoch in range(epochs):
        Probabilities, _ =multinomialLogReg(X=features,W=weights,b=biases)

        CEloss =crossEntropyLoss(Probabilities,target)
        loss_list[epoch] = CEloss
        Probabilities[np.arange(features.shape[0]),target] -= 1
    
    grad_weight = Probabilities.T @ features # gradient of loss w.r.t. weights
    grad_biases = np.sum(Probabilities, axis = 0).reshape(-1,1) # gradient of loss w.r.t. biases

    weights -= (learningRate * grad_weight)
    biases -= (learningRate * grad_biases)

    return weights,biases,loss_list 

class MultiLogReg:
    def __init__(self,X,y,K):
        self.X=X
        self.y=y
        self.W_init= np.random.rand(int(K),len(X[1]))
        self.b_init= np.random.rand(int(K))

    def train(self,learningRate,epochs):
        self.W,self.b,self.loss = stochGradDes(int(learningRate),int(epochs),self.y,self.X,self.W_init,self.b_init)
        return self.loss
    
    def predict(self,X):
        _,Predictions = multinomialLogReg(X,self.W,self.b)
        return Predictions