#Libraries
from __future__ import division

from sklearn import cluster, datasets, mixture
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt
import pandas as pd
import torch
import torch.nn as nn 
from math import pi 

from torch.distributions.multivariate_normal import MultivariateNormal
from torch.distributions import LogisticNormal
import numpy as np

import pickle

#Helper Classes
class R_NVP(nn.Module):
    def __init__(self, d, k, hidden):
        super().__init__()
        self.d, self.k = d, k
        self.sig_net = nn.Sequential(
                    nn.Linear(k, hidden),
                    nn.LeakyReLU(),
                    nn.Linear(hidden, d - k))

        self.mu_net = nn.Sequential(
                    nn.Linear(k, hidden),
                    nn.LeakyReLU(),
                    nn.Linear(hidden, d - k))

    def forward(self, x, flip=False):
        x1, x2 = x[:, :self.k], x[:, self.k:] 

        if flip:
            x2, x1 = x1, x2
        
        # forward
        sig = self.sig_net(x1)
        z1, z2 = x1, x2 * torch.exp(sig) + self.mu_net(x1)
        
        if flip:
            z2, z1 = z1, z2
        
        z_hat = torch.cat([z1, z2], dim=-1)
        
        log_pz = base_dist.log_prob(z_hat)
        log_jacob = sig.sum(-1)
        
        return z_hat, log_pz, log_jacob
    
    def inverse(self, Z, flip=False):
        z1, z2 = Z[:, :self.k], Z[:, self.k:] 
        
        if flip:
            z2, z1 = z1, z2
        
        x1 = z1
        x2 = (z2 - self.mu_net(z1)) * torch.exp(-self.sig_net(z1))
        
        if flip:
            x2, x1 = x1, x2
        return torch.cat([x1, x2], -1)

class stacked_NVP(nn.Module):
    def __init__(self, d, k, hidden, n):
        super().__init__()
        self.bijectors = nn.ModuleList([
            R_NVP(d, k, hidden=hidden) for _ in range(n)
        ])
        self.flips = [True if i%2 else False for i in range(n)]
        
    def forward(self, x):
        log_jacobs = []
        
        for bijector, f in zip(self.bijectors, self.flips):
            x, log_pz, lj = bijector(x, flip=f)
            log_jacobs.append(lj)
        
        return x, log_pz, sum(log_jacobs)
    
    def inverse(self, z):
        for bijector, f in zip(reversed(self.bijectors), reversed(self.flips)):
            z = bijector.inverse(z, flip=f)
        return z

def train(model, epochs, batch_size, optim, scheduler,triplet):
    losses = []
    for _ in range(epochs):

        # get batch 
        time = [t[4] for t in triplet]
        X, _ = triplet, time
        X = torch.from_numpy(StandardScaler().fit_transform(X)).float()

        optim.zero_grad()
        z, log_pz, log_jacob = model(X)
        loss = (-log_pz - log_jacob).mean()
        losses.append(loss)

        loss.backward()
        optim.step()
        scheduler.step()
    return losses

def view(model, losses):
    plt.plot(losses)
    plt.title("Model Loss vs Epoch")
    plt.xlabel('Epochs')
    plt.ylabel('Loss')
    plt.show()

    X_hat = model.inverse(Z).detach().numpy()
    plt.scatter(X_hat[:, 3], X_hat[:, 4])
    plt.title("Inverse of Normal Samples Z: X = F^-1(Z)")
    plt.show()

    n_samples = 3000
    time = [t[0] for t in triplet]
    X, _ = triplet, time
    X = torch.from_numpy(StandardScaler().fit_transform(X)).float()
    z, _, _ = model(X)
    z = z.detach().numpy()
    plt.scatter(z[:, 0], z[:, 1])
    plt.title("Transformation of Data Samples X: Z = F(X)")
    plt.show()
    
#Loading Data
n_samples = 2000
base_mu, base_cov = torch.zeros(5), torch.eye(5)
base_dist = MultivariateNormal(base_mu, base_cov)
Z = base_dist.rsample(sample_shape=(3000,))

filename = 'palo_alto.csv'
def load_data(filename):
    location = 'Data/'+filename
    data = pd.read_csv(location)
    return data

data = load_data(filename)
data['just_date'] = pd.to_datetime(data['Start']).dt.date
data['time']=(pd.to_datetime(data.Start).dt.minute+pd.to_datetime(data.Start).dt.hour*60)#/(24*60)

models=[]

unique_dates=list(data.just_date.unique())

for unique_date in unique_dates:
    print(unique_date)
    day_data=data[data.just_date==unique_date]
    #print(day_data)
    time=day_data.time.to_list()
    arrival=day_data.Arrival.to_list()
    c_dur = day_data['Charge.Duration'].to_list()
    p_dur = day_data['Park.Duration'].to_list()
    energy = day_data.Energy.to_list()

    triplet=[[aset[0],aset[1],aset[2],aset[3],aset[4]]for aset in zip(arrival,c_dur,p_dur,energy,time)]
    X,y = triplet,time
    #Normalize
    scal = StandardScaler()
    scal.fit(X)
    X = scal.transform(X)
    X=np.asarray(X)

    # Model Parameters
    d = 5
    k = 1

    #Training on layers
    model = R_NVP(d, k, hidden=512)
    models.append(model)
    optim = torch.optim.Adam(model.parameters(), lr=1e-3)
    scheduler = torch.optim.lr_scheduler.ExponentialLR(optim, 0.999)
    n_samples = 512

    # training loop
    losses = train(model, 100, n_samples, optim, scheduler,triplet)
    losses1=[losses[i].detach().numpy() for i in range(len(losses))]

    X_hat = model.inverse(Z).detach().numpy()
    X_hat=scal.inverse_transform(X_hat)
    pred=pd.DataFrame(X_hat,columns=['Arrival', 'Charge.Duration',
           'Park.Duration', 'Energy', 'time'])    
    
#save models
with open('NAF_Models.pkl', 'wb') as file:
    pickle.dump(models, file)    