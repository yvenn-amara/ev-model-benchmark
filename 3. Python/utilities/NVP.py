# Import required packages
import torch
import numpy as np
import normflow as nf

from matplotlib import pyplot as plt
from tqdm import tqdm
import pickle as pkl

# Import Data
from os import listdir
import pandas as pd
cwd0 = '2. R/1. Outputs/'
datasets = [f for f in listdir(cwd0)]
dataset=input("Datasets Available: " + ', '.join(datasets) + "\nSelect Dataset: ") +"/"

cwd1 = '2. R/1. Outputs/' + dataset
cwd3 = '3. Python/1. Outputs/' + dataset

df = pd.read_csv(cwd1 + "sarima/cleaned_train.csv")
df['Start'] = pd.to_datetime(df['Start']).dt.round('1min')

raw_rows = len(df)

duration = input("Choose Duration Type (Park.Duration/Charge.Duration):")
df = df[(df[duration] > 0) & (df['Energy']>0)]# Remove impossible sessions (negative charge/park duration or negative energy)
print("Impossible sessions discarded: ", (raw_rows-len(df))/raw_rows, "%")

triple = df[["Arrival",duration,"Energy"]].values
print(triple)

x = torch.tensor(triple)
# Set up model #

# Define flows
K = 4
torch.manual_seed(0)

latent_size = 3
b = torch.Tensor([1 if i % 3 == 0 else 0 for i in range(latent_size)])
flows = []
for i in range(K):
    s = nf.nets.MLP([latent_size, 3 * latent_size, latent_size], init_zeros=True)
    t = nf.nets.MLP([latent_size, 3 * latent_size, latent_size], init_zeros=True)
    if i % 3 == 0:
        flows += [nf.flows.MaskedAffineFlow(b, t, s)]
    else:
        flows += [nf.flows.MaskedAffineFlow(1 - b, t, s)]
    # flows += [nf.flows.ActNorm(latent_size)]

# Define 3D base distribution
base = nf.distributions.base.DiagGaussian(3)

# Set prior and q0
# prior = nf.distributions.TwoModes(2, 0.1)
# q0 = nf.distributions.DiagGaussian(2)

# Construct flow model
# nfm = nf.NormalizingFlow(q0=q0, flows=flows, p=prior)
nfm = nf.NormalizingFlow(q0=base, flows=flows)
print(nfm)
# Move model on GPU if available
enable_cuda = True
device = torch.device('cuda' if torch.cuda.is_available() and enable_cuda else 'cpu')
nfm = nfm.to(device)
nfm = nfm.double()

# Initialize ActNorm
z, _ = nfm.sample(num_samples=2 ** 7)
z_np = z.to('cpu').data.numpy()
# plt.figure(figsize=(15, 15))
# plt.hist2d(z_np[:, 0].flatten(), z_np[:, 1].flatten())
# plt.gca().set_aspect('equal', 'box')
# plt.show()

# Plot prior distribution
grid_size = 10
# xx, yy = torch.meshgrid(torch.linspace(-3, 3, grid_size), torch.linspace(-3, 3, grid_size))
xx, yy, tt = torch.meshgrid(torch.linspace(-3, 3, grid_size), torch.linspace(-3, 3, grid_size), torch.linspace(-3, 3, grid_size))
zz = torch.cat([xx.unsqueeze(3), yy.unsqueeze(3), tt.unsqueeze(3)], 3).view(-1, 3)
# print(zz)
zz = zz.double().to(device)
# log_prob = prior.log_prob(zz).to('cpu').view(*xx.shape)
# prob_prior = torch.exp(log_prob)

# Plot initial posterior distribution
log_prob = nfm.log_prob(zz).to('cpu').view(*xx.shape)
prob = torch.exp(log_prob)
prob[torch.isnan(prob)] = 0

# plt.figure(figsize=(10, 10))
# plt.pcolormesh(xx, yy, prob.data.numpy())
# plt.contour(xx, yy, prob_prior.data.numpy(), cmap=plt.get_cmap('cool'), linewidths=2)
# plt.gca().set_aspect('equal', 'box')
# plt.show()

# Train model
# max_iter = 20000
max_iter = 500
# num_samples = 2 * 10
# anneal_iter = 10000
anneal_iter = 500
annealing = False
# show_iter = 1000
show_iter = 50

loss_hist = np.array([])

optimizer = torch.optim.Adam(nfm.parameters(), lr=1e-4, weight_decay=1e-6)
for it in tqdm(range(max_iter)):
    optimizer.zero_grad()
    if annealing:
        # loss = nfm.reverse_kld(num_samples, beta=np.min([1., 0.001 + it / anneal_iter]))
        loss = nfm.forward_kld(x)
    else:
        # loss = nfm.reverse_alpha_div(num_samples, dreg=True, alpha=1)
        loss = nfm.forward_kld(x)

    if ~(torch.isnan(loss) | torch.isinf(loss)):
        loss.backward()
        optimizer.step()

    loss_hist = np.append(loss_hist, loss.to('cpu').data.numpy())

    # Plot learned posterior
    if (it + 1) % show_iter == 0:
        log_prob = nfm.log_prob(zz).to('cpu').view(*xx.shape)
        prob = torch.exp(log_prob)
        prob[torch.isnan(prob)] = 0
        print(loss_hist[-1])
        # print(log_prob)

        # plt.figure(figsize=(15, 15))
        # plt.pcolormesh(xx, yy, prob.data.numpy())
        # plt.contour(xx, yy, prob_prior.data.numpy(), cmap=plt.get_cmap('cool'), linewidths=2)
        # plt.gca().set_aspect('equal', 'box')
        # plt.show()

plt.figure(figsize=(10, 10))
plt.plot(loss_hist, label='loss')
plt.legend()
plt.show()

# Plot learned posterior distribution
log_prob = nfm.log_prob(zz).to('cpu').view(*xx.shape)
prob = torch.exp(log_prob)
prob[torch.isnan(prob)] = 0

# x_sample, _ = nfm.sample(1000)
with torch.no_grad():
    x_sample = nfm(nfm.sample(1))
    print(x_sample)
    print(x_sample.mean(0))
    print(x.mean(0))

with open(cwd3 + "NVP_" + duration + ".pkl", "wb") as fp:
    pkl.dump(nfm, fp)

# plt.figure(figsize=(15, 15))
# plt.pcolormesh(xx, yy, prob.data.numpy())
# plt.contour(xx, yy, prob_prior.data.numpy(), cmap=plt.get_cmap('cool'), linewidths=2)
# plt.gca().set_aspect('equal', 'box')
# plt.show()