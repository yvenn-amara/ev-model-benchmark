from gru_utility import *
from torch.utils.data import TensorDataset, DataLoader
import sys
import torch.optim as optim
from ray import tune
import pickle

dataset = sys.argv[1]
train_start = pd.to_datetime(pd.read_csv('2. R/1. Outputs/'+dataset+'/train_dates.csv')['Start'])[0]
train_end = pd.to_datetime(pd.read_csv('2. R/1. Outputs/'+dataset+'/train_dates.csv')['End'])[0]

# print(train_start)
# print(train_end)

# Import data
df = pd.read_csv('2. R/1. Outputs/'+dataset+'/daily_sessions.csv')
# print(df.head())
df['Start_date'] = pd.to_datetime(df['Start_date'])
df = df[(df['Start_date'] >= train_start) & (df['Start_date'] <= train_end)]
# Lagged inputs
input_dim = 5
df = generate_time_lags(df, input_dim)

#### Creating additional temporal features
df.set_index('Start_date',inplace=True)

df_features = (
                df
                # .assign(hour = df.index.hour)
                # .assign(day = df.index.day)
                # .assign(month = df.index.month)
                .assign(dow = df.index.dayofweek)
                # .assign(week_of_year = df.index.week)
              )

#### One hot encoding of categorical features
df_features = onehot_encode_pd(df_features, 'dow')

# print(df_features)
#### Getting train and test sets
X_train, X_test, y_train, y_test= train_test_split(df_features.drop(['sessions'],axis=1), df[['sessions']], test_size = 0.2, shuffle = False)

# df_train = df[(df.index>=train_start) & (df.index<=train_end)]
# df_test = df[(df.index>=test_start) & (df.index<=test_end)]
#
# X_train = df_train.drop("sessions",axis=1)
# X_test = df_test.drop("sessions",axis=1)
# y_train = df_train["sessions"].values.reshape(-1,1)
# y_test = df_test["sessions"].values.reshape(-1,1)

# print(X_train)
# print(X_test)
# print(y_train)
# print(y_test)

#### Scaling features
scaler = get_scaler('robust')

scaler = MinMaxScaler()
X_train_arr = scaler.fit_transform(X_train)
# X_val_arr = scaler.transform(X_val)
X_test_arr = scaler.transform(X_test)

y_train_arr = scaler.fit_transform(y_train)
# y_val_arr = scaler.transform(y_val)
y_test_arr = scaler.transform(y_test)

#### Data loaders
train_features = torch.Tensor(X_train_arr)
train_targets = torch.Tensor(y_train_arr)
# val_features = torch.Tensor(X_val_arr)
# val_targets = torch.Tensor(y_val_arr)
test_features = torch.Tensor(X_test_arr)
test_targets = torch.Tensor(y_test_arr)

train = TensorDataset(train_features, train_targets)
# val = TensorDataset(val_features, val_targets)
test = TensorDataset(test_features, test_targets)

#### Training
def train_for_tune(config):
    batch_size = config['batch_size']
    train_loader = DataLoader(train, batch_size=batch_size, shuffle=False, drop_last=True)
    test_loader = DataLoader(test, batch_size=batch_size, shuffle=False, drop_last=True)
    input_dim = len(X_train.columns)
    output_dim = 1
    hidden_dim = config['hidden_dim']  # 64
    layer_dim = config['layer_dim']  # 3
    dropout = 0.1
    n_epochs = 1000
    learning_rate = config['learning_rate']
    weight_decay = 0

    model_params = {'input_dim': input_dim,
                    'hidden_dim': hidden_dim,
                    'layer_dim': layer_dim,
                    'output_dim': output_dim,
                    'dropout_prob': dropout}
    model = get_model('gru', model_params)

    loss_fn = nn.MSELoss(reduction="mean")

    optimizer = optim.Adam(model.parameters(), lr=learning_rate, weight_decay=weight_decay)
    opt = Optimization(model=model, loss_fn=loss_fn, optimizer=optimizer)
    opt.train(train_loader,
              test_loader,
              batch_size=batch_size, n_epochs=n_epochs, n_features=input_dim, verbose=False)
    tune.report(mean_loss=opt.val_losses[opt.best_epoch-1],best_epoch=opt.best_epoch)

if dataset=='paris':
    config = {"batch_size": tune.grid_search([1,2,3]),
              "hidden_dim": tune.grid_search([16,32,64]),
              "layer_dim": tune.grid_search([2,4,6]),
              "learning_rate": tune.grid_search([0.001, 0.01])}
else:
    config = {"batch_size": tune.grid_search([5,10,30]),
              "hidden_dim": tune.grid_search([16,32,64]),
              "layer_dim": tune.grid_search([2,4,6]),
              "learning_rate": tune.grid_search([0.001, 0.01])}

analysis = tune.run(train_for_tune, config=config,
                    metric='mean_loss', mode="min")

best_config = analysis.get_best_config(metric='mean_loss', mode="min")
best_config['n_epochs'] = analysis.best_dataframe['best_epoch'][0]
print("Best config: ", best_config)

with open('3. Python/1. Outputs/' + dataset + '/best_config_gru.pkl', 'wb') as f:
    pickle.dump(best_config, f)