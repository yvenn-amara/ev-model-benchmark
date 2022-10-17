from gru_utility import *
from torch.utils.data import TensorDataset, DataLoader
import sys
import torch.optim as optim
import pickle

dataset = sys.argv[1]
train_start = pd.to_datetime(sys.argv[2])
train_end = pd.to_datetime(sys.argv[3])
test_start = pd.to_datetime(sys.argv[4])
test_end = pd.to_datetime(sys.argv[5])
number = sys.argv[6]

# print(dataset)
# print(train_start)
# print(train_end)
# print(test_start)
# print(test_end)
# print(number)

# Import data
df = pd.read_csv('2. R/1. Outputs/'+dataset+'/daily_sessions.csv')
# print(df.head())
df['Start_date'] = pd.to_datetime(df['Start_date'])

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

#### Getting train and test sets
# X_train, X_val, X_test, y_train, y_val, y_test = train_val_test_split(df_features, 'sessions', 0.2)

df_train = df[(df.index>=train_start) & (df.index<=train_end)]
df_test = df[(df.index>=test_start) & (df.index<=test_end)]

X_train = df_train.drop("sessions",axis=1)
X_test = df_test.drop("sessions",axis=1)
y_train = df_train[["sessions"]]#.values.reshape(-1,1)
y_test = df_test[["sessions"]]#.values.reshape(-1,1)

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
with open('3. Python/1. Outputs/' + dataset + '/best_config_gru.pkl', 'rb') as handle:
    best_config = pickle.load(handle)

batch_size = best_config['batch_size']

train_features = torch.Tensor(X_train_arr)
train_targets = torch.Tensor(y_train_arr)
# val_features = torch.Tensor(X_val_arr)
# val_targets = torch.Tensor(y_val_arr)
test_features = torch.Tensor(X_test_arr)
test_targets = torch.Tensor(y_test_arr)

train = TensorDataset(train_features, train_targets)
# val = TensorDataset(val_features, val_targets)
test = TensorDataset(test_features, test_targets)

train_loader = DataLoader(train, batch_size=batch_size, shuffle=False, drop_last=True)
train_loader_one = DataLoader(train, batch_size=1, shuffle=False, drop_last=True)
# val_loader = DataLoader(val, batch_size=batch_size, shuffle=False, drop_last=True)
test_loader = DataLoader(test, batch_size=batch_size, shuffle=False, drop_last=True)
test_loader_one = DataLoader(test, batch_size=1, shuffle=False, drop_last=True)

#### Training

input_dim = len(X_train.columns)
output_dim = 1
hidden_dim = best_config['hidden_dim'] #64
layer_dim = best_config['layer_dim'] #3
# batch_size = 64
dropout = 0.1
n_epochs = best_config['n_epochs'] #500
learning_rate = best_config['learning_rate'] #1e-3
weight_decay = 0

model_params = {'input_dim': input_dim,
                'hidden_dim' : hidden_dim,
                'layer_dim' : layer_dim,
                'output_dim' : output_dim,
                'dropout_prob' : dropout}

model = get_model('gru', model_params)

loss_fn = nn.MSELoss(reduction="mean")
optimizer = optim.Adam(model.parameters(), lr=learning_rate, weight_decay=weight_decay)

opt = Optimization(model=model, loss_fn=loss_fn, optimizer=optimizer)
opt.train(train_loader,
          train_loader,
          # val_loader,
          batch_size=batch_size, n_epochs=n_epochs, n_features=input_dim)
# opt.plot_losses()

train_predictions, train_values = opt.evaluate(train_loader_one, batch_size=1, n_features=input_dim)
test_predictions, test_values = opt.evaluate(test_loader_one, batch_size=1, n_features=input_dim)

train_result = format_predictions(train_predictions, train_values, X_train, scaler)
test_result = format_predictions(test_predictions, test_values, X_test, scaler)

# print(calculate_metrics(opt))
df_train = df_train.reset_index()[["Start_date","sessions"]]
df_test = df_test.reset_index()[["Start_date","sessions"]]

df_train["pred"] = train_result.prediction.values
df_test["pred"] = test_result.prediction.values

# print(train)
# print(test)

df_train.to_csv('2. R/1. Outputs/'+dataset+'/sarima/gru_sessions_train_'+number+'.csv')
df_test.to_csv('2. R/1. Outputs/'+dataset+'/sarima/gru_sessions_test_'+number+'.csv')