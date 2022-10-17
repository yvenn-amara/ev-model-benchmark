from sklearn import mixture
import numpy as np
# from sklearn.model_selection import KFold, train_test_split #StratifiedKFold
import matplotlib.pyplot as plt
import seaborn as sns
sns.set('paper', style="whitegrid", font_scale=2.5, rc={"lines.linewidth": 2.0}, )

from collections import Counter

def gmm_search_fit(X, path, n_comp=7, seed=123, test_split=0.1, verbose=False, desc=None, loglink=False, logeps=0.000000001):
    bic = []
    aic = []
    log_likelihood = []
    best_gmm = None
    lowest_bic = np.infty
    best_score = -np.infty
    N = X.shape[0]

    if loglink:
        X = np.log(X+logeps) #we add a neglectable constant in the transformation to avoid log(0)

    np.random.seed(seed)
    rnd_index=np.random.permutation(N)
    size_val = int((test_split * N))
    for n in range(2, n_comp+1):
        Xtrain = X[rnd_index[:(N-size_val)]]
        Xtest = X[rnd_index[(N-size_val):]]
        clf = mixture.GaussianMixture(n_components=n, covariance_type='full', tol=5*1e-4,
                                      max_iter=200, n_init=1, random_state=seed)

        clf.fit(Xtrain)
        bic.append(clf.bic(Xtrain))
        aic.append(clf.aic(Xtrain))
        log_likelihood.append(clf.score(Xtest))
        # print(f"score : {clf.score(Xtest)}")
        if bic[-1] < lowest_bic: #or log_likelihood[-1] > best_score:
            lowest_bic = bic[-1]
            best_score = log_likelihood[-1]
            best_gmm = clf
            print(f"lowest bic {lowest_bic}, best score {best_score}, best gmm components: {best_gmm.n_components}")

    if verbose:
        fig, ax = plt.subplots(figsize=(9, 6))
        ax.set_title(desc)
        sns.lineplot(x=range(2, n_comp+1), y=bic, ax=ax, label='bic')
        sns.lineplot(x=range(2, n_comp + 1), y=aic, ax=ax, label='aic')
        ax.set_xlabel("components")
        plt.tight_layout()
        plt.savefig(path+desc+".png")
        plt.close()
        plt.show()
    best_gmm.fit(X)
    return best_gmm


def gmm_fit(X, n_comp=12, seed=123, verbose=False, desc=None):
    """

    :param X (Dataframe or np.ndarray):
    :param n_comp (int):
    :param rnd_seed (int):
    :param verbose (bool):
    :param desc (str):

    :return (mixtureModel):
    """
    clf = mixture.GaussianMixture(n_components=n_comp, covariance_type='full', tol=5 * 1e-4,
                                  max_iter=200, n_init=1, random_state=seed)

    clf.fit(X)
    return clf


def baysian_gmm_fit(X, n_comp=1, seed=1, n_init=2):
    """

    :param X:
    :param n_comp:
    :param rnd_seed:
    :return:
    """
    clf = mixture.BayesianGaussianMixture(n_components=n_comp, covariance_type='full', tol=1e-4,
                                          max_iter=150, n_init=n_init, random_state=seed)

    clf.fit(X)
    best_gmm = clf
    return best_gmm

def bic(X, n_params, model):
    """Bayesian information criterion for the current model on the input X.
    Parameters
    ----------
    X : array of shape (n_samples, n_dimensions)
    n_params: a scalar
    model: gmm fitted model

    Returns
    -------
    bic : float
        The lower the better.
    """
    # return (-2 * self.score(X) * X.shape[0] +
    #         self._n_parameters() * np.log(X.shape[0]))
    return (-2 * model.score(X) * X.shape[0] +
            n_params * np.log(X.shape[0]))


def aic(X, n_params, model):
    """Akaike information criterion for the current model on the input X.
    Parameters
    ----------
    X : array of shape (n_samples, n_dimensions)
    n_params: a scalar
    model: gmm fitted model

    Returns
    -------
    aic : float
        The lower the better.
    """
    return -2 * model.score(X) * X.shape[0] + 2 * n_params


# create GMM-conditional instance
class NestedGMM(object):
    def __init__(self, base_gmm, sub_gmm_dict, seed=123):
        self.base_gmm = base_gmm
        self.seed = seed
        self.sub_gmm_dict = sub_gmm_dict

    def fit_base_gmm(self, X):
        self.base_gmm.fit(X)

    def sample_highlevel(self, n):
        s_vals, s_labels = self.base_gmm.sample(n)
        return s_vals, s_labels

    def cache(self):
        pass

    def sample(self, n):
        s_vals, s_labels = self.base_gmm.sample(n)
        label_cnt = Counter(s_labels)
        a_d_e_mat = np.array([]).reshape(0, 3)
        for k, v in label_cnt.items():
            e, e_labels = self.sub_gmm_dict[k].sample(v)
            e = np.abs(e)
            a_d_e = np.concatenate([s_vals[s_labels==k], e.reshape(-1,1)], axis=1)
            # a_d_e[:, 1] = a_d_e[:, 0] + a_d_e[:, 1]
            a_d_e_mat = np.concatenate([a_d_e_mat, a_d_e], axis=0)

        return a_d_e_mat, s_labels
