def df_wine(split=True):
	import pandas as pd
	from sklearn.model_selection import train_test_split

	df = pd.read_csv('https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data', header=None)
	df.columns = ['Class label', 'Alcohol', 'Malic acid', 'Ash',
				  'Alcalinity of ash', 'Magnesium', 'Total phenols',
				  'Flavanoids', 'Nonflavanoid phenols',
				  'Proanthocyanins', 'Color intensity', 'Hue',
				  'OD280/OD315 of diluted wines', 'Proline']

	X, y = df.iloc[:, 1:], df.iloc[:, 0]

	if split == True:
		X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, stratify=y)
		return X_train, X_test, y_train, y_test
	else:
		return X, y


def df_boston(split=True):
	import pandas as pd
	from sklearn.model_selection import train_test_split

	df = pd.read_csv('/Users/jihun/Documents/data_science/datasets/Fastcampus/Boston_house.csv')

	X, y = df.drop(['Target'], axis=1), df['Target']

	if split == True:
		X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3)
		return X_train, X_test, y_train, y_test
	else:
		return X, y

def df_iris():
	import pandas as pd
	from sklearn.datasets import load_iris
	X = pd.DataFrame(data = load_iris().data, columns=load_iris().feature_names)
	y = pd.DataFrame(load_iris().target, columns=['target'])

	return X, y

def df_titanic():
	import pandas as pd
	titanic_df = pd.read_csv('/Users/jihun/Documents/data_science/datasets/titanic/train.csv')

	return titanic_df
