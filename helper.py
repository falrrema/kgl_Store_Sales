# Funciones claves
import importlib
import subprocess
import pandas as pd
import numpy as np

def load_install_package(packages):
    for package in packages:
        try:
            importlib.import_module(package)
            print(f"{package} está instalada y lista para usar.")
        except ImportError:
            print(f"{package} no está instalada. Instalando...")
            subprocess.check_call(['pip', 'install', package])
            print(f"{package} ha sido instalada exitosamente.")

# Clase para hacer CV en bloques (creado con chatGPT)
class BlockingTimeSeriesSplit():
    def __init__(self, train_size=None, test_size=None, n_splits=None, step=None):
        self.train_size = train_size
        self.test_size = test_size
        self.n_splits = n_splits
        self.step = step if step else test_size  # Default step size is test_size

    def get_n_splits(self, X, y=None, groups=None):
        n_samples = len(X)
        
        # If train_size, test_size, and step are provided
        if self.train_size and self.test_size:
            max_splits = 1 + (n_samples - self.train_size - self.test_size) // self.step
            if self.n_splits:
                if self.n_splits > max_splits:
                    raise ValueError(f"Cannot have n_splits > {max_splits} with the provided train and test sizes and step. Consider reducing n_splits.")
                return self.n_splits
            return max_splits
        
        # If only n_splits is provided
        if self.n_splits:
            return self.n_splits
        raise ValueError("Either train/test sizes or n_splits should be provided.")
    
    def split(self, X, y=None, groups=None):
        n_samples = len(X)
        indices = np.arange(n_samples)
        
        n_splits = self.get_n_splits(X)
        
        if self.train_size and self.test_size:
            for i in range(n_splits):
                start = i * self.step
                mid = start + self.train_size
                stop = mid + self.test_size
                if stop > n_samples:
                    stop = n_samples
                yield indices[start: mid], indices[mid: stop]
        else:
            k_fold_size = n_samples // n_splits
            for i in range(n_splits):
                start = i * k_fold_size
                stop = start + k_fold_size
                mid = int(0.5 * (stop - start)) + start
                yield indices[start: mid], indices[mid: stop]

# Función para hacer CV generando una lista de DF con splits de train y test
def sliding_period(df, btss, period):
    import pandas as pd
    # Step 1: Check Period Index
    if period == "day":
        if not isinstance(df.index, pd.PeriodIndex) or df.index.freqstr != 'D':
            raise ValueError("Index should be of type PeriodIndex with 'D' frequency for 'day' period.")
    elif period == "week":
        if not isinstance(df.index, pd.PeriodIndex) or df.index.freqstr != 'W-SUN':
            raise ValueError("Index should be of type PeriodIndex with 'W-SUN' frequency for 'week' period.")
    elif period == "month":
        if not isinstance(df.index, pd.PeriodIndex) or df.index.freqstr != 'M':
            raise ValueError("Index should be of type PeriodIndex with 'M' frequency for 'month' period.")
    elif period == "quarter":
        if not isinstance(df.index, pd.PeriodIndex) or df.index.freqstr != 'Q-DEC':
            raise ValueError("Index should be of type PeriodIndex with 'Q-DEC' frequency for 'quarter' period.")
    elif period == "year":
        if not isinstance(df.index, pd.PeriodIndex) or df.index.freqstr != 'A-DEC':
            raise ValueError("Index should be of type PeriodIndex with 'A-DEC' frequency for 'year' period.")
    # Add more checks for other periods if needed

    # Step 2: Extract Unique Periods
    unique_periods = df.index.unique()

    results = []

    # Step 3 & 4: Apply `BlockingTimeSeriesSplit` and Filter & Annotate DataFrame
    for train_periods, test_periods in btss.split(unique_periods):
        train_df = df[df.index.isin(unique_periods[train_periods])].copy()
        train_df['split'] = 'train'
        
        test_df = df[df.index.isin(unique_periods[test_periods])].copy()
        test_df['split'] = 'test'
        
        combined_df = pd.concat([train_df, test_df])
        results.append(combined_df)

    # Step 5: Return List
    return results
  
# Funcion de chequeo de BlockingTimeSeriesSplit para experimentar splits
def check_BlockTimeSeriesSplit(df, train_size=None, test_size=None, n_splits=None, step=None):
    # Extract unique periods
    unique_periods = df.index.unique()
    
    # Calculate max_splits for train_size and test_size
    max_splits = 1 + (len(unique_periods) - train_size - test_size) // (step or test_size)
    
    # Validate and compute missing parameters
    if n_splits:
        if train_size is None and test_size is None:
            split_size = len(unique_periods) // n_splits
            train_size = split_size - (split_size // 3)
            test_size = split_size // 3
        elif train_size and test_size:
            if n_splits > max_splits:
                raise ValueError(f"Cannot have n_splits > {max_splits}. Adjust train/test sizes or step.")
        else:
            raise ValueError("If n_splits is provided along with train_size or test_size, both train_size and test_size must be provided.")
    elif train_size and test_size:
        n_splits = max_splits
    else:
        raise ValueError("Provide either n_splits or both train_size and test_size.")
    
    # Create an instance of BlockingTimeSeriesSplit
    btss = BlockingTimeSeriesSplit(train_size=train_size, test_size=test_size, n_splits=n_splits, step=step or test_size)
    
    # Collect split details
    results = []
    fold_number = 1
    last_test_period = None
    for train_periods, test_periods in btss.split(unique_periods):
        results.append([fold_number, 'Train', len(train_periods), unique_periods[train_periods][0], unique_periods[train_periods][-1]])
        results.append([fold_number, 'Test', len(test_periods), unique_periods[test_periods][0], unique_periods[test_periods][-1]])
        last_test_period = unique_periods[test_periods][-1]
        fold_number += 1
    
    # Convert results to DataFrame
    splits_df = pd.DataFrame(results, columns=['Fold', 'Split', 'Length', 'Initial_period', 'Ending_period'])
    
    # Calculate unused periods
    unused_periods = len(unique_periods[unique_periods.tolist().index(last_test_period)+1:])
    
    # Print report
    print(f"Dataframe Initial Period: {unique_periods[0]}")
    print(f"Dataframe Max Period: {unique_periods[-1]}")
    print(f"Maximum possible splits: {max_splits}")
    if n_splits:
        print(f"n_splits: {n_splits}")
    print(f"Number of unused periods: {unused_periods}")
    
    return splits_df

# Funcion de medicion
def RMSLE(y_true: list, y_pred: list) -> float:
    """
    The Root Mean Squared Log Error (RMSLE) metric using only NumPy
    
    :param y_true: The ground truth labels given in the dataset
    :param y_pred: Our predictions
    :return: The RMSLE score
    """
    n = len(y_true)
    msle = np.sqrt(np.mean(np.square(np.log1p(y_pred) - np.log1p(y_true))))
    return msle