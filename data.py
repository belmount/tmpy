import pandas as pd
import numpy as np
import re
from  os import listdir, path

def get_sym_name(fn):
    with open(fn) as f:
        first_line = f.readline()
        name, code = first_line.split()[:2]
        
    return '{1}-{0}'.format(name, code)


class DataLoader:
    '''
        param: data files location
        data files is tab split text file exported from tongdaxin day line
    '''
    def __init__(self, data_path):
        self._data_path = path.abspath(data_path)
        self._datas = {}

    @property
    def datas(self):
        return self._datas

    '''
        load data into pandas dataframe
    '''
    def load(self, filecount=5):
        fc = 0
        for fn in listdir(self._data_path):
            fc +=1
            
            fpath = '{0}/{1}'.format(self._data_path, fn)
        
            symbol = get_sym_name(fpath)
            data = pd.read_csv(fpath, skiprows=1, encoding='GBK', index_col=0, parse_dates=True,sep='\t')
            data = data.iloc[:-1, :5]
            data.columns = ['open', 'high', 'low', 'close', 'volume']
            data.index.names = ['datetime']
            data.index = pd.to_datetime(data.index)
            self._datas[symbol] = data

            if fc == filecount: break