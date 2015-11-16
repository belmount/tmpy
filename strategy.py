# Trading Strategy class
import numpy as np
import pandas as py

class Strategy():
    def __init__(self, data):
        self._data = data 
        self._indicators = pd.DataFrame(index = data.index)
    
    def add_indicators(self, **args):
        ''' Generate indicators for processing
        args['short_ma'] means short time window for mean average adj close 
        args['long_ma'] means long time window for mean average adj close
        also generate a atr for trailing stop   
        '''
        stand_factor = self.data['Adj Close'] / self.data['Close']
        self._indicators['short_sma'] = ma(self._data['Adj Close'], args['short_ma'])
        self._indicators['long_sma'] = ma(self._data['Adj Close'], args['long_ma'])
        self._indicators['atr'] = pd.rolling_mean(tr(self._data), args['short_ma']) * stand_factor
    
    @staticmethod
    def ma(data, time_window):
        return pd.rolling_mean(data, time_window)

    @staticmethod
    def tr(data):
        ''' generate true range technical signal'''
        prev_data = data.shift(1).Close
        hh = pd.DataFrame({'a': prev_data, 'b':data.High).max(axis =1 )
        ll = pd.DataFrame({'a': prev_data, 'b':data.Low).min(axis =1 )
        return (hh - ll)
        
    def can_entry(self, index):
        bar = selft._data[index]
        indicator = self._indicators[index]
        return bar['Adj Close'] > indicator['short_sma'] and indicator['short_sma']>  indicator['long_sma']