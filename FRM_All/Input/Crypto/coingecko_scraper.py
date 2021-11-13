# -*- coding: utf-8 -*-
"""
Created on Fri Jan  8 18:48:17 2021

@author: Vanessa Guarino
"""

from pycoingecko import CoinGeckoAPI
import pandas as pd
import time
from datetime import datetime
from functools import reduce

data_type = {'prices':'prices', 
             'market caps': 'market_caps', 
             'total volumes': 'total_volumes'}

from_time = 1604188800
to_time = 1609372800 

class APIwrapper: 
    
    def __init__(self):
        self.socket = CoinGeckoAPI()
        
    def mktcap_ord_api(self, *args): 
        return self.socket.get_coins_markets(*args)
                
    def coin_api(self, tries, delay, backoff, *args): 
        for n in range(tries):
            try: 
                return self.socket.get_coin_market_chart_range_by_id(*args)
            
            except (KeyboardInterrupt, SystemExit):
                raise
                
            except Exception as e:
                time.sleep(delay)
                delay *= backoff
                print(f"{e} occurred. New attempt in {delay} seconds")
                continue 
            
        raise NameError(f"Failed within {tries} attempts")


def get_coins_name_id(dataframe_coin):
    coin_ids = dataframe_coin['id']
    coin_names = dataframe_coin['name']
    coin_symbols = dataframe_coin['symbol']
    
    return coin_ids, coin_names, coin_symbols

def create_dataframe(dataframe, coin_id, symbol, timeseries): 
    dataframe = pd.DataFrame(dataframe)
    dataframe.columns = [symbol]
    dataframe["Datetime"] = timeseries
    dataframe["Datetime"] = pd.to_datetime(dataframe["Datetime"])
    dataframe = dataframe.set_index(["Datetime"])
    dataframe = dataframe.median(level=0)
    dataframe = dataframe.applymap('{:.2f}'.format)
    
    return dataframe

def coin_scraper(coin, coin_id, symbol, minimum_date, maximum_date, data_type):
    dataframe = []
    timeseries = []
    n_var = [len(val) for key, val in coin.items()][0]
     
    for i in range(n_var):
        try: 
            variables = coin[data_type][i][1]
            date = [datetime.fromtimestamp(item[i][0]/1000).strftime("%Y-%m-%d") for item in coin.values()][0]
            dataframe.append(variables)
            timeseries.append(date)
            minimum_date.append(min(timeseries))
            maximum_date.append(max(timeseries))
        
        except (KeyboardInterrupt, SystemExit):
                raise
        
        except Exception as e:
            time.sleep(10)
            print(f"Error: {e}, occurred for {coin_id}. New attempt in 5 seconds.")
            continue 
    
    dataframe = create_dataframe(dataframe, coin_id, symbol, timeseries)
    
    return minimum_date, maximum_date, timeseries, dataframe

def get_df_by_coin_time_range_to_csv(coin_ids, symbols, data_type = 'prices', n_start = 0, n_end = 10, 
                                     from_time = '1604188800', to_time = '1609372800'):
    frames = []
    minim_date = []
    maxim_date = []
    id_coin = []
    
    for idx, coin in enumerate(coin_ids[n_start : n_end]):
        api = APIwrapper()
        tmp = api.coin_api(10, 1, 2, coin,'usd', from_time, to_time)
            
        if [val for key, val in tmp.items()][0] == []:
            continue
        else: 
            id_coin.append(coin) 
        
        minim_date, maxim_date, t, dataframe = coin_scraper(tmp, coin, symbols[idx], minim_date, maxim_date, data_type) 
        print(f"{coin} correctly downloaded.")
        frames.append(dataframe)
        time.sleep(5)
        
    return frames, minim_date, maxim_date, id_coin


def main():
    
    api = APIwrapper()  
    top_15_coins = api.mktcap_ord_api('usd')[:100] #top 100
    ids = [dic['id'] for dic in top_15_coins]
    symbols = [dic['symbol'] for dic in top_15_coins]
    
    frames = []
    minim_date = []
    maxim_date = []
    id_coin = []
    
    frames, minim_date, maxim_date, id_coin = get_df_by_coin_time_range_to_csv(ids, symbols, data_type['market caps'], n_start = 0, 
                                                n_end = len(ids), from_time = str(from_time), to_time = str(to_time))
    
    if minim_date == []:
        print("Coins data are not available. Try with another time sequence.")
        
    else:
        index = [pd.date_range(start= min(minim_date), end=max(maxim_date)).tolist()]
        match_df = pd.DataFrame(index = index).rename_axis("Datetime")
        filter_type_frames = match_df.merge(reduce(lambda left,right: pd.merge(left, right, on = "Datetime", how="outer"), frames), 
                              how="outer", on ="Datetime")
        
        filter_type_frames.to_csv(datetime.fromtimestamp(from_time).strftime("%Y%m%d") + " " +
                                  datetime.fromtimestamp(to_time).strftime("%Y%m%d") + " " + data_type['market caps'] + " cryptos.csv")


if __name__== "__main__":
  main()
