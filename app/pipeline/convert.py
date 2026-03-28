import pandas as pd
from dbctodbf import DBCDecompress
from simpledbf import Dbf5


def converter_dbc_para_dbf(caminho_dbc, caminho_dbf, caminho_csv):
    """
    Descomprime arquivos DBC em DBF e converte para CSV.
    """
    try:
        dbc2dbf = DBCDecompress()
        dbc2dbf.decompressFile(caminho_dbc, caminho_dbf)
        dbf_file = Dbf5(caminho_dbf)
        df = dbf_file.to_dataframe()
        df.to_csv(caminho_csv, sep=';', index=False)
        print(f'Conversão concluída: {caminho_csv}')
    except Exception as e:
        print(f'Erro ao converter: {e}')

converter_dbc_para_dbf(
    './data/raw/HANSBR25.dbc', 
    './data/interim/result.dbf', 
    './data/processed/result_final.csv'
    )