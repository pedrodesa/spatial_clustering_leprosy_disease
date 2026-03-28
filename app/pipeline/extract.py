import json
import urllib.request
import urllib.error
import os


class SinanDownloader:
    def __init__(self, agravo):
        self.agravo = agravo

    def extract_sinan(self, tipo, local, ano):
        """
        Extrair tabelas de dados do SINAN via FTP do site do DATASUS.
        """
        # URL de extração dos dados do DATASUS
        url = f'ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/{tipo}/{self.agravo}{local}{ano}.dbc'

        # Repositório de armazenamento dos dados no projeto 'data/'
        local_path = f'./data/raw/{self.agravo}{local}{ano}.dbc'

        os.makedirs(os.path.dirname(local_path), exist_ok=True)

        try:
            urllib.request.urlretrieve(url, local_path)
            print(f'Arquivo {local_path} baixado com sucesso')

        except Exception as erro:
            print(f'Erro ao baixar arquivo {local_path}: {erro}')

    def get_data(self, tipos, local, anos):
        """
        Iterar o download de arquivos
        """
        for ano in anos:
            for tipo in tipos:
                self.extract_sinan(tipo, local, ano)


def load_config(filename):
    """
    Carregar configurações do arquivo JSON.
    """
    with open(filename) as user_file:
        config = json.load(user_file)

    return config