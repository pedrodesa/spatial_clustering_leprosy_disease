from pipeline.extract import SinanDownloader, load_config


# Download dos dados
def baixar_dados_sinan():
    # Carregar configurações do arquivo JSON
    config = load_config('config/settings.json')

    agravo = config['agravo']
    local = config['local']
    tipos = config['tipos']
    anos = config['anos']

    # Criando uma instância de SINANDownloader
    downloader = SinanDownloader(agravo)

    # Chamando o método get_data para baixar os dados
    downloader.get_data(tipos, local, anos)


baixar_dados_sinan()