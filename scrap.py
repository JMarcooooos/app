# Bibliotecas ----
from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.options import Options
from dotenv import load_dotenv
import os
import time

# Configurações ----
caminho_da_pasta = r"Z:\SUPGEAR\GEARE\PAINEL ESCOLAS\app"

opcoes = webdriver.ChromeOptions()
preferencias = {
    "download.default_directory": caminho_da_pasta,
    "download.prompt_for_download": False,
}
opcoes.add_experimental_option("prefs", preferencias)

# Iniciar ----
navegador = webdriver.Chrome(options=opcoes)
espera = WebDriverWait(navegador, 20)
url_do_site = "https://goias360.educacao.go.gov.br/Login.html"
navegador.get(url_do_site)

load_dotenv()
meu_usuario = os.getenv("BOT_USER")
minha_senha = os.getenv("BOT_PASS")

# Login ----
try:
    campo_usuario = espera.until(
        EC.presence_of_element_located((By.XPATH, "//*[@id='usuario']"))
    )
    campo_usuario.send_keys(meu_usuario)

    campo_senha = navegador.find_element(By.XPATH, "//*[@id='senha']")
    campo_senha.send_keys(minha_senha)

    botao_entrar = navegador.find_element(
        By.XPATH, "//*[@id='login']/div[1]/div[3]/div/div[2]/div/div[3]/div/input"
    )
    botao_entrar.click()

except Exception as e:
    print(f"Ocorreu um erro durante o login: {e}")


# Base de Escolas ----
try:
    botao_educacao = espera.until(
        EC.element_to_be_clickable((By.XPATH, "//*[@id='index']/div[1]/div[1]/a"))
    )
    botao_educacao.click()

    botao_escolas = espera.until(
        EC.element_to_be_clickable(
            (By.XPATH, "//*[@id='homeEducacao']/div[1]/div[1]/a")
        )
    )
    botao_escolas.click()

    botao_basededados = espera.until(
        EC.element_to_be_clickable(
            (By.XPATH, "//*[@id='escola']/div[1]/div[1]/div/div/div/div")
        )
    )
    botao_basededados.click()

    nome_do_arquivo = "Escola Estrutura.xlsx"

    caminho_arquivo_antigo = os.path.join(caminho_da_pasta, nome_do_arquivo)

    if os.path.exists(caminho_arquivo_antigo):
        os.remove(caminho_arquivo_antigo)

    botao_download = espera.until(
        EC.element_to_be_clickable(
            (By.XPATH, "//*[@id='gridEstrutura']/div[1]/div[1]/div[1]/button[2]")
        )
    )

    botao_download.click()

except Exception as e:
    print(f"Erro na etapa de Base de Escolas: {e}")

time.sleep(5)


# Detalhes Escolas Quantitativos ----

navegador.get("https://goias360.educacao.go.gov.br/HomeEducacao.html")
time.sleep(2)
try:
    botao_estrategia = espera.until(
        EC.element_to_be_clickable(
            (By.XPATH, "//*[@id='homeEducacao']/div[1]/div[4]/a")
        )
    )
    botao_estrategia.click()

    botao_quantitativoescola = espera.until(
        EC.presence_of_element_located(
            (By.XPATH, "//a[contains(@href, 'DetalhesEscolaVisao.html')]")
        )
    )

    navegador.execute_script("arguments[0].click();", botao_quantitativoescola)

    nome_do_arquivo = "Detalhes Escolas Quantitativos.xlsx"

    caminho_arquivo_antigo = os.path.join(caminho_da_pasta, nome_do_arquivo)

    if os.path.exists(caminho_arquivo_antigo):
        os.remove(caminho_arquivo_antigo)

    botao_download = espera.until(
        EC.element_to_be_clickable(
            (
                By.XPATH,
                "//*[@id='detalhesEscolaVisao']/div[2]/div/div[1]/div[1]/div[1]/button[2]",
            )
        )
    )
    botao_download.click()

except Exception as e:
    print(f"Erro na etapa de Detalhes Escolas Quantitativos: {e}")

time.sleep(5)

# Detalhes Conselhos Escolares ----
navegador.get("https://goias360.educacao.go.gov.br/HomeEducacao.html")
time.sleep(2)
try:
    botao_estrategia = espera.until(
        EC.element_to_be_clickable(
            (By.XPATH, "//*[@id='homeEducacao']/div[1]/div[4]/a")
        )
    )
    botao_estrategia.click()

    botao_conselho = espera.until(
        EC.presence_of_element_located(
            (By.XPATH, "//a[contains(@href, 'DetalhesEscolaConselho.html')]")
        )
    )

    navegador.execute_script("arguments[0].click();", botao_conselho)

    nome_do_arquivo = "Detalhes Conselhos Escolares.xlsx"

    caminho_arquivo_antigo = os.path.join(caminho_da_pasta, nome_do_arquivo)

    if os.path.exists(caminho_arquivo_antigo):
        os.remove(caminho_arquivo_antigo)

    botao_download = espera.until(
        EC.element_to_be_clickable(
            (
                By.XPATH,
                "//*[@id='detalhesEscolaConselho']/div/div[2]/div/div[1]/div[1]/div[1]/button[2]",
            )
        )
    )
    botao_download.click()

except Exception as e:
    print(f"Erro na etapa de Detalhamento Conselho Escolares: {e}")

time.sleep(5)

# Quantitativo Alunos Por Turma ----

navegador.get("https://goias360.educacao.go.gov.br/HomeEducacao.html")
time.sleep(2)
try:
    botao_estrategia = espera.until(
        EC.element_to_be_clickable(
            (By.XPATH, "//*[@id='homeEducacao']/div[1]/div[4]/a")
        )
    )
    botao_estrategia.click()

    botao_quantitativo = espera.until(
        EC.presence_of_element_located(
            (By.XPATH, "//a[contains(@href, 'QuantitativoAlunoPorTurma.html')]")
        )
    )

    navegador.execute_script("arguments[0].click();", botao_quantitativo)

    nome_do_arquivo = "Quantitativo Alunos Por Turma.xlsx"

    caminho_arquivo_antigo = os.path.join(caminho_da_pasta, nome_do_arquivo)

    if os.path.exists(caminho_arquivo_antigo):
        os.remove(caminho_arquivo_antigo)

    botao_download = espera.until(
        EC.element_to_be_clickable(
            (
                By.XPATH,
                "//*[@id='QuadroQuantitativoAlunoPorTurma']/div/div[2]/div/div[1]/div[1]/div[1]/button[2]",
            )
        )
    )
    botao_download.click()

except Exception as e:
    print(f"Erro na etapa de Quantitativo Alunos Por Turma: {e}")

time.sleep(15)

# Fechar Navegador ----
time.sleep(10)
navegador.quit()
