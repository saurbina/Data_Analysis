import requests
from bs4 import BeautifulSoup
import re
import urllib.request
import time
import os

os.chdir("/Users/saurbina/Documents/pdf-txt")

url = 'https://www.bcn.cl/historiapolitica/corporaciones/cuentas_publicas/detalle?tipo=presidentes'
response = requests.get(url)

soup = BeautifulSoup(response.text, 'html.parser')  

pdf_urls = []

for link in soup.findAll('a'):
    href = link.get('href')
    if href and href.endswith('.pdf') and 'recursoslegales' in href and 'docreader' not in href:
        pdf_urls.append(href)

print(pdf_urls)


line_count = 1
download_directory = '/Users/saurbina/Documents/pdf-txt/'  # Replace this with your desired directory

for link in pdf_urls: 
    if line_count <= 10 : 
        try:
            download_url = link 
            file_name = link.split('/')[-1]  
            urllib.request.urlretrieve(download_url, download_directory + file_name) 
            print(f"Downloaded {file_name}")
            time.sleep(15)  # Pause the code for 15 seconds
        except Exception as e:
            print(f"Failed to download {file_name}: {e}")
    else:
        break  # Exit the loop once 10 files have been downloaded
    # Increment line_count for next iteration
    line_count += 1



