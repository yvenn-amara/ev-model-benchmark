# Import libraries
import requests
from bs4 import BeautifulSoup
import pandas as pd

url = 'https://www.worldfootball.net/teams/as-monaco/10/'
page = requests.get(url)

soup = BeautifulSoup(page.text, 'lxml')
table= soup.find('table')q