"""
https://towardsdatascience.com/web-scraping-using-selenium-python-8a60f4cf40ab
https://selenium-python.readthedocs.io/locating-elements.html
https://www.geeksforgeeks.org/how-to-select-a-drop-down-menu-value-using-selenium-in-python/
"""


import bs4
import html5lib
import requests
import time
import selenium
from selenium import webdriver
import pandas as pd
from selenium.webdriver.edge.options import Options
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import Select
from selenium.webdriver.support import expected_conditions as EC

payload = {'_username':'nayers', 
          '_password':'nayers1'
         }


# may need to use selenium: switch to frame
# use selenium to get to login page
#https://cmx.lablynx.com/elab/elab/vistalynx/syslogin.asp?FormAction=NewUser
# try to enter bad username/pwd first to see if get something back, if it might work
# click on loginLnkImg
# selenium has a method to download, click and download files
driver = webdriver.Edge('C:/Users/natra/AppData/Local/MicrosoftEdge/msedgedriver.exe')
#driver.get('https://www.google.com/')
#driver.get("https://cmx.lablynx.com/elab")
driver.get('https://cmx.lablynx.com/elab/elab/vistalynx/syslogin.asp')
username = driver.find_element_by_name('loginid')
password = driver.find_element_by_name('password')
username.send_keys("nayers")
password.send_keys("nayers1")
driver.find_element_by_name('loginLnkImg').click()
driver.find_element_by_xpath('//a[@href="#!"]').click()

drop=Select(driver.find_element_by_name('cboLabNo'))
drop.select_by_value('-1')


driver.switch_to.frame(iframe)
driver.get('https://cmx.lablynx.com/elab/elab/vistalynx/syslogin.asp')


chrome_options = Options()
# chrome_options.add_argument("--headless")
chrome_options.binary_location ='C:/Program Files/Google/Chrome/Application/chrome.exe'
chrome_options.add_argument("--start-maximized") #open Browser in maximized mode
chrome_options.add_argument("--no-sandbox") #bypass OS security model
chrome_options.add_argument("--disable-dev-shm-usage") #overcome limited resource problems
driver = webdriver.Chrome('C:/Program Files/Google/chromedriver_win32/chromedriver.exe',options=chrome_options)

 
driver.get("https://cmx.lablynx.com/elab")

session = requests.Session()
s = session.post("https://cmx.lablynx.com/elab/", data=payload)
s = session.get('https://cmx.lablynx.com/elab/elab/vistalynx/sysTableEdit.Asp?Table=MEO_vuCASE&PrimaryKey=CASEID%3D319013', data=payload)

soup = bs4.BeautifulSoup(s.text, 'html5lib')
print(soup)
#s = session.get('https://cmx.lablynx.com/elab/vistalynx/sysTreeAnonymous.asp?TreeID=-2')
#s = session.post('https://cmx.lablynx.com/elab/elab/vistalynx/sysTableEdit.Asp?Table=MEO_vuCASE&PrimaryKey=CASEID%3D319013', data=payload)

# Main ME Cases page:
'https://cmx.lablynx.com/elab/elab/vistalynx/sysTableEdit.Asp?Table=MEO_vuCASE&PrimaryKey=CASEID%3D319013'

# Files attached to cases?
'https://cmx.lablynx.com/elab/elab/vistalynx/sysTableListFra.Asp?Table=MEO_VUREFINFOGROUPMEMBER&Frame=fraMEO_VUREFINFOGROUPMEMBER&REFINFOGROUPID=667078&NavPageSize=75&clFrameWidth=900'
