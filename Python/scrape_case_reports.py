import bs4
import html5lib
import requests
import time




payload = {'_username':'nayers', 
          '_password':'nayers1'
         }

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
