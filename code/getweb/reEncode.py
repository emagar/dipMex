# attempt to read, encode to unicode, write the file... need to work with a loop
f = open('/home/eric/Dropbox/data/rollcall/dipMex/data/fromWeb/votes/62/tmp1.txt', 'r') # read file... check if using 'w' affects file after close
from bs4 import BeautifulSoup # load library
soup = BeautifulSoup(f)
f.close()

print(soup.get_text()) # how to write text only?


f.readline()
f.close()
