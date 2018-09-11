from bs4 import BeautifulSoup # load library
import sys

# to make this file executable from path, it would need to determine the path where it was invoked (where the input/output files reside). This would help:
# import os
# path = os.getcwd() # Path at terminal when executing this file

filename = sys.argv[1]          # passes the argument given when invoking this code in console
newfilename = filename + ".txt" # name for the file that will be created

file = open(filename, "r")             # open's file for reading
soup = BeautifulSoup(file)             # passes its contents to beautiful soup
file.close()                           # shuts the file
text = soup.get_text().encode("utf-8") # gets text only without html markups

f = open(newfilename, "w")      # creates a new file to dump the text
f.write( text )                 # puts text from beautiful soup in it
f.close()                       # saves the file

