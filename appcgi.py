#!/usr/bin/python3

import sys;
import cgi;
import cgitb;
import subprocess;
import os;

#cgitb.enable(); #Enable for debugging

print("Content-Type: text/html\n\n")

#os.chdir("/home/solarman")

post = cgi.FieldStorage()
query = post["query"].value;


#p = subprocess.check_output(["whoami"])

#print(p.decode("utf-8"))

os.environ["HOME"] = "/var/www"
#Was: XSaiga.Interactive
#proc = subprocess.Popen(["/opt/ghc8/bin/ghci", "src/XSaiga/Interactive.hs", "-e", ":set -XSafe", "-e", query], stderr=subprocess.PIPE, stdout=subprocess.PIPE)
proc = subprocess.Popen(["/opt/ghc8/bin/ghci", "util/Interactive.hs", "-package XSaiga", "-e", ":set -XSafe", "-e", query], stderr=subprocess.PIPE, stdout=subprocess.PIPE)

#sys.stdout.write("<pre>")
for lineu in proc.stdout:
  line=lineu.decode("utf-8")
  sys.stdout.write(line)
for lineu in proc.stderr:
  line=lineu.decode("utf-8")
  sys.stdout.write(line)
#sys.stdout.write("</pre>")

proc.wait()

#print(p.decode("utf-8"))
