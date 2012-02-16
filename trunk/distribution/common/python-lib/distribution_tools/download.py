import urllib2
import os

def download(dest, url):
    if os.path.exists(dest):
        print 'downloading "%s" (already downloaded)' % (dest, )
    else:
        print 'downloading "%s" from: %s' % (dest, url)
        f = open(dest, 'wb')
        f.write(urllib2.urlopen(url).read())
        f.close() 

