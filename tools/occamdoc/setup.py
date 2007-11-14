from distutils.core import setup
import py2exe

setup(
	console = ["occamdoc.in"],
	data_files = ["to-html.xsl", "frames.html", "occamdoc.css"]
	)

