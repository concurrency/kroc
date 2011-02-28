from util import *

# DO THE PARSE
parser = OptionParser()
parser.add_option('-p', '--path', dest='path',
									help='Path to checkout.')

(options, args) = parser.parse_args()

def checkout(path):
	header("RUNNING CHECKOUT")
	base_url = config.get('SVN', 'base')
	checkout_url = "%s/%s" % (base_url, path)
	header(checkout_url)
	remove_and_mkdir(config.get('BUILD', 'build'))
	mkdir(config.get('BUILD', 'svn'))

	print checkout_url
	cmd(build_command(["svn", "co", 
											checkout_url, 
											config.get('BUILD', 'svn')]))

def main ():
	for key, val in props(options).iteritems():
		if (key == 'path'):
			checkout(val)	
			sys.exit(0)
	checkout('trunk')

main()
