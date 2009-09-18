#!/usr/bin/python

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

import MySQLdb
import sys, cgi, cgitb, urllib

cgitb.enable()
form = cgi.FieldStorage()
host = form.getfirst('host', '')
name = form.getfirst('name', '')
dpi = int(form.getfirst('dpi', 100))
width = int(form.getfirst('width', 8))
height = int(form.getfirst('height', 4))
history = int(form.getfirst('history', 0))
simple = form.getfirst('simple', False)

db = MySQLdb.connect(user = 'occbench', passwd = 'p3rf0rmANCE', db = 'occbench')
c = db.cursor()

if not host:
	c.execute("SELECT DISTINCT host FROM result ORDER BY host ASC")
	hosts = c.fetchall()
	print '<html>'
	print '<p>Select Host</p>'
	for (host,) in hosts:
		print '<a href="graph.py?' + urllib.urlencode({'host' : host}) + '">', host, '</a><br/>'
	print '</html>'
elif not name:
	c.execute("SELECT DISTINCT name FROM result WHERE host = %s ORDER BY host ASC", (host, ))
	names = c.fetchall()
	print '<html>'
	print '<p>Select Benchmark</p>'
	for (name,) in names:
		print '<a href="graph.py?' + urllib.urlencode({'host' : host, 'name' : name}) + '">', name, '</a><br/>'
	print '</html>'
else:
	c.execute("SELECT rev, type, cpus, avg FROM result WHERE host = %s AND name = %s ORDER BY cpus ASC", (host, name))
	
	bench_type = ""
	results = {}
	for (rev, type, cpus, avg) in c.fetchall():
		bench_type = type
		if not results.has_key(rev):
			results[rev] = { 'values' : {} }
		data_set = results[rev]
		if not data_set['values'].has_key(cpus):
			data_set['values'][cpus] = []
		data_set['values'][cpus].append(avg)

	
	for rev, data_set in results.items():
		data_set['x'] = x_values = []
		data_set['y'] = y_values = []
		for x in sorted(data_set['values'].iterkeys()):
			ys = data_set['values'][x]
			x_values.append(x)
			y_values.append(sum(ys) / len(ys))

	if bench_type == "scaling":
		aggregate = { 'scaling' : [] }
		revs = sorted(results.iterkeys())
		rev_labels = [ "r" + str(r) for r in revs ]
		rev_base = []
		rev_scaling = []
		rev_worst_y0 = 0.0

		# Calculate scaling value, optimal y values and worst y0 for revs
		for rev in revs:
			data_set = results[rev]
			data_set['opt_y'] = opt_y = []
			y0 = data_set['y'][0]
			scaling = 0.0
			scaling_n = 0
			
			if y0 > rev_worst_y0:
				rev_worst_y0 = y0
			
			for (cpus, y) in zip(data_set['x'], data_set['y']):
				opt_y.append(y0 / cpus)
				if cpus > 1:
					scaling += ((y0 / y) - 1.0) / (cpus - 1.0)
					scaling_n += 1
			
			if scaling_n > 0:
				scaling /= scaling_n
			
			rev_scaling.append(scaling)

		# Generate baseline (normalised y0) data
		for rev in revs:
			data_set = results[rev]
			rev_base.append(data_set['y'][0] / rev_worst_y0)

		font_def = matplotlib.font_manager.FontProperties()
		font_small = font_def.copy()
		font_small.set_size('small')

		fig = plt.figure()
		fig.set_dpi(dpi)
		fig.set_size_inches(width, height)

		# Plot revision time line
		ax = fig.add_subplot(history + 1, 1, 1)
		ax.plot(revs, rev_scaling, 'o-')
		ax.plot(revs, rev_base, 'o-')
		ax.legend(('scaling', 'baseline'), 'upper right')
		ax.set_title(name)
		ax.set_xticks(revs)
		ax.set_xticklabels(rev_labels)
		ax.set_ybound(lower=0.0, upper=2.0)

		# Plot full revision history
		cnt = 0
		for rev in sorted(results.iterkeys(), reverse=True)[0:history]:
			data_set = results[rev]
			cnt += 1

			ax = fig.add_subplot(history + 1, 1, cnt + 1)
			ax.plot(data_set['x'], data_set['y'])
			ax.plot(data_set['x'], data_set['opt_y'])
			ax.legend((host + ' r' + str(rev), 'ideal'), 'upper right', prop=font_small)
			
			if cnt == history:
				ax.set_xlabel('CPUs')
			ax.set_xscale('log', basex=2)
			ax.set_xticks(data_set['x'])
			ax.set_xticklabels(data_set['x'])
			
			ax.set_ylabel('Time (s)')
			ax.set_ybound(lower=0.0)
		
		fig.savefig(sys.stdout)
	
