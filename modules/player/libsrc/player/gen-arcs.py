from math import cos, sin, pi

num_points = 181
radius = 40
displacement_x = 0.0
displacement_y = 8.0

rows = []
for d in range(radius):
	cols = []
	for i in range(num_points):
		angle = (i * pi) / num_points
		cols.append("[%.8f,%.8f]"
		            % (d * cos(angle) + displacement_x,
		               d * sin(angle) + displacement_y))

	rows.append("[" + ",\n".join(cols) + "]")

print "VAL [][]POINT points IS [" + ",\n\n".join(rows) + "]:\n"
