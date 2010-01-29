#!/usr/bin/python
# Generate the same distribution charts as test-random using Python's
# random number generator. You can use this as a quick check that the
# distributions are approximately correct.

import sys, random

num_bins = 20
num_iterations = 100000

def show_function(f, label):
	w = sys.stdout.write

	w("%s:\n" % label)

	bins = [0] * num_bins
	outside = 0

	for i in range(num_iterations):
		n = f()
		bin = int(n)
		if bin < 0 or bin >= num_bins:
			outside += 1
		else:
			bins[bin] += 1

	scale = 60.0 / max(bins)
	for i in range(num_bins):
		w("|")
		for j in range(int(bins[i] * scale)):
			w("=")
		w(" %d\n" % bins[i])
	w("Outside: %d\n" % outside)

def random_int():
	return random.randint(0, num_bins - 1)
show_function(random_int, "Uniform ints")
def random_real32():
	return random.uniform(0.0, num_bins)
show_function(random_real32, "Uniform reals")
def random_gaussian_real32():
	return random.gauss(0.0, 1.0) + num_bins / 2
show_function(random_gaussian_real32, "Gaussian reals")
