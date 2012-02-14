#!/usr/bin/python
# python script to generate 
# test files for work stealing 
# threaded scheduler

# Zachary Williams

import sys
from random import *


# Function for distributing all processes to processors
# 
# returns a list of length = number of processors where
# each element is the number of processes at indexs processor
def distribute(num, proc):
	avg = num/proc
	rem = num % proc
	
	# create a list of length proc
	# w/ init value avg
	dist = []
	for i in range(proc):
		dist.append(avg)

	# distribute remainng processes
	for i in range(rem):
		dist[i]=dist[i]+1	

	# randomly redistribute 5 * proc processes
	for i in range(5):	
		for j in range(proc):
			if random() > .5:
				dist[j] = dist[j]+1
				if j != (proc-1):
					dist[j+1] = dist[j+1]-1
				else:
					dist[j-1] = dist[j-1]-1

	return dist
		
# Function to distribute processes to batches
# 
# returns a list with each element (representing a 
# processor) is a list where each element is the 
# number of processes in the batch ided by index 

def batch (dist):
	
	batchlist = []

	# create list of batches with one process in each
	# number of batches is random and between 1 and half
	# the number of processes assigned to a processor
	#
	# each batch starts with one process in it
	for nr_proc in dist:
		batchlist.append([1 for i in range(randint(1, nr_proc/2))])


	# distribute remaining processes into batches
	count = 0
	for batch in batchlist:
		for j in range(dist[count]-len(batch)):
			batch[randint(0, len(batch) - 1)] += 1
		count+=1

	return batchlist


###################################
#
#  Main code to generate c test
#
####################################

if(len(sys.argv) == 5):
	processor = int(float(sys.argv[1])) # number of processors
	numP = int(float(sys.argv[2])) # number of processes
	dis = int(float(sys.argv[3]))	  # dispatch count
	winS = int(float(sys.argv[4]))    # window size	
else:
	print "Input Error"
	sys.exit()


# set up initial comments
filename="SchedTest_%d_%d_%d_%d.c" % (processor, numP, dis, winS)
filelist = []
filelist.append("/* Automatically generated test with\n")
filelist.append(" * \n")
filelist.append(" * %d PROCESSORS (must match constant in wf-sched.h)\n" % processor)
filelist.append(" * %d = dispatch count (must match constant in wf-sched.h)\n" % dis)
filelist.append(" * %d Processes\n" % numP)
filelist.append(" * %d = window size\n" % winS)
filelist.append(" * \n")
filelist.append(" */\n\n")


filelist.append("#include <stdio.h>\n#include <stdlib.h>\n#include <wf-sched.h>\n")

filelist.append("int global_procs[NUMBER_OF_PROCS];\n")
filelist.append("logical_processor_t global_proc_pointer[NUMBER_OF_PROCS];\n\n")

filelist.append("void main()\n{\n")
#populate the test

filelist.append("\tpthread_t ")
for i in range(processor):
	if (i+1) != processor:
		filelist.append("t%d, " % (i+1))
	else:
		filelist.append("t%d;\n" % (i+1))

# create procesessors
filelist.append("\n")
for i in range(processor):
	filelist.append(
		"\tlogical_processor_t pr%d = malloc(sizeof(struct logical_processor));\n" % (i+1))

# assign processor ids
filelist.append("\n")
for i in range(processor):
	filelist.append("\tpr%d->id = %d;\n" % ((i+1), (i)))

# link processors
filelist.append("\n")
for i in range(processor):
	if (i+1) != processor:
		filelist.append("\tpr%d->partner = %d;\n" % ((i+1), (i+1)))
	else:
		filelist.append("\tpr%d->partner = %d;\n" % ((i+1), 0))


# set up global array (initialized in header)
filelist.append("\n")
for i in range(processor):
	filelist.append("\tglobal_proc_pointer[%d]=pr%d;\n" % (i, (i+1)))


filelist.append("\n")
# set up global processor list (tells which processors have work)
filelist.append("\tint i=0;\n\tfor(i=0;i<NUMBER_OF_PROCS;i++)\n\t\tglobal_procs[i]=1;\n")

# set up processes and batches for each processor

# "randomly" distribute processes into batches on processors
dist = batch(distribute(numP, processor))
print dist

# create c code based on structure on dist
filelist.append("\n")
filelist.append("\t/* CODE TO SET UP PROCESSES AND BATCHES */\n")
filelist.append("\n")
pcount=1 # keeps track of processor number
for proc in dist:
	bcount=1 # keeps track of batch number
	for batch in proc:
		filelist.append("\n")
		filelist.append("\tbatch_t b%d%d = malloc(sizeof(struct batch));\n" % (pcount, bcount))
		filelist.append("\tprocess_t p%d%d1 = malloc(sizeof(struct process));\n" % (pcount, bcount))
		filelist.append("\n")
		filelist.append("\tp%d%d1->id = %d%d1;\n" % (pcount, bcount, pcount, bcount))
		filelist.append("\tb%d%d->head = p%d%d1;\n" % (pcount, bcount, pcount, bcount))
		filelist.append("\tb%d%d->stolen = 0;\n" % (pcount, bcount))
		filelist.append("\tb%d%d->window = 0;\n" % (pcount, bcount))
		filelist.append("\n")

		# initialize all but first process in this batch
		for i in (range(1,batch)):
			filelist.append("\tprocess_t p%d%d%d = malloc(sizeof(struct process));\n" % (pcount, bcount, (i+1)))
			filelist.append("\tp%d%d%d->id = %d%d%d;\n" % (pcount, bcount,(i+1), pcount, bcount, (i+1)))
	
		# assign next pointers (process)
		filelist.append("\n")
		for i in (range(0,batch-1)):
			filelist.append("\tp%d%d%d->next = p%d%d%d;\n" % (pcount, bcount,(i+1), pcount, bcount, (i+2)))
		if batch>1:	
		  filelist.append("\tp%d%d%d->next = NULL;\n" % (pcount, bcount,(i+2)))
		else:
		  filelist.append("\tp%d%d%d->next = NULL;\n" % (pcount, bcount, 1))
			
		bcount+=1
	# END FOR batch in proc

	# set the first winS batches of every 
	# processor to be in the window
	batchC=1	
	windowC=winS
	filelist.append("\n")
	while(batchC <= len(proc)) and (windowC > 0):
		filelist.append("\tb%d%d->window = 1;\n" % (pcount, batchC))
		batchC+=1
		windowC-=1

	# assign next pointers (batches)
	# len(proc) = number of batches on proc
	# len(proc) = number of batches on proc
	filelist.append("\n")
	for i in (range(0,len(proc)-1)):
		filelist.append("\tb%d%d->next = b%d%d;\n" % (pcount, (i+1), pcount, (i+2)))
	if len(proc)>1:	
		filelist.append("\tb%d%d->next = NULL;\n" % (pcount, (i+2)))
	else:
		filelist.append("\tb%d%d->next = NULL;\n" % (pcount, 1))
		

	# assign head and tail for each processor  
	filelist.append("\tpr%d->head = b%d%d;\n" % (pcount, pcount, 1))
	if len(dist[pcount-1]) > 1:
		filelist.append("\tpr%d->tail = b%d%d;\n" % (pcount, pcount, (len(dist[pcount-1]))))
	else:
		filelist.append("\tpr%d->tail = b%d%d;\n" % (pcount, pcount, 1))

	pcount+=1


	
# initialize locks
filelist.append("\n")
for i in range(processor):
	 filelist.append("\tpthread_mutex_t m%d = PTHREAD_MUTEX_INITIALIZER;\n" % (i+1))	
	 filelist.append("\tpr%d->run_queue_lock = &m%d;\n" % ((i+1), (i+1)))

# run threads
filelist.append("\n")
for i in range(processor):
	filelist.append("\tpthread_create (&t%d, NULL, test_run, pr%d);\n" % ((i+1), (i+1)))


# join threads
filelist.append("\n")
for i in range(processor):
	filelist.append("\tpthread_join (t%d, NULL);\n" % (i+1))


filelist.append("\n}\n")
# write the file
FILE = open(filename, "w")
FILE.writelines(filelist) 
FILE.close

