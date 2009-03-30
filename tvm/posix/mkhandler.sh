#!/bin/sh

# FIXME: Should the functionaloty of this file be transported into the Makefile?
if [ "$2" = "OCCAMPI" ]
then
  ../../skroc/skroc --no-std-libs --occ21-opts '-DEF OCCAMPI' --c -f $1handler_tmp.h $1handler.occ
else
  ../../skroc/skroc --no-std-libs --c -f $1handler_tmp.h $1handler.occ
fi
if [ $? -ne 0 ] ; then
	echo "Failed to build the $1 handler ($1handler.occ)"
	exit 1
fi

# FIXME: Is it still neccesary to do the "s/, *,/,/" substitution???
cat $1handler_tmp.h | sed -e "\
	s/memory_size/$1h_memsize/ ;\
	s/ws_size/$1h_ws_size/ ;\
	s/vs_size/$1h_vs_size/ ;\
	s/ms_size/$1h_ms_size/ ;\
	s/inst_size/$1h_instsize/ ;\
	s/transputercode/$1h_transputercode/;\
	s/, *,/,/" > $1handler.h
