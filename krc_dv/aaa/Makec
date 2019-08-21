##########################################################################
#_Title Makec -    Linux  x86_64 OS make KRC C routine library
#_Args  NONE
#_Descr Defines dependancies for GNU compilers using  make
#_Hist  Feb 08 2005 Kris Becker, USGS, Flagstaff Original Version
# 2009may17 Hugh Kieffer Modify for the C routines needed by KRC
# 2011oct24 HK Modify for distribution
# 2014may05 -lg2c >> -lgfortran
#_End
###########################################################################

# Set up some shell-level specific variables
SHELL=/bin/csh
RM=/bin/rm -f
HHK_DBG=-g -C

HOST_ARCH ?= $(shell uname -s)
HOST_MACH  = $(shell uname -m)

#  Temporary defines for building in foreign systems
HHKOS      ?= $(HOST_MACH)
HHKINC     ?= $(CURDIR)

#  GNU C compiler and flags.
CC=gcc -pipe -O2  -Wall -fPIC -Dunix -D$(HOST_ARCH) -D$(HOST_MACH) -D$(HHKOS)

#  Special load flags.  These flags are utilized in all builds, whether
#  they are FORTRAN or C
LDFLAGS=-shared

# Special library MACROS
AR=ar           # handles archives
ARFLAGS=-rvs    # r=replace v=verbose s=include index
#? RANLIB=/bin/echo
#? RXROOT=/usr/X11   #??? where is this
#? RMOTIFROOT=/usr/local  #  not used at all?

# set LIBRARY to be name of library
#LIBRARY  ?= libhk_cisis
#LIBNAME   = $(LIBRARY).a
LIBNAME=./libhk_cisis.a
# SYSLIBS=-lncurse -ltermcap  -lg2c -lc -lm
SYSLIBS=-lncurse -ltermcap  -lgfortran -lc -lm

#  Set up source dependancies
SRCS = $(wildcard *.c)
OBJS = $(addsuffix .o, $(basename $(SRCS)))

#  Define all required targets
.PHONY : all install clean $(LIBNAME)

all: $(LIBNAME)

install: $(LIBNAME)
	$(CP) $@ $(SYSLIB)

$(LIBNAME): $(OBJS)
	$(AR) $(ARFLAGS) $(LIBNAME) $(OBJS)

#  Clean up 
clean: 
	$(RM) $(OBJS) $(LIBNAME)