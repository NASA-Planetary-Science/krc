# Makefile for IDL externals for KRC users
#_Hist 2014feb28 HK Derive from Hugh's idl/externals/Makefile
# Tried  -m32 on CFLAGS FFLAGS and LDFAGS; this caused errors
# 2014may05 -lg2c >> -lgfortran
#########################################################################

# Set up some shell-level specific variables
SHELL=/bin/csh 

#  These are the GNU C compiler flags.
CC= gcc -pipe  
CFLAGS= -fPIC -Wall
#  These are the FORTRAN compiler flags
FC=gfortran
#FC=g77
FFLAGS= -fno-automatic -fno-second-underscore -fargument-alias -fd-lines-as-comments -fPIC 

#  Special load flags, utilized in all builds, whether FORTRAN or C
LD=gcc
LDFLAGS= -shared -fPIC --whole-archive   # -Wall -Wl
# whole archive only needed if refer to .a libs

# Special library MACROS for .a libraries  There are none built here
# AR=ar   # archinve.. moves .o into .a    No harm 
# ARFLAGS=-rvs
# RANLIB=/bin/echo  # nedded for .a lib

#------------- Libraries and paths-------------------------
# Include files paths
INCLUDES=-I.   # in this directory

# L are Library directores that always are searched in
# l are libraries to include  -lc==libc.a  etc. 
LIBDIRS=-L.  # -L/home/hkieffer/linux/lib  #<<< last is for NumRec
#SYSLIBS = -lg2c -lc -lm   commented 2014may05
SYSLIBS = -lgfortran -lc -lm  

#  Set up how to process each source file type. 
.SUFFIXES:           # clean out any prior
.SUFFIXES: .o .c  .f
.c.o: 
	$(CC) $(CFLAGS) $(INCLUDES) -c $<
.f.o: 
	$(FC) $(FFLAGS) $(INCLUDES) -c $<

#------------------- target dependencies -------------------

# Binding routines in this directory.   Then change w.c to w.o
OBJSW= ccor2dw.o con4w.o deding2w.o dpephemw.o elevatew.o eqoftimew.o errdelw.o ertiaw.o  fl1w.o miedpw.o modinitw.o mseasw.o planckw.o  rndexw.o tifovw.o vlpresw.o

# Source routines   copy OBJSW  Change w.o to .o
OBJS= ccor2d.o con4.o deding2.o dpephem.o elevate.o eqoftime.o errdel.o ertia.o fl1.o miedp.o modinit.o mseas.o planck.o rndex.o tifov.o vlpres.o  

# Routines not in reference libraries that are called only from Fortran
OBJI = averag.o const.o fsizer2.o interp.o split.o state.o dspline.o dsplint.o

OBJSALL=  $(OBJI) $(OBJSW) $(OBJS) # concatonate objects

#------------- Actions -----------------------------

# machines running in 64-bit mode, IDL must be in same mode
ftnwrap64.so:	$(OBJSALL)    
	$(LD) $(LDFLAGS)   -o $@ $(OBJSALL) $(LIBDIRS) \
	$(SYSLIBS)
#	 -lhk_fNumRec $(SYSLIBS)

clean:
	-unalias rm; rm -f *.o ftnwrap64.so

#----------- routine dependancies --------------

dpephem.o: stacom.inc ephcom.inc

mooneph.o: stacom.inc ephcom.inc

dpephem.o: stacom.inc ephcom.inc

dp_eph.o: stacom.inc ephcom.inc

state.o: stacom.inc ephcom.inc

tifov.o: tifov.f modcom.inc errcom.inc

ertia.o: ertia.f modcom.inc errcom.inc

modinit.o: modinit.f modcom.inc

elevate.o: elevate.f errcom.inc

errdel.o: errdel.f errcom.inc