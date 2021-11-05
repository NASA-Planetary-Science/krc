# Makefile for KRC double precision version
# Attempt to have minimum differences between versions used for development and
# for formal distributions
# 2014mar11  Hugh Kieffer Derive from single precision version
# 2014jun19  HK Incorporate Version 3 PORB
# 2016feb13:mar23  HK  Version 33 commons
# 2016may12  HK  Incorporate V3.4 routines and testing programs, update common 
#      names. Move notes on unused options to  krc/aaa/makeNotes
# 2016may26 Kenny Rios method to handle development vrs production environments 
# 2016jun02 Comment out Rios distribution items
# 2016aug20 Commented options for debugger
# 2017feb23 HK Clarify differences between H3 and Distribution builds 
# lines containing  #<>H should be active if and only if on Hughs computer
# lines containing  #<>D should be active if and only if building a Distribution
# 2017mar15 HK include eclipse routines
# 2017apr29 HK Add krcdb as target with for debug options
# 2017sep30 HK Include  nowhite.f  to clean-up errorlog filename
# 2018feb01 HK include orlint.f
# 2018oct14 HK Include wraper.f
# 2019dec04 HK Change from wraper.f to  wraper8.f
####################################################

SHELL=/bin/csh

FC=gfortran

# Use 2nd version below to allow debugger and enable most IDBG actions
FFLAGS= -fno-automatic -fno-second-underscore -fd-lines-as-comments -Wall
#FFLAGS= -fno-automatic -fno-second-underscore -fd-lines-as-code  -fbounds-check # -Wall   #  -O

# next line for fortrancallgraph-master
#FFLAGS= -fno-automatic -fno-second-underscore -fd-lines-as-comments -Wall -S -g -O0

LD=gfortran
#------------- Libraries -------------------------
# Pointers to top of libraries to use. 
HHKLIB=/home/hkieffer/linux/lib  # available only non H3
KRCLIB=.                         #<>D  Only for distribution

#SYSLIBS = -L /usr/lib64  -lg2c -lgfortran -lc -lm    ## ? lgfortran 
 SYSLIBS = -L -lgfortran -lc -lm 

# LDFLAGS= -fdump-tree-slim  # used, but does nothing 2014mar11
LDFLAGS= -g -fbounds-check  # prepare for debugger. Used only by krcdb

# Library directories that always are searched
#LIBDIRS=-L$(HHKLIB)                     #<>H
 LIBDIRS_DEV=-L$(KRCLIB) -L$(HHKLIB)   #<>D
 LIBDIRS_PROD=-L$(KRCLIB)              #<>D

#  Special Kieffer library groups
#HLIB=-lhk_fmath -lhk_fgeom -lhk_futil -lhk_fchar  ##2-lhk_fNumRec # -lhk_rad

# CLIB=/home/hkieffer/linux/lib/libhk_cisis.a    #<>H
CLIB=./cfiles/libhk_cisis.a    # -lhk_crest    #<>D

#------------- system dependencies -------------

OBJ8 = krc8.o tseas8.o tlats8.o tday8.o tcard8.o tprint8.o tdisk8.o tun8.o \
 epred8.o tint8.o albvar8.o vlpres.o porb08.o porbit.o orbit8.o eccanom8.o \
 alsubs.o deding28.o seasalb.o seastau.o readtxt360.o finterp.o evmono38.o \
 climtau.o binf5.o bigend.o rotmdp8.o vadddp8.o cocodp8.o readzone.o \
 catime.o white1.o ksubs8.o tfar8.o cubuterp8.o sigma8.o fillmv.o \
 eclipse.o tfine8.o dspline.o dsplint.o evmono3d.o strumi.o strumr8.o gaspt8.o \
 orlint8.o wraper8.o
# replace  nowhite  with code in krc

# PORB double precision
OBJP3 = porbmn.o porbio.o ephemr.o ymd2j2.o porbig.o porbit.o porbel.o \
 orbit8.o spcrev.o caldate.o caldat.o julday.o b2b.o upcase.o eccanom8.o \
 catime.o prtpcom.o rotmdp8.o cocodp8.o vadddp8.o

# testing
OBJT = testrou.o alsubs.o averag.o aveyear.o bigend.o binf5.o \
 catime.o climtau.o cubuterp8.o datime.o dpythag.o dspline.o dsplint.o \
 dsvbksb.o dsvdcmp.o eclipse.o evmono38.o evmono3d.o evrf4.o \
 fillmv.o getpi4.o getpr8.o hratlsq.o kratlsq.o m2eul.o qtlats.o \
 randomn.o ratval.o rotmdp8.o sigma.o sigma8.o spline.o splint.o \
 strumi.o strumr8.o tdif3.o tfar8.o epred8.o \
 vadddp8.o white1.o gaspt8.o nowhite.o orlint8.o wraper8.o

OBBB = krt8.o fillmv.o 


# global maps
OBJECTG = glot.o binf5.o  st2real6.o readkrcm1.o averag.o \
 catime.o white0.o fillmv.o minvr4.o bigend.o white1.o

# r2r.o d2d.o d2md.o 

OBJT1 = test8.o deding2.o deding28.o

# normal link

krt: $(OBBB) 
	$(FC) $(LIBDIRS_PROD) -o $@ $(OBBB) \
	$(CLIB) $(SYSLIBS)

krc: $(OBJ8) 
	$(FC) $(LIBDIRS_PROD) -o $@ $(OBJ8) \
	$(CLIB) $(SYSLIBS)

porbmn: $(OBJP3)
	$(FC)  $(LIBDIRS_PROD) -o $@ $(OBJP3) \
	$(SYSLIBS)

glot: $(OBJECTG) 
	$(FC) $(LIBDIRS_PROD) -o $@ $(OBJECTG) \
	$(CLIB) $(SYSLIBS)

# testing and development

krcdb: $(OBJ8)  # -  with debug
	$(FC) $(LDFLAGS) $(LIBDIRS_PROD) -o $@ $(OBJ8) \
	$(CLIB) $(SYSLIBS)

tporbmn: $(OBJP3)
	$(FC) $(LDFLAGS) $(LIBDIRS_DEV) -o $@ $(OBJP3) \
	$(SYSLIBS)

testr: $(OBJT)   # - 
	$(FC) $(LIBDIRS_DEV) -o $@ $(OBJT) \
	$(CLIB) $(SYSLIBS)

testd: $(OBJT)   # with debug
	$(FC) $(LDFLAGS) $(LIBDIRS_DEV) -o $@ $(OBJT) \
	$(CLIB) $(SYSLIBS)

test1: $(OBJT1)   # -
	$(FC) $(LDFLAGS) $(LIBDIRS_DEV) -o $@ $(OBJT1) \
	$(CLIB) $(SYSLIBS)

# make routines for program dependencies 
#
krt8.o:       krt8.f krcc8m.f latc8m.f dayc8m.f unic8m.f filc8m.f hatc8m.f
krc8.o:       krc8.f krcc8m.f latc8m.f dayc8m.f unic8m.f filc8m.f hatc8m.f
tseas8.o:   tseas8.f krcc8m.f latc8m.f          unic8m.f          hatc8m.f porbc8m.f
tlats8.o:   tlats8.f krcc8m.f latc8m.f dayc8m.f unic8m.f filc8m.f hatc8m.f porbc8m.f
tday8.o:     tday8.f krcc8m.f          dayc8m.f unic8m.f filc8m.f hatc8m.f porbc8m.f
tfine8.o:   tfine8.f krcc8m.f          dayc8m.f unic8m.f          hatc8m.f
tcard8.o:   tcard8.f krcc8m.f latc8m.f dayc8m.f unic8m.f filc8m.f hatc8m.f
tprint8.o: tprint8.f krcc8m.f latc8m.f dayc8m.f unic8m.f filc8m.f 
tdisk8.o:   tdisk8.f krcc8m.f latc8m.f dayc8m.f unic8m.f filc8m.f hatc8m.f
tfar8.o:     tfar8.f krcc8m.f                   unic8m.f filc8m.f 
# above containss TFAREAD
tun8.o:       tun8.f krcc8m.f          dayc8m.f                   hatc8m.f
readzone.o: readzone.f krcc8m.f                 unic8m.f filc8m.f
tdif3.o:     tdif3.f krcc8m.f                   unic8m.f filc8m.f
seasalb.o: seasalb.f                            unic8m.f filc8m.f
seastau.o: seastau.f                            unic8m.f filc8m.f
climtau.o: climtau.f                            unic8m.f filc8m.f
albvar8.o: albvar8.f krcc8m.f
gaspt8.o:   gaspt8.f krcc8m.f
tint8.o:     tint8.f krcc8m.f 
ephemr.o:   ephemr.f           porbc8m.f
porb08.o:   porb08.f           porbc8m.f        unic8m.f
porbig.o:   porbig.f           porbc8m.f
porbio.o:   porbio.f           porbc8m.f
porbit.o:   porbit.f           porbc8m.f  # this is DP   porbit4 is SP
prtpcom.o: prtpcom.f           porbc8m.f
testrou.o: testrou.f krcc8m.f                   unic8m.f filc8m.f    # test main program
wraper8.o: wraper8.f                            unic8m.f  # only if D lines
glot.o: glot.f glotcom.f
readkrcm1.o: readkrcm1.f glotcom.f 
#------------------  do not have includes
alsubs.o: alsubs.f 
averag.o: averag.f  # test for function should be defined and called
aveyear.o: aveyear.f
bigend.o: bigend.f
binf5.o: binf5.F  # uses  B2B  BIGEND  CATIME WHITE1  PIO_system
cocodp8.o: cocodp8.f  # Contains: COCOCM  COCOMC  COCOSC  COCOCS  
#                       COCOSM  COCEMC  COCECM 
cubuterp8.o: cubuterp8.f
deding28.o: deding28.f
eccanom8.o: eccanom8.f
eclipse.o: eclipse.f 
epred8.o: epred8.f
evmono38.o: evmono38.f
evmono3d.o: evmono3d.f
finterp.o: finterp.f
getpi4.o: getpi4.f
getpr8.o: getpr8.f
jdate.o:  jdate.f  
julday.o:  julday.f 
minvr4.o: minvr4.f
orbit8.o: orbit8.f
porbel.o: porbel.f 
#porb1.o: porb1.f unic8m.f porbcom8.f
readaie.o: readaie.f
readtxt360.o: readtxt360.f
randomn.o: randomn.f # used to test sigma8 
#r2i2s.o: r2i2s.f
rotmdp8.o: rotmdp8.f  # Contains:  MEQUAL  MPROD3  ROTAX  ROTCOL  ROTDIA 
#                      ROTEST  ROTEXM  ROTEXV  ROTMAT  ROTORB  ROTRIP  
#                      ROTROW  ROTSHO  ROTV  ROTVEC  ROTZXM  TRANS3  VROTV
sigma.o: sigma.f  
sigma8.o: sigma8.f   # used for debug of cubuterp
spcrev.o: spcrev.f 
st2real6.o: st2real6.f
strumi.o: strumi.f
strumr8.o: strumr8.f
tridag8.o: tridah8.f   # uses prior partial solution
vlpres.o: vlpres.f
vadddp8.o: vadddp8.f  # Contains:  VADD  VCROSS  VDOT  VEQUAL  VMAG  VNEG 
 #                       VNORM  VPRF  VPRINT  VSCALE  VSHOW  VSUB  VUNIT
ymd2j2.o: ymd2j2.f

#----------------------- added after remove use of all but C library
b2b.o: b2b.f
caldat.o: caldat.f
caldate.o: caldate.f
catime.o: catime.f
datime.o: datime.f
d2d.o: d2d.f
dmd.o: dmd.f
d2md.o: d2md.f
fillmv.o: fillmv.f  # has  FILLB FILLI FILLL FILLR FILLD MVB MVI MVL MVR MVD
#                     MVDF MVDM MVD21
ksubs8.o: ksubs8.f  # has  AVEDAY  AVEYEAR  CO2PT  SIGMA 
nowhite.o: nowhite.f
otlint8.o: orlint8.f
r2r.o: r2r.f
upcase.o: upcase.f
# white0.o: white0.f
white1.o: white1.f
vec2code.o: vec2code.f

# ------------------used only for testing
bigend1.o: bigend1.f
catime:o /home/hkieffer/src/for/util/catime.f
climtau.o: climtau.f
deding2.o: deding2.f
dpythag.o: dpythag.f # - /home/hkieffer/src/for/NumRec/dpythag.f
dspline.o: dspline.f # -
dsplint.o: dsplint.f # -
dsvbksb.o: dsvbksb.f # -
dsvdcmp.o: dsvdcmp.f # calls dpythag
evrf4.o: evrf4.f
hratlsq.o: hratlsq.f # calls ratval,dsvbksb,dsvdcmp spline splint
kratlsq.o: kratlsq.f # calls ratval,dsvbksb,dsvdcmp dspline dsplint
m2eul.o: m2eul.f
qtlats.o: qtlats.f
ratval.o: ratval.f
spline.o: spline.f # -
splint.o: splint.f # -
test8.o: test8.f

#
# Make clean
#
clean:
	-unalias rm; rm -f *.o 
realclean:
	-rm krc
	-rm porb
	-rm -f *.o 

# -rm -f krc8.o  tseas8.o tlats8.o tday8.o  tcard8.o tprint8.o tdisk8.o tun8.o readzone.o porb08.o tint8.o 
