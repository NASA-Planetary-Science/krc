# Makefile for KRC distribution  2012nov22

# Set up some shell-level specific variables
SHELL=/bin/csh

FC=gfortran  # NEWER    
#FC=g77 -pipe   # OLDER then get unrecognized command line option "-fd-lines-as-comments"   -xld  set to compile D lines ??

# -fd-lines-as-comment is listed as gfortran option: http://gcc.gnu.org/onlinedocs/gfortran/Option-Summary.html#Option-Summary  

FFLAGS= -O  -fno-automatic -fno-second-underscore -fargument-alias -fd-lines-as-comments
# -fbounds-check
# -Wall
LD=gfortran

SYSLIBS =  -lg2c -lc -lm

#  library groups
HHKLIB=/work1/build/  # top of libraries to use
LIBDIRS=-L$(HHKLIB)

CLIB=./cfiles/libhk_cisis.a

# system dependencies
#OBJECTS1 = krc.o tseas.o tlats.o tday.o tcard.o tprint.o tdisk.o  albvar.o alsubs.o aveday.o aveyear.o binf5.o catime.o climtau.o co2pt.o datime.o deding2.o eccanom.o epred.o evmono3.o finterp.o idarch.o orbit.o porb0.o porb.o readtxt360.o rndex.o rotmsp.o rotv.o rotvec.o rterp.o seasalb.o seastau.o sigma.o tint.o vaddsp.o vcros.f vfdot.o vfmag.o vlpres.o vrotv.o white1.o xtreme.o 

OBJECTSM = mkrc.o  mlats.o tday.o tcard.o tprint.o tdisk.o epred.o \
           porb0.o tint.o albvar.o co2pt.o vlpres.o aveday.o porb.o orbit.o \
           eccanom.o alsubs.o deding2.o seasalb.o seastau.o readtxt360.o \
           finterp.o evmono3.o idarch.o r2i2s.o white0.o readaie.o \
	  binf5.o catime.o datime.o rndex.o rotmsp.o rterp.o sigma.o \
	vaddsp.o white1.o xtreme.o

#OBJECTSP = porbmn.o porbio.o porb.o porb1.o porbqq.o orbit.o eccanom.o \
#          obliq.o oblip.o ephemr.o spcrev.o caldate.o porbel.o ymd2jd.o \
#	 ang360.o b2b.o caldat.o datime.o julday.o mprint.o mprod3.o rotax.o rotdia.o rotvec.o
   
OBJ6 = krc.o tseas.o tlats.o tday.o tcard.o tprint.o tdisk.o epred.o porb0.o \
 tint.o albvar.o co2pt.o vlpres.o aveday.o porbit.o orbit.o eccanom.o  \
 deding2.o idarch.o seasalb.o seastau.o sigma.o readtxt360.o finterp.o evmono3.o aveyear.o \
 climtau.o catime.o datime.o binf5.o bigend.o white1.o rotmsp.o cocosp.o vaddsp.o

OBJP = porbmn.o porbio.o ephemr.o ymd2j2.o porbig.o porbit.o porbel.o orbit.o spcrev.o caldate.o caldat.o julday.o b2b.o upcase.o eccanom.o datime.o prtpcom.o rotmsp.o cocosp.o vaddsp.o

# normal link
krc: $(OBJ6)     # -xl required for record length to be in words
	$(FC) $(LDFLAGS) $(LIBDIRS) -o $@ $(OBJ6) \
	$(HLIB) $(CLIB) $(SYSLIBS)


# map movie version
mkrc: $(OBJECTSM)     # -xl required for record length to be in words
	$(FC) $(LDFLAGS) $(LIBDIRS) -o $@ $(OBJECTSM) \
	$(CLIB) $(SYSLIBS)

porbmn: $(OBJP)
	$(FC) $(LDFLAGS) $(LIBDIRS) -o $@ $(OBJP) \
	$(SYSLIBS)

# routines with dependencies
krc.o:   krc.f krccom.inc latcom.inc daycom.inc units.inc filcom.inc  
mkrc.o: mkrc.f krccom.inc latcom.inc daycom.inc units.inc filcom.inc 

tseas.o: tseas.f krccom.inc latcom.inc hatcom.inc units.inc 
mlats.o: mlats.f krccom.inc latcom.inc daycom.inc hatcom.inc units.inc
tlats.o: tlats.f krccom.inc latcom.inc daycom.inc hatcom.inc units.inc porbcx.inc
tday.o: tday.f krccom.inc daycom.inc hatcom.inc units.inc
tyearp.o: tyearp.f krccom.inc latcom.inc hatcom.inc
seasalb.o: seasalb.f units.inc filcom.inc
seastau.o: seastau.f units.inc filcom.inc
tcard.o: tcard.f krccom.inc latcom.inc daycom.inc units.inc 
tprint.o: tprint.f krccom.inc latcom.inc daycom.inc units.inc filcom.inc 
tdisk.o: tdisk.f krccom.inc latcom.inc daycom.inc hatcom.inc units.inc filcom.inc
porb1.o: porb1.f units.inc porbcm.inc

ephemr.o: ephemr.f   porbcm.inc
porb.o:   porb.f     porbcm.inc
porb1.o:  porb1.f    porbcm.inc
porbio.o: porbio.f   porbcm.inc
porbmn.o: porbmn.f   porbcm.inc
porbqq.o: porbqq.f   porbcm.inc
porbig.o: porbig.f   porbcm.inc
porbit.o: porbit.f   porbcm.inc



# no additional dependancies
albvar.o: albvar.f
alsubs.o: alsubs.f
ang360.o: ang360.f 
aveday.o: aveday.f 
aveyear.o: aveyear.f
b2b.o: b2b.f 
bigend.o: bigend.f
binf5.o: binf5.F
caldat.o: caldat.f
caldate.o: caldate.f
catime.o: catime.f
climtau.o: climtau.f
cocosp.o: cocosp.f # coordinate conversion package
co2pt.o: co2pt.f
datime.o: datime.f
deding2.o: deding2.f
eccanom.o: eccanom.f
evmono3.o: evmono3.f
finterp.o: finterp.f
idarch.o: idarch.F
julday.o:  julday.f  
koft.o: koft.f
orbit.o: orbit.f
porb.o:  porb.f
porb0.o: porb0.f
porbel.o: porbel.f 
prtpcom.o: prtpcom.f
oblip.o:  oblip.f  
obliq.o:  obliq.f  
orbit.o:  orbit.f
r2i2s.o: r2i2s.f
readaie.o: readaie.f
readtxt360.o: readtxt360.f
rndex.o: rndex.f 
rotmsp.o: rotmsp.f   # many routines
rterp.o: rterp.f
sigma.o: sigma.f  
spcrev.o: spcrev.f
tint.o:  tint.f
tyearp.o: tyearp.f
upcase.o: upcase.f
vaddsp.o: vaddsp.f   # many routines
vlpres.o: vlpres.f
white0.o: white0.f 
white1.o: white1.f
xtreme.o: xtreme.f
ymd2jd.o: ymd2jd.f

# epred.o: epred.f 
# idarch.o: idarch.F
#mprint.o: mprint.f 
#mprod3.o: mprod3.f
# rotv.o 

clean:  
	rm -f krc *.o
