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
# 2017sep30 HK Include  $(KRCLIB)/nowhite.f  to clean-up errorlog filename
# 2018feb01 HK include $(KRCLIB)/orlint.f
# 2018oct14 HK Include $(KRCLIB)/wraper.f
# 2019dec04 HK Change from $(KRCLIB)/wraper.f to  $(KRCLIB)/wraper8.f
####################################################

SHELL=/bin/csh
RM=/bin/rm -f

FC=gfortran

# Use 2nd version below to allow debugger and enable most IDBG actions
FFLAGS= -fno-automatic -fno-second-underscore -fd-lines-as-comments -fallow-argument-mismatch -Wall -cpp
#FFLAGS= -fno-automatic -fno-second-underscore -fd-lines-as-code  -fbounds-check # -Wall   #  -O

# next line for fortrancallgraph-master
#FFLAGS= -fno-automatic -fno-second-underscore -fd-lines-as-comments -Wall -S -g -O0

LD=gfortran
#------------- Libraries -------------------------
# Pointers to top of libraries to use. 
#<>D  Only for distribution
KRCLIB=./src/

#SYSLIBS = -L /usr/lib64  -lg2c -lgfortran -lc -lm    ## ? lgfortran 
 SYSLIBS = -L -lgfortran -lc -lm 

# LDFLAGS= -fdump-tree-slim  # used, but does nothing 2014mar11
LDBFLAGS= -g -fbounds-check  # prepare for debugger. Used only by krcdb

# Library directories that always are searched
LIBDIRS=-L$(KRCLIB)              #<>D

#  Special Kieffer library groups
#HLIB=-lhk_fmath -lhk_fgeom -lhk_futil -lhk_fchar  ##2-lhk_fNumRec # -lhk_rad

# CLIB=/home/hkieffer/linux/lib/libhk_cisis.a    #<>H
CSRC_DIR=$(KRCLIB)/cfiles/
# -lhk_crest    #<>D
CLIB=$(CSRC_DIR)/libhk_cisis.a

#------------- system dependencies -------------

OBJ8 = $(KRCLIB)/krc8.o $(KRCLIB)/tseas8.o $(KRCLIB)/tlats8.o $(KRCLIB)/tday8.o $(KRCLIB)/tcard8.o $(KRCLIB)/tprint8.o $(KRCLIB)/tdisk8.o $(KRCLIB)/tun8.o \
 $(KRCLIB)/epred8.o $(KRCLIB)/tint8.o $(KRCLIB)/albvar8.o $(KRCLIB)/vlpres.o $(KRCLIB)/porb08.o $(KRCLIB)/porbit.o $(KRCLIB)/orbit8.o $(KRCLIB)/eccanom8.o \
 $(KRCLIB)/alsubs.o $(KRCLIB)/deding28.o $(KRCLIB)/seasalb.o $(KRCLIB)/seastau.o $(KRCLIB)/readtxt360.o $(KRCLIB)/finterp.o $(KRCLIB)/evmono38.o \
 $(KRCLIB)/climtau.o $(KRCLIB)/binf5.o $(KRCLIB)/bigend.o $(KRCLIB)/rotmdp8.o $(KRCLIB)/vadddp8.o $(KRCLIB)/cocodp8.o $(KRCLIB)/readzone.o \
 $(KRCLIB)/catime.o $(KRCLIB)/white1.o $(KRCLIB)/ksubs8.o $(KRCLIB)/tfar8.o $(KRCLIB)/cubuterp8.o $(KRCLIB)/sigma8.o $(KRCLIB)/fillmv.o \
 $(KRCLIB)/eclipse.o $(KRCLIB)/tfine8.o $(KRCLIB)/dspline.o $(KRCLIB)/dsplint.o $(KRCLIB)/evmono3d.o $(KRCLIB)/strumi.o $(KRCLIB)/strumr8.o $(KRCLIB)/gaspt8.o \
 $(KRCLIB)/orlint8.o $(KRCLIB)/wraper8.o
# replace  nowhite  with code in krc

# PORB double precision
OBJP3 = $(KRCLIB)/porbmn.o $(KRCLIB)/porbio.o $(KRCLIB)/ephemr.o $(KRCLIB)/ymd2j2.o $(KRCLIB)/porbig.o $(KRCLIB)/porbit.o $(KRCLIB)/porbel.o \
 $(KRCLIB)/orbit8.o $(KRCLIB)/spcrev.o $(KRCLIB)/caldate.o $(KRCLIB)/caldat.o $(KRCLIB)/julday.o $(KRCLIB)/b2b.o $(KRCLIB)/upcase.o $(KRCLIB)/eccanom8.o \
 $(KRCLIB)/catime.o $(KRCLIB)/prtpcom.o $(KRCLIB)/rotmdp8.o $(KRCLIB)/cocodp8.o $(KRCLIB)/vadddp8.o


# normal link
krc: $(OBJ8) $(CLIB)
	$(FC) $(LIBDIRS_PROD) -o $@ $(OBJ8) \
	$(CLIB) $(SYSLIBS)

porbmn: $(OBJP3)
	$(FC)  $(LIBDIRS_PROD) -o $@ $(OBJP3) \
	$(SYSLIBS)

# testing and development

krcdb: $(OBJ8) $(CLIB)  # -  with debug
	$(FC) $(LDBFLAGS) $(LIBDIRS) -o $@ $(OBJ8) \
	$(CLIB) $(SYSLIBS)

porbmndb: $(OBJP3)
	$(FC) $(LDBFLAGS) $(LIBDIRS) -o $@ $(OBJP3) \
	$(SYSLIBS)

# make routines for program dependencies 
#
$(KRCLIB)/krt8.o:       $(KRCLIB)/krt8.f $(KRCLIB)/krcc8m.f $(KRCLIB)/latc8m.f $(KRCLIB)/dayc8m.f $(KRCLIB)/unic8m.f $(KRCLIB)/filc8m.f $(KRCLIB)/hatc8m.f
$(KRCLIB)/krc8.o:       $(KRCLIB)/krc8.f $(KRCLIB)/krcc8m.f $(KRCLIB)/latc8m.f $(KRCLIB)/dayc8m.f $(KRCLIB)/unic8m.f $(KRCLIB)/filc8m.f $(KRCLIB)/hatc8m.f
$(KRCLIB)/tseas8.o:   $(KRCLIB)/tseas8.f $(KRCLIB)/krcc8m.f $(KRCLIB)/latc8m.f          $(KRCLIB)/unic8m.f          $(KRCLIB)/hatc8m.f $(KRCLIB)/porbc8m.f
$(KRCLIB)/tlats8.o:   $(KRCLIB)/tlats8.f $(KRCLIB)/krcc8m.f $(KRCLIB)/latc8m.f $(KRCLIB)/dayc8m.f $(KRCLIB)/unic8m.f $(KRCLIB)/filc8m.f $(KRCLIB)/hatc8m.f $(KRCLIB)/porbc8m.f
$(KRCLIB)/tday8.o:     $(KRCLIB)/tday8.f $(KRCLIB)/krcc8m.f          $(KRCLIB)/dayc8m.f $(KRCLIB)/unic8m.f $(KRCLIB)/filc8m.f $(KRCLIB)/hatc8m.f $(KRCLIB)/porbc8m.f
$(KRCLIB)/tfine8.o:   $(KRCLIB)/tfine8.f $(KRCLIB)/krcc8m.f          $(KRCLIB)/dayc8m.f $(KRCLIB)/unic8m.f          $(KRCLIB)/hatc8m.f
$(KRCLIB)/tcard8.o:   $(KRCLIB)/tcard8.f $(KRCLIB)/krcc8m.f $(KRCLIB)/latc8m.f $(KRCLIB)/dayc8m.f $(KRCLIB)/unic8m.f $(KRCLIB)/filc8m.f $(KRCLIB)/hatc8m.f
$(KRCLIB)/tprint8.o: $(KRCLIB)/tprint8.f $(KRCLIB)/krcc8m.f $(KRCLIB)/latc8m.f $(KRCLIB)/dayc8m.f $(KRCLIB)/unic8m.f $(KRCLIB)/filc8m.f 
$(KRCLIB)/tdisk8.o:   $(KRCLIB)/tdisk8.f $(KRCLIB)/krcc8m.f $(KRCLIB)/latc8m.f $(KRCLIB)/dayc8m.f $(KRCLIB)/unic8m.f $(KRCLIB)/filc8m.f $(KRCLIB)/hatc8m.f
$(KRCLIB)/tfar8.o:     $(KRCLIB)/tfar8.f $(KRCLIB)/krcc8m.f                   $(KRCLIB)/unic8m.f $(KRCLIB)/filc8m.f 
# above containss TFAREAD
$(KRCLIB)/tun8.o:       $(KRCLIB)/tun8.f $(KRCLIB)/krcc8m.f          $(KRCLIB)/dayc8m.f                   $(KRCLIB)/hatc8m.f
$(KRCLIB)/readzone.o: $(KRCLIB)/readzone.f $(KRCLIB)/krcc8m.f                 $(KRCLIB)/unic8m.f $(KRCLIB)/filc8m.f
$(KRCLIB)/tdif3.o:     $(KRCLIB)/tdif3.f $(KRCLIB)/krcc8m.f                   $(KRCLIB)/unic8m.f $(KRCLIB)/filc8m.f
$(KRCLIB)/seasalb.o: $(KRCLIB)/seasalb.f                            $(KRCLIB)/unic8m.f $(KRCLIB)/filc8m.f
$(KRCLIB)/seastau.o: $(KRCLIB)/seastau.f                            $(KRCLIB)/unic8m.f $(KRCLIB)/filc8m.f
$(KRCLIB)/climtau.o: $(KRCLIB)/climtau.f                            $(KRCLIB)/unic8m.f $(KRCLIB)/filc8m.f
$(KRCLIB)/albvar8.o: $(KRCLIB)/albvar8.f $(KRCLIB)/krcc8m.f
$(KRCLIB)/gaspt8.o:   $(KRCLIB)/gaspt8.f $(KRCLIB)/krcc8m.f
$(KRCLIB)/tint8.o:     $(KRCLIB)/tint8.f $(KRCLIB)/krcc8m.f 
$(KRCLIB)/ephemr.o:   $(KRCLIB)/ephemr.f           $(KRCLIB)/porbc8m.f
$(KRCLIB)/porb08.o:   $(KRCLIB)/porb08.f           $(KRCLIB)/porbc8m.f        $(KRCLIB)/unic8m.f
$(KRCLIB)/porbig.o:   $(KRCLIB)/porbig.f           $(KRCLIB)/porbc8m.f
$(KRCLIB)/porbio.o:   $(KRCLIB)/porbio.f           $(KRCLIB)/porbc8m.f
$(KRCLIB)/porbit.o:   $(KRCLIB)/porbit.f           $(KRCLIB)/porbc8m.f  # this is DP   porbit4 is SP
$(KRCLIB)/prtpcom.o: $(KRCLIB)/prtpcom.f           $(KRCLIB)/porbc8m.f
$(KRCLIB)/testrou.o: $(KRCLIB)/testrou.f $(KRCLIB)/krcc8m.f                   $(KRCLIB)/unic8m.f $(KRCLIB)/filc8m.f    # test main program
$(KRCLIB)/wraper8.o: $(KRCLIB)/wraper8.f                            $(KRCLIB)/unic8m.f  # only if D lines
$(KRCLIB)/glot.o: $(KRCLIB)/glot.f $(KRCLIB)/glotcom.f
$(KRCLIB)/readkrcm1.o: $(KRCLIB)/readkrcm1.f $(KRCLIB)/glotcom.f 
#------------------  do not have includes
$(KRCLIB)/alsubs.o: $(KRCLIB)/alsubs.f 
$(KRCLIB)/averag.o: $(KRCLIB)/averag.f  # test for function should be defined and called
$(KRCLIB)/aveyear.o: $(KRCLIB)/aveyear.f
$(KRCLIB)/bigend.o: $(KRCLIB)/bigend.f
$(KRCLIB)/binf5.o: $(KRCLIB)/binf5.f  # uses  B2B  BIGEND  CATIME WHITE1  PIO_system
$(KRCLIB)/cocodp8.o: $(KRCLIB)/cocodp8.f  # Contains: COCOCM  COCOMC  COCOSC  COCOCS  
#                       COCOSM  COCEMC  COCECM 
$(KRCLIB)/cubuterp8.o: $(KRCLIB)/cubuterp8.f
$(KRCLIB)/deding28.o: $(KRCLIB)/deding28.f
$(KRCLIB)/eccanom8.o: $(KRCLIB)/eccanom8.f
$(KRCLIB)/eclipse.o: $(KRCLIB)/eclipse.f 
$(KRCLIB)/epred8.o: $(KRCLIB)/epred8.f
$(KRCLIB)/evmono38.o: $(KRCLIB)/evmono38.f
$(KRCLIB)/evmono3d.o: $(KRCLIB)/evmono3d.f
$(KRCLIB)/finterp.o: $(KRCLIB)/finterp.f
$(KRCLIB)/getpi4.o: $(KRCLIB)/getpi4.f
$(KRCLIB)/getpr8.o: $(KRCLIB)/getpr8.f
$(KRCLIB)/jdate.o:  $(KRCLIB)/jdate.f  
$(KRCLIB)/julday.o:  $(KRCLIB)/julday.f 
$(KRCLIB)/minvr4.o: $(KRCLIB)/minvr4.f
$(KRCLIB)/orbit8.o: $(KRCLIB)/orbit8.f
$(KRCLIB)/porbel.o: $(KRCLIB)/porbel.f 
#$(KRCLIB)/porb1.o: $(KRCLIB)/porb1.f $(KRCLIB)/unic8m.f $(KRCLIB)/porbcom8.f
$(KRCLIB)/readaie.o: $(KRCLIB)/readaie.f
$(KRCLIB)/readtxt360.o: $(KRCLIB)/readtxt360.f
$(KRCLIB)/randomn.o: $(KRCLIB)/randomn.f # used to test sigma8 
#$(KRCLIB)/r2i2s.o: $(KRCLIB)/r2i2s.f
$(KRCLIB)/rotmdp8.o: $(KRCLIB)/rotmdp8.f  # Contains:  MEQUAL  MPROD3  ROTAX  ROTCOL  ROTDIA 
#                      ROTEST  ROTEXM  ROTEXV  ROTMAT  ROTORB  ROTRIP  
#                      ROTROW  ROTSHO  ROTV  ROTVEC  ROTZXM  TRANS3  VROTV
$(KRCLIB)/sigma.o: $(KRCLIB)/sigma.f  
$(KRCLIB)/sigma8.o: $(KRCLIB)/sigma8.f   # used for debug of cubuterp
$(KRCLIB)/spcrev.o: $(KRCLIB)/spcrev.f 
$(KRCLIB)/st2real6.o: $(KRCLIB)/st2real6.f
$(KRCLIB)/strumi.o: $(KRCLIB)/strumi.f
$(KRCLIB)/strumr8.o: $(KRCLIB)/strumr8.f
$(KRCLIB)/tridag8.o: $(KRCLIB)/tridah8.f   # uses prior partial solution
$(KRCLIB)/vlpres.o: $(KRCLIB)/vlpres.f
$(KRCLIB)/vadddp8.o: $(KRCLIB)/vadddp8.f  # Contains:  VADD  VCROSS  VDOT  VEQUAL  VMAG  VNEG 
 #                       VNORM  VPRF  VPRINT  VSCALE  VSHOW  VSUB  VUNIT
$(KRCLIB)/ymd2j2.o: $(KRCLIB)/ymd2j2.f

#----------------------- added after remove use of all but C library
$(KRCLIB)/b2b.o: $(KRCLIB)/b2b.f
$(KRCLIB)/caldat.o: $(KRCLIB)/caldat.f
$(KRCLIB)/caldate.o: $(KRCLIB)/caldate.f
$(KRCLIB)/catime.o: $(KRCLIB)/catime.f
$(KRCLIB)/datime.o: $(KRCLIB)/datime.f
$(KRCLIB)/d2d.o: $(KRCLIB)/d2d.f
$(KRCLIB)/dmd.o: $(KRCLIB)/dmd.f
$(KRCLIB)/d2md.o: $(KRCLIB)/d2md.f
$(KRCLIB)/fillmv.o: $(KRCLIB)/fillmv.f  # has  FILLB FILLI FILLL FILLR FILLD MVB MVI MVL MVR MVD
#                     MVDF MVDM MVD21
$(KRCLIB)/ksubs8.o: $(KRCLIB)/ksubs8.f  # has  AVEDAY  AVEYEAR  CO2PT  SIGMA 
$(KRCLIB)/nowhite.o: $(KRCLIB)/nowhite.f
$(KRCLIB)/otlint8.o: $(KRCLIB)/orlint8.f
$(KRCLIB)/r2r.o: $(KRCLIB)/r2r.f
$(KRCLIB)/upcase.o: $(KRCLIB)/upcase.f
# $(KRCLIB)/white0.o: $(KRCLIB)/white0.f
$(KRCLIB)/white1.o: $(KRCLIB)/white1.f
$(KRCLIB)/vec2code.o: $(KRCLIB)/vec2code.f

# ------------------used only for testing
$(KRCLIB)/bigend1.o: $(KRCLIB)/bigend1.f
catime:o /home/hkieffer/src/for/util/$(KRCLIB)/catime.f
$(KRCLIB)/climtau.o: $(KRCLIB)/climtau.f
$(KRCLIB)/deding2.o: $(KRCLIB)/deding2.f
$(KRCLIB)/dpythag.o: $(KRCLIB)/dpythag.f # - /home/hkieffer/src/for/NumRec/$(KRCLIB)/dpythag.f
$(KRCLIB)/dspline.o: $(KRCLIB)/dspline.f # -
$(KRCLIB)/dsplint.o: $(KRCLIB)/dsplint.f # -
$(KRCLIB)/dsvbksb.o: $(KRCLIB)/dsvbksb.f # -
$(KRCLIB)/dsvdcmp.o: $(KRCLIB)/dsvdcmp.f # calls dpythag
$(KRCLIB)/evrf4.o: $(KRCLIB)/evrf4.f
$(KRCLIB)/hratlsq.o: $(KRCLIB)/hratlsq.f # calls ratval,dsvbksb,dsvdcmp spline splint
$(KRCLIB)/kratlsq.o: $(KRCLIB)/kratlsq.f # calls ratval,dsvbksb,dsvdcmp dspline dsplint
$(KRCLIB)/m2eul.o: $(KRCLIB)/m2eul.f
$(KRCLIB)/qtlats.o: $(KRCLIB)/qtlats.f
$(KRCLIB)/ratval.o: $(KRCLIB)/ratval.f
$(KRCLIB)/spline.o: $(KRCLIB)/spline.f # -
$(KRCLIB)/splint.o: $(KRCLIB)/splint.f # -
$(KRCLIB)/test8.o: $(KRCLIB)/test8.f

### C library make section

# Set up some shell-level specific variables
C_DBG=-g -C

HOST_ARCH ?= $(shell uname -s)
HOST_MACH  = $(shell uname -m)

#  Temporary defines for building in foreign systems
HHKOS      ?= $(HOST_MACH)
HHKINC     ?= $(CURDIR)

#  GNU C compiler and flags.
CC=gcc -pipe -O2  -Wall -fPIC -Dunix -D$(HOST_ARCH) -D$(HOST_MACH) -D$(HHKOS)

#  Special load flags.  These flags are utilized in all builds, whether
#  they are FORTRAN or C
C_LDFLAGS=-shared

# Special library MACROS
# handles archives
AR=ar
# r=replace v=verbose s=include index
ARFLAGS=-rvs

#  Set up source dependancies
CSRCS = $(wildcard $(CSRC_DIR)*.c)
COBJS = $(addsuffix .o, $(basename $(CSRCS)))

#  Define all required targets
call: $(CLIB)

$(CLIB): $(COBJS)
	$(AR) $(ARFLAGS) $(CLIB) $(COBJS)

#  Clean up 
cclean: 
	- $(RM) $(COBJS) $(CLIB)


### IDL module make section
# Makefile for IDL externals for KRC users
#_Hist 2014feb28 HK Derive from Hugh's idl/externals/Makefile
# Tried  -m32 on IDLCFLAGS IDLFFLAGS and LDFAGS; this caused errors
# 2014may05 -lg2c >> -lgfortran
#########################################################################

# Set up some shell-level specific variables
SHELL=/bin/csh 

#  These are the GNU C compiler flags.
IDLCC= gcc -pipe  
IDLCFLAGS= -fPIC -Wall
#  These are the FORTRAN compiler flags
IDLFFLAGS= -fno-automatic -fno-second-underscore -fargument-alias -fd-lines-as-comments -fallow-argument-mismatch -fPIC 

#  Special load flags, utilized in all builds, whether FORTRAN or C
LD=gcc
IDLLDFLAGS= -shared -fPIC # -Wall -Wl
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

#------------------- target dependencies -------------------
IDLOBJDIR = idl/objects
IDLSRCDIR = idl/extern

EXCW = exfuncw.c exfunctionw.c exroutinew.c

# Find C and Fortran sources and make object targets
IDLSRCW = $(shell find $(IDLSRCDIR) -type f -name "*.c")
IDLSRCW_FILTER = $(filter-out $(addprefix $(IDLSRCDIR)/,$(EXCW)), $(IDLSRCW))
IDLOBJSW = $(IDLSRCW_FILTER:.c=.o)

IDLSRCF = $(shell find $(IDLSRCDIR) -type f -name "*.f")
IDLOBJSF = $(IDLSRCF:.f=.o)

IDLOBJSALL= $(IDLOBJSF) $(IDLOBJSW) # concatonate objects

# Pattern rules to match files in source directory with object targets
# Not needed for KRC C files 
$(IDLOBJSW): %.o: %.c
	$(IDLCC) $(IDLCFLAGS) $(INCLUDES) -c $< -o $@

$(IDLOBJSF): %.o: %.f
	$(FC) $(IDLFFLAGS) $(INCLUDES) -c $< -o $@

%.o : $(IDLSRCDIR)/%.c 
	$(IDLCC) $(IDLCFLAGS) $(INCLUDES) -c $<
%.o : $(IDLSRCDIR)/%.f
	$(FC) $(IDLFFLAGS) $(INCLUDES) -c $<
#------------- Actions -----------------------------

# machines running in 64-bit mode, IDL must be in same mode
ftnwrap64.so:	$(IDLOBJSALL)    
	$(LD) $(IDLLDFLAGS)   -o $(IDLSRCDIR)/$@ $(IDLOBJSALL) $(LIBDIRS) \
	$(SYSLIBS)
#	 -lhk_fNumRec $(SYSLIBS)

.PHONY : call cclean clean cleanall cleanidl
#
# Make clean
#
clean:
	- $(RM) $(KRCLIB)/*.o 
cleanbin: 
	- $(RM) krc
	- $(RM) porb

cleanidl:
	-unalias rm; cd idl/extern; rm -f *.o ftnwrap64.so

cleandocs:
	-unalias rm; cd doc_build; rm -f *; cd ../doc_output; rm -f *.pdf

cleanall: cclean clean cleanbin cleanidl cleandocs


### Documentation build section

DOCSRC = doc/

DOCOUT = $(CURDIR)/doc_output/

DOCBUILD = $(CURDIR)/doc_build/

UGDIR = $(DOCSRC)/user_guide/

TEXMKFLAGS = -pdf -pdflatex="pdflatex -interaction=nonstopmode"
TEXMKDEST = -outdir=$(DOCOUT) -auxdir=$(DOCBUILD) -emulate-aux-dir
export BIBINPUTS = $(CURDIR)/doc

.PHONY: docs

docs: doc_output/V34UG.pdf doc_output/slopes.pdf doc_output/eclipse.pdf doc_output/helplist.pdf doc_output/hporb.pdf doc_output/PUG.pdf

doc_output/V34UG.pdf: $(UGDIR)/V34UG.tex $(UGDIR)/farg.tex $(UGDIR)/fard.tex $(UGDIR)/v34p.tex
	cd $(UGDIR); \
	latexmk $(TEXMKFLAGS) $(TEXMKDEST) \
	V34UG.tex 

doc_output/slopes.pdf: $(DOCSRC)/slopes/slopes.tex
	cd $(DOCSRC)/slopes; \
	latexmk $(TEXMKFLAGS) $(TEXMKDEST) \
	slopes.tex

doc_output/eclipse.pdf: $(DOCSRC)/eclipse/eclipse.tex
	cd $(DOCSRC)/eclipse; \
	latexmk $(TEXMKFLAGS) $(TEXMKDEST) \
	eclipse.tex

doc_output/helplist.pdf: $(DOCSRC)/helplist/helplist.tex
	cd $(DOCSRC)/helplist; \
	latexmk $(TEXMKFLAGS) $(TEXMKDEST) \
	helplist.tex

doc_output/hporb.pdf: $(DOCSRC)/hporb/hporb.tex
	cd $(DOCSRC)/hporb; \
	latexmk $(TEXMKFLAGS) $(TEXMKDEST) \
	hporb.tex

doc_output/PUG.pdf: $(DOCSRC)/PUG/PUG.tex
	cd $(DOCSRC)/PUG; \
	latexmk $(TEXMKFLAGS) $(TEXMKDEST) \
	PUG.tex

