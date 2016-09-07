PFUNIT = $(HOME)/bin/pfunit/pfunit-serial
CABLE_DIR = /short/w35/ccc561/CABLE/CABLE_IO

include $(PFUNIT)/include/base.mk

SRCS = $(wildcard *.pf)
OBJS = $(SRCS:.pf=$(OBJ_EXT))
NCMOD = $(NETCDF_ROOT)/include/Intel
FFLAGS := $(FFLAGS) -I$(NCMOD) -I$(PFUNIT)/mod
LIBS:= -L/apps/netcdf/4.3.3.1/lib -lnetcdff -lnetcdf

#default rule for .pf
%.F90: %.pf
	$(PFUNIT)/bin/pFUnitParser.py $< $@

all: tests_io.x

#Compile tests
tests_io.x: testSuites.inc test_io$(OBJ_EXT) cable_gswp3_mod$(OBJ_EXT)
	$(FC) -o $@ -I$(PFUNIT)/mod -I$(PFUNIT)/include \
	$(PFUNIT)/include/driver.F90 ./*$(OBJ_EXT) $(LIBS) $(FFLAGS) -L/home/561/ccc561/bin/pfunit/pfunit-serial/lib -lpfunit

#Copy files from CABLE directory
cable_gswp3_mod.F90:
	cp $(CABLE_DIR)/offline/cable_gswp3_mod.F90 .

cable_iovars.F90: 
	cp $(CABLE_DIR)/offline/cable_iovars.F90 .

cable_define_types.F90:
	cp $(CABLE_DIR)/core/biogeophys/cable_define_types.F90 .

cable_common.F90:
	cp $(CABLE_DIR)/core/biogeophys/cable_common.F90 .

#Compile CABLE files
cable_gswp3_mod$(OBJ_EXT): cable_gswp3_mod.F90 cable_common$(OBJ_EXT) cable_iovars$(OBJ_EXT)

cable_iovars$(OBJ_EXT): cable_iovars.F90 cable_define_types$(OBJ_EXT)

cable_define_types$(OBJ_EXT): cable_define_types.F90

cable_common$(OBJ_EXT): cable_common.F90 cable_define_types$(OBJ_EXT)

#
test_io$(OBJ_EXT): test_io.F90 cable_gswp3_mod$(OBJ_EXT)

test_io.F90: test_io.pf

#copy: 
#	cp $(CABLE_DIR)/offline/cable_gswp3_mod.F90 .
#	cp $(CABLE_DIR)/core/biogeophys/cable_common.F90 .
#	cp $(CABLE_DIR)/core/biogeophys/cable_define_types.F90 .
#	cp $(CABLE_DIR)/offline/cable_iovars.F90 .

