#FOR COD
#DEFINES = -DWIND -DBIOLOGY -DCOD    #CRAB or LOBSTER
#TARGETS = GSS4_cod
#
#
#FOR LOBSTER
#DEFINES = -DTRAJECTORY -DWIND -DCHECK -DBIOLOGY -DLOBSTER  #CRAB or LOBSTER
DEFINES = -DWIND -DBIOLOGY -DLOBSTER    #CRAB or LOBSTER
TARGETS = GSS4_lobster_new_mortality
#
#
#FOR CRAB
#DEFINES =  -DTRAJECTORY -DWIND -DCHECK -DBIOLOGY -DCRAB   #CRAB or LOBSTER
#DEFINES =  -DWIND -DBIOLOGY -DCRAB    #CRAB or LOBSTER
#TARGETS = GSS4_crab
#
#FOR SHRIMP
#DEFINES = -DWIND -DBIOLOGY -DSHRIMP
#TARGETS = GSS4_shrimp
#
#FOR SCALLOP 
#DEFINES = -DWIND -DBIOLOGY -DSCALLOP
#TARGETS = GSS4_scallop
#
#FOR SALMON
#DEFINES = -DWIND -DBIOLOGY -DSALMON
#TARGETS = GSS4_salmon
#
#
#DEFINES = -DTRAJECTORY
#DEFINES = -DTRAJECTORY -DCHECK -DWIND
#DEFINES = -DTRAJECTORY -DCHECK 
#DEFINES = -DTRAJECTORY -DWIND 
#DEFINES = -DWIND 
#TARGETS = ../GSS4_dx_dy
#
OPTIONS = -O2 
#OPTIONS = -O2 -Mbounds
#OPTIONS = -O2 -Mconcur
OBJECTS =ocean_model.o biology.o openbound.o fluxes3.o sigmat.o motmit2.o wcalc.o stability2.o xpand.o ll_to_xpyp_GSS4.o temp_salt_to_st.o rivers2.o divers.o output.o init.o druxav.o eddy.o kotief.o traj.o sor.o  gatmit.o blockdata.o update2.o xygrad.o zeros.o vuf2.o adv_mix.o util.o field_shift.o field_bc.o shift2.o atmosphere.o

$(TARGETS): $(OBJECTS) ocean_header.h
	 f90 $(OPTIONS)   $(OBJECTS)  -o $(TARGETS)
traj.o:	traj.F ocean_header.h
	f90 $(OPTIONS)  -c  $(DEFINES) traj.F
openbound.o: openbound.F ocean_header.h
	f90 $(OPTIONS)  -c  $(DEFINES)    openbound.F 
atmosphere.o:	atmosphere.F ocean_header.h atmosphere.h
	f90 $(OPTIONS)  -c  $(DEFINES) atmosphere.F
vuf2.o: vuf2.F
	 f90 $(OPTIONS)  -c   $(DEFINES)    vuf2.F
xpand.o:	xpand.F
	f90 $(OPTIONS)  -c  $(DEFINES) xpand.F
ll_to_xpyp_GSS4.o:	ll_to_xpyp_GSS4.F
	f90 $(OPTIONS)  -c  $(DEFINES) ll_to_xpyp_GSS4.F
fluxes3.o: fluxes3.F ocean_header.h
	 f90 $(OPTIONS)  -c  $(DEFINES)    fluxes3.F 
adv_mix.o: adv_mix.F ocean_header.h
	 f90 $(OPTIONS)  -c  $(DEFINES)    adv_mix.F 
biology.o: biology.F ocean_header.h biology.h
	 f90 $(OPTIONS)  -c  $(DEFINES)    biology.F 
sigmat.o: sigmat.F        ocean_header.h
	 f90 $(OPTIONS)  -c  $(DEFINES)    sigmat.F
temp_salt_to_st.o: temp_salt_to_st.F        ocean_header.h
	 f90 $(OPTIONS)  -c  $(DEFINES)    temp_salt_to_st.F
gatmit.o: gatmit.F        ocean_header.h
	 f90 $(OPTIONS)  -c  $(DEFINES)    gatmit.F
blockdata.o: blockdata.F        ocean_header.h
	 f90 $(OPTIONS)  -c  $(DEFINES)    blockdata.F
motmit2.o: motmit2.F        ocean_header.h
	 f90 $(OPTIONS)  -c  $(DEFINES)    motmit2.F
wcalc.o: wcalc.F          ocean_header.h
	 f90 $(OPTIONS)  -c  $(DEFINES)    wcalc.F
stability2.o: stability2.F  ocean_header.h
	 f90 $(OPTIONS)  -c  $(DEFINES)    stability2.F
update2.o: update2.F        ocean_header.h
	 f90 $(OPTIONS)  -c  $(DEFINES)    update2.F
eddy.o: eddy.F            ocean_header.h
	 f90 $(OPTIONS)  -c  $(DEFINES)    eddy.F
kotief.o: kotief.F        ocean_header.h
	 f90 $(OPTIONS)  -c  $(DEFINES)    kotief.F
druxav.o: druxav.F      ocean_header.h
	 f90 $(OPTIONS)  -c  $(DEFINES)    druxav.F
ocean_model.o: ocean_model.F        ocean_header.h biology.h
	 f90 $(OPTIONS) -c  $(DEFINES)    ocean_model.F
output.o: output.F            ocean_header.h biology.h
	 f90 $(OPTIONS)  -c  $(DEFINES)    output.F
rivers2.o: rivers2.F            ocean_header.h
	 f90 $(OPTIONS)  -c  $(DEFINES)    rivers2.F
divers.o: divers.F            ocean_header.h
	 f90 $(OPTIONS)  -c  $(DEFINES)    divers.F
sor.o: sor.F              ocean_header.h
	 f90 $(OPTIONS)  -c  $(DEFINES)    sor.F
init.o: init.F            ocean_header.h biology.h
	 f90 $(OPTIONS)  -c  $(DEFINES)    init.F
xygrad.o:   xygrad.F            ocean_header.h
	 f90 $(OPTIONS)  -c  $(DEFINES)    xygrad.F
zeros.o:   zeros.F            ocean_header.h
	 f90 $(OPTIONS)  -c  $(DEFINES)    zeros.F
field_shift.o:   field_shift.F            ocean_header.h
	 f90 $(OPTIONS)  -c  $(DEFINES)    field_shift.F
field_bc.o:   field_bc.F            ocean_header.h
	 f90 $(OPTIONS)  -c  $(DEFINES)    field_bc.F
shift2.o:   shift2.F            ocean_header.h
	 f90 $(OPTIONS)  -c  $(DEFINES)    shift2.F
util.o:   util.F      
	 f90 $(OPTIONS)  -c  $(DEFINES)    util.F

