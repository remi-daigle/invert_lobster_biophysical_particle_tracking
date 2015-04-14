#for i in {1..36}
#do
  #for j in {1..39}
  #do
i=19
j=8
        lat=(45.5216 45.5476 45.5737 45.5997 45.6257 45.6517 45.6776 45.7035 45.7294 45.7553 45.7811 45.807 45.8328 45.8585 45.8843 45.91 45.9357 45.9614 45.9871 46.0127 46.0383 46.0639 46.0895 46.115 46.1405 46.166 46.1915 46.2169 46.2423 46.2677 46.2931 46.3185 46.3438 46.3691 46.3944 46.4196 46.4449 46.4701 46.4952)
        long=(-62.6 -62.5558 -62.5112 -62.4664 -62.4218 -62.3771 -62.3323 -62.2877 -62.243 -62.1984 -62.1538 -62.1093 -62.0648 -62.02 -61.9755 -61.9309 -61.8866 -61.8419 -61.7974 -61.7528 -61.7083 -61.6638 -61.6195 -61.575 -61.5307 -61.4861 -61.4418 -61.3973 -61.353 -61.3086 -61.2642 -61.22 -61.1757 -61.1314 -61.087 -61.0427)
	cd ~/Run/
        ./compile_part_loc
        ./part_loc.o ${long[i-1]} ${lat[j-1]}
	#create temporary behav_param
	# h=hour,m=minute,s=second,swim = vertical swimming speed (mm/s), sv = SD for "swim", Kdiff = K (diffusion index m/s^2), diel (1 or 0), tidal (1 or 0), uzl=upper depth limit, lzl=lower depth limit, modelrez=h or l
	#     yr1 m1 d1 yr2 m2 d2 h1 m1 s1 swim sv Kdiff diel tidal uzl lzl modelrez
	rm -rf ~/Run/behav_param
	rm -rf ~/Run/behav_param2
	rm -rf ~/Run/stages
	rm -rf ~/Run/stages_temp
	rm -rf ~/ADCP/ADCPdata
	rm -rf ~/Run/OUT/output_particles
	sed -n "${1}p" <~/track/behav_table >>~/Run/behav_param
        cp ~/Run/behav_param ~/Run/behav_param2
	#select appropriate ocean.h and parameter.h
	rm -rf ~/model/ocean.h
	rm -rf ~/track/parameter.h
	rm -rf ~/Grid/parameter.h
	while read -r _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ modelrez
	do
		 if [ $modelrez = 'l' ];then
		    cp ~/model/oceanlow.h ~/model/ocean.h
		    cp ~/track/parameterlow.h ~/track/parameter.h
		    cp ~/Grid/parameterlow.h ~/Grid/parameter.h
		 else
		    cp ~/model/oceanhigh.h ~/model/ocean.h
		    cp ~/track/parameterhigh.h ~/track/parameter.h
		    cp ~/Grid/parameterhigh.h ~/Grid/parameter.h
		 fi
	done < ~/Run/behav_param

	# compile Particle_tracking.F90
	cd ~/model/
	./compile
	# compile trans.x (transfer part_loc.ll to part_loc.xy) 
	cd ~/track/
	./compile_trans  #turn this off if not starting with lat/long
	# compile the input maker (part_loc.xy to input_particles) 
	cd ~/Run/
	./compile_make
	#run the model
	./Particle_tracking.o
	# replace spaces with _ in behav_param and create directory
	cd ~/track/tmp/
	s=$(< ~/Run/behav_param)
	rm -rf ~/Run/behav_param
        ll="_${long[i-1]}_${lat[j-1]}"
        s="$s$ll"
	echo $s | awk '{$1=$1}1' OFS="_">>~/Run/behav_param
	mkdir $(< ~/Run/behav_param) #_${long[i-1]}_${lat[j-1]}
	# read track
	cd ~/track/
        gfortran -L/usr/local/include/ -lnetcdf -lnetcdff -I$NETCDF/include -L$NETCDF/lib ../Grid/OPA_GET_XY_LL.F90 ../Grid/ll_to_xy_NEMO.F90 ../divers/util.f read_track_2.f -o read_track2.x
        ./read_track2.x ${long[i-1]} ${lat[j-1]}
	rm -rf ~/Run/behav_param
	rm -rf ~/Run/behav_param2
#  done
#done
cd ~/Output/
_now=$(date +"%m_%d_%Y")
_file="output_$_now.gz"
tar -zcvf $_file ~/track/tmp/
