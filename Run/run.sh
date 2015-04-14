for i in $(eval echo {1..$(wc -l <~/track/behav_table)})
#i=1
do
	cd ~/Run/
	#create temporary behav_param
	# h=hour,m=minute,s=second,swim = vertical swimming speed (mm/s), sv = SD for "swim", Kdiff = K (diffusion index m/s^2), diel (1 or 0), tidal (1 or 0), uzl=upper depth limit, lzl=lower depth limit, modelrez=h or l
	#     yr1 m1 d1 yr2 m2 d2 h1 m1 s1 swim sv Kdiff behav uzl lzl modelrez
        rm -rf ~/Run/bp/behav_param
        rm -rf ~/Run/bp/behav_param2
        rm -rf ~/Run/bp/behav_param3
	sed -n "${i}p" <~/track/behav_table >>~/Run/bp/behav_param
#        cp ~/Run/bp/behav_param ~/Run/bp/behav_param2
	

	#select appropriate ocean.h and parameter.h
	#rm -rf ~/model/ocean.h
	#rm -rf ~/track/parameter.h
	#rm -rf ~/Grid/parameter.h
	while read -r yr1 m1 d1 yr2 m2 d2 h1 min1 s1 swim sv Kdiff behav uzl lzl modelrez
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
        echo $yr1 $m1 $d1 $swim $sv $Kdiff $behav $uzl $lzl | awk '{$1=$1}1' OFS="_">>~/Run/bp/behav_param3
	done < ~/Run/bp/behav_param
        s=$(< ~/Run/bp/behav_param)
	echo $s | awk '{$1=$1}1' OFS="_">>~/Run/bp/behav_param2
#        echo $~/Run/bp/behav_param2 | awk '{$1=$1}1' OFS="_">>~/Run/bp/behav_param2
        cp ~/Run/bp/behav_param3 ~/Run/bp/3_$(< ~/Run/bp/behav_param3)
        cp ~/Run/bp/behav_param ~/Run/bp/1_$(< ~/Run/bp/behav_param3)
        cp ~/Run/bp/behav_param2 ~/Run/bp/2_$(< ~/Run/bp/behav_param3)
	
	sleep 1

	# compile Particle_tracking.F90
	cd ~/model/
	./compile

	sleep 1 

	cp ~/Run/Particle_tracking.o ~/Run/Particle_tracking/p$i

	# compile trans.x (transfer part_loc.ll to part_loc.xy)

	

	cd ~/track/
	./compile_trans  #turn this off if not starting with lat/long

	sleep 1

	# compile the input maker (part_loc.xy to input_particles) 
	cd ~/Run/
	./compile_make

	sleep 5
 
	#run the model
	f=$(< ~/Run/bp/behav_param3)
        #~/Run/Particle_tracking/p$i $f

		if [ $i = $(wc -l <~/track/behav_table) ]; then
		   qsub -cwd -l h_rt=23:55:0 -v i=$i,f=$f -m e -M daigleremi@gmail.com ../treatment_run.sh
		else
		   qsub -cwd -l h_rt=23:55:0 -v i=$i,f=$f ../treatment_run.sh
		fi
        

        
	sleep 2

done
