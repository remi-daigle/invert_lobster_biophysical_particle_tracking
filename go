#clear tmp
rm -rfv ~/track/tmp/*
rm -rfv ~/Run/OUT/*
rm -rfv ~/Run/bp/1*
rm -rfv ~/Run/bp/2*
rm -rfv ~/Run/bp/3*
rm -rfv ~/Run/bp/*
rm -rfv ~/Run/core.*
rm -rfv ~/Run/Particle_tracking/*
rm -rfv ~/Run/stage/*
rm -rfv ~/ADCP/ADCPdata
rm -rfv ~/Run/treatment_run.sh.o*
rm -rfv ~/Run/treatment_run.sh.e*
rm -rfv ~/run.sh.*
rm -rfv ~/process.*
# submit to grid engine
qsub -cwd -l h_rt=47:55:55 -m e -M daigleremi@gmail.com ~/Run/run.sh 
#~/Run/run.sh

