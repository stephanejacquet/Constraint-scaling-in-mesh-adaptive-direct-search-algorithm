#!/bin/bash

#-------------------------------------------------------------------------------------#
#  NOMAD - Nonlinear Optimization by Mesh Adaptive Direct search - version 3.8.0      #
#                                                                                     #
#                                                                                     #
#  NOMAD - version 3.8.0 has been created by                                          #
#                  Charles Audet        - Ecole Polytechnique de Montreal             #
#                  Sebastien Le Digabel - Ecole Polytechnique de Montreal             #
#                  Christophe Tribes    - Ecole Polytechnique de Montreal             #
#                                                                                     #
#  The copyright of NOMAD - version 3.8.0 is owned by                                 #
#                  Sebastien Le Digabel - Ecole Polytechnique de Montreal             #
#                  Christophe Tribes    - Ecole Polytechnique de Montreal             #
#                                                                                     #
#  NOMAD v3 has been funded by AFOSR and Exxon Mobil.                                 #
#                                                                                     #
#  NOMAD v3 is a new version of NOMAD v1 and v2. NOMAD v1 and v2 were created and     #
#  developed by Mark Abramson, Charles Audet, Gilles Couture and John E. Dennis Jr.,  #
#  and were funded by AFOSR and Exxon Mobil.                                          #
#                                                                                     #
#                                                                                     #
#  Contact information:                                                               #
#    Ecole Polytechnique de Montreal - GERAD                                          #
#    C.P. 6079, Succ. Centre-ville, Montreal (Quebec) H3C 3A7 Canada                  #
#    e-mail: nomad@gerad.ca                                                           #
#    phone : 1-514-340-6053 #6928                                                     #
#    fax   : 1-514-340-5665                                                           #
#                                                                                     #
#  This program is free software: you can redistribute it and/or modify it under the  #
#  terms of the GNU Lesser General Public License as published by the Free Software   #
#  Foundation, either version 3 of the License, or (at your option) any later         #
#  version.                                                                           #
#                                                                                     #
#  This program is distributed in the hope that it will be useful, but WITHOUT ANY    #
#  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A    #
#  PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.   #
#                                                                                     #
#  You should have received a copy of the GNU Lesser General Public License along     #
#  with this program. If not, see <http://www.gnu.org/licenses/>.                     #
#                                                                                     #
#  You can find information on the NOMAD software at www.gerad.ca/nomad               #
#-------------------------------------------------------------------------------------#
#

echo "*****************************************************"
echo "            NOMAD 3.8.0 Installation Script          "
echo " usage: ./install.sh [nparallel]                     "
echo "       - nparallel : number of parallel process to make " 
echo "*****************************************************"

echo

# Number of parallel process to make
nparallel=$1

BASEDIR=$(dirname $0)

echo "***** Test compilator configuration *****"

echo "Testing gcc and mpi libraries"

cd ${BASEDIR}/../src

VersionGCC=(`g++ -v 2>&1 | awk '$1 ~ /gcc/ && $2 ~ /version/ {print $3}' | awk -F . '{print $1; print $2;print $3}'`)
if [ ${#VersionGCC[@]} -eq 0 ]; then
	echo "======> gcc not installed."
	
	VersionCLANG=(`g++ -v 2>&1 | awk '$1 ~ /Apple/ && $2 ~ /LLVM/  && $3 ~ /version/ {print $4}' | awk -F . '{print $1; print $2}'`)
	if [ ${#VersionCLANG[@]} -eq 0 ]; then
		echo "======> CLANG not installed. No supported compiler is available."
		exit 1
	else
		if [ ${VersionCLANG[0]} -gt 4 ]; then
			echo "=======> CLANG Version ${VersionCLANG[0]}.${VersionCLANG[1]} ---> ok"
		else
			echo "=======> CLANG Version ${VersionCLANG[0]}.${VersionCLANG[1]} ---> Version of CLANG < 5 has not been tested for Nomad! Let us try to compile anyway."
		fi
	fi
else
	if [ ${VersionGCC[0]} -gt 3 ]; then
		echo "=======> gcc Version ${VersionGCC[0]}.${VersionGCC[1]}.${VersionGCC[2]} ---> ok"
	else
		echo "=======> gcc Version ${VersionGCC[0]}.${VersionGCC[1]}.${VersionGCC[2]} ---> Version of gcc < 4 has not been tested for Nomad! Let us try to compile anyway."
	fi
fi



echo "Testing mpic++ "
VersionMPICPP=(`mpic++ -v 2>&1 | awk '$2 ~ /version/ {print $3}' | awk -F . '{print $1; print $2;print $3}'`)
if [ ${#VersionMPICPP[@]} -eq 0 ]; then
	echo "======> mpic++ (gcc) wrapper not available."

	VersionMPICLANG=(`mpic++ -v 2>&1 | awk '$1 ~ /Apple/ && $2 ~ /LLVM/  && $3 ~ /version/ {print $4}' | awk -F . '{print $1; print $2}'`)
	if [ ${#VersionMPICLANG[@]} -eq 0 ]; then
		echo "======> MPI wrapper for CLANG not installed."
		echo "Testing if MPI header available"
		mpiHeaderAvailable=(`g++ -c nomad.cpp -DUSE_MPI 2>&1 | awk 'BEGIN{FS=":"} /mpi.h/ || /fatal error/ {Success=1;exit} END{print Success}' Success=0`)
		if [ ${mpiHeaderAvailable} -eq 1 ]; then
			echo "======> mpi.h not available. NOMAD Parallel version will not be compiled."
			makeMPI=1
		else
			echo "======> mpi.h is present. Proper installation of MPI is supposed available and NOMAD parallel version will be compiled."
			makeMPI=0
			COMPILATOR_MPI="g++"
		fi
	else
		if [ ${VersionMPICLANG[0]} -gt 4 ]; then
			echo "=======> MPI wrapper for CLANG Version ${VersionCLANG[0]}.${VersionCLANG[1]} ---> ok"
		else
			echo "=======> MPI wrapper for CLANG Version ${VersionCLANG[0]}.${VersionCLANG[1]} ---> Version of CLANG < 5 has not been tested for Nomad! Let us try to compile anyway."
		fi
		makeMPI=0
		COMPILATOR_MPI="mpic++"
	fi	
else
	echo "=======> mpicc++ wrapper for gcc version ${VersionMPICPP[0]}.${VersionMPICPP[1]}.${VersionMPICPP[2]} ---> ok"
	makeMPI=0
	COMPILATOR_MPI="mpic++"
fi

echo 

echo "***** Start NOMAD compilation for stand-alone application and library (non-MPI) *****"
make clean
if [ -z $nparallel ]; then
	make all 2>&1 | tee log.txt.$$
else
	make -j $nparallel all 2>&1 | tee log.txt.$$
fi
successMake=$?
success=`awk '/stop./ || /Error/ || /Stop./ {Success=1;exit} END {print Success}' Success=0 log.txt.$$`  
rm -f log.txt.$$
make clean
if [ $success -eq 0 ]; then
	echo "***** NOMAD compilation successful (non-MPI) *****"
else
	echo "***** NOMAD compilation failed (non-MPI) *****"
	exit 1
fi



echo
if [ $makeMPI -eq 0 ]; then
    echo "***** NOMAD compilation stand-alone application and library (MPI) *****"
    make clean
    if [ -z $nparallel ]; then
		make mpi 2>&1 | tee log.txt.$$
	else
		make -j $nparallel mpi 2>&1 | tee log.txt.$$
	fi
	successMake=$?
	success=`awk '/stop./ || /Error/ || /Stop./ {Success=1;exit} END {print Success}' Success=0 log.txt.$$` 
	rm -f log.txt.$$
	make clean
	if [ $success -eq 0 ]; then
		echo "***** NOMAD compilation successful (MPI) *****"
	else
		echo "***** NOMAD compilation failed (MPI) *****"
		exit 1
	fi
fi

cd $BASEDIR
