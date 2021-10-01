#ifndef __RUNNER_DEFINES__
#define __RUNNER_DEFINES__

#include "nomad.hpp"

/*-------------------------------------*/
/*               constants             */
/*-------------------------------------*/
const std::string ENV              = "MAC";
// const std::string ENV           = "LINUX";

const std::string RUNNER_VERSION   = "2.11"; // 2.11 for granular variables (continuous) //2.10 for testing ordo strategies // 2.9 add RUN_SEEDS // 2.8 add ANISOTROPIC_MESH param // 2.7 -> add user_param   // 2.6-> account for Ortho n+1 in Version 3.6.0  // 2.4-> add model_dyn_dir parameter
const std::string PROBLEMS_DIR     = "problems/";
const std::string TESTS_DIR        = "tests/";
const std::string ID_FILE          = "id.txt";
const std::string STATS_FILE       = "stats.txt";
const std::string MODEL_STATS_FILE = "model_stats.txt";
const std::string TMP_FILE         = "tmp.txt";

const std::string TABLE_OUT_FILE   = "outputs/results_table.txt";
const std::string PP_OUT_FILE      = "outputs/pp";
const std::string DP_OUT_FILE      = "outputs/dp"; // tau is added to the name
const std::string PPS_OUT_FILE     = "outputs/pps.txt";

const int           PP_NB_LINES  = 100;   // number of lines in perf. profiles
const NOMAD::Double PP_MAX_ALPHA = 100.0; // max alpha value in perf. profiles

const int           MAX_BB_EVAL  = 20000; // global max number of evaluatons (DEFAULT VALUE)

const bool USE_BEST_EVER_SOL = false ; // if true  : the best ever solution of a problem is considered (known best before or obtained during runs)
										// otherwise: the best solution for the current runs is considered
										// This option has no effect if MW_DATA_PROFILE==true

const bool USE_REL_ERROR_FX0_DP = true ; // if true  : a relative error using best solution and initial solution is used for data profiles)
									// otherwise: the relative error from NOMAD::Double is used

const bool MW_DATA_PROFILE = true ; // if true : determine data profile using Moré-Wild SIAM jopt 2009  ; if false use Seb strategie
const bool MW_PERF_PROFILE = true ; // if true : determine performance profile using Moré-Wild SIAM jopt 2009  ; if false use Seb strategie
const int  NB_SIMPLEX_GRAD=400; //  number of simplex grad -> number of lines in data profile using Moré-Wild SIAM jopt 2009

// C.Tribes feb 5, 2014 --- added to perform several nomad run per problem per algorithm by changing the seed.
// const int RUN_SEEDS[]={0,10};
// const int RUN_SEEDS[]={4,12,40,120,400,1200,4000,12000,40000,120000};
// const int RUN_SEEDS[]={3571} ; // ,84913};
//  const int RUN_SEEDS[]={93399,67874,75774}; //
const int RUN_SEEDS[]={65574,74313,39223,65548,17119,70605,3183,27692,4617,3571} ; // ,9713,82346,69483,31710,95022,3445};
// const int RUN_SEEDS[]={0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19};
// const int RUN_SEEDS[]={93399,67874,75774,9713,82346,69483,31710,95022,3445,84913}; //
//const int RUN_SEEDS[]={65574,3571,74313,39223,65548,17119,70605,3183,27692,4617,93399,67874,75774,9713,82346,69483,31710,95022,3445,84913};
// const int RUN_SEEDS[]={1000};

#endif
