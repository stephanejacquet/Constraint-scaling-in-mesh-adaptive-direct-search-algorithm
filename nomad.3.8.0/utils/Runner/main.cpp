#include "Runner.hpp"

/*-------------------------------------*/
/*          RUNNER main function       */
/*-------------------------------------*/
int main ( int argc , char ** argv ) {

  MPI_Init ( &argc , &argv );

  int rank , np;

  MPI_Comm_size ( MPI_COMM_WORLD , &np   );
  MPI_Comm_rank ( MPI_COMM_WORLD , &rank );

  if ( np <= 1 ) {
    if ( rank == 0 )
      std::cerr << std::endl << "usage: \'mpirun -np p "
		<< argv[0] << " (config test files)\' with p>1"
		<< std::endl << std::endl;
    MPI_Finalize();
    return 1;
  }

  // display:
  NOMAD::Display out ( std::cout );

  // Runner object:
  Runner runner ( out );
    
    
//  // clear tests:
//  runner.select_problems_by_keyword ( "MORE_WILD_PAPER" );
//    if ( rank == 0 )
//    {
//        for ( int i = 1 ; i < argc ; ++i )
//        {
//            std::string error_msg;
//            if ( !runner.read_test_config ( argv[i] , error_msg ) )
//                out << "warning: " << error_msg << std::endl;
//        }
//        runner.clear_tests_by_config_and_selected_pbs("NOMAD","*");
//    }
//    MPI_Finalize();
//    return 0;

  
  // clear tests:
  // runner.clear_tests_by_solver ( "nomad" , "*" );
  // runner.clear_all_tests();
  // runner.clear_invalid_tests();
  // MPI_Finalize();
  // return 0;

  // display all problems:
  // runner.display_all_problems();

  /*-----------------------------------------------------------*/

  // select problems:
  // ----------------
    
    
    /*--------------------------------*/
    /*  Test from Anne-Sophie Crelot  */
    /*--------------------------------*/
//    runner.select_problems_by_keyword ( "anne_sophie_crelot"   );
//    runner.exclude_problem_by_name ( " SteppedCantileverBeamMOD1" );
// runner.select_problem_by_name ( "RHEOLOGY" );
    // runner.select_problems_by_keyword ( "PressureVessel_Mixed_Case1"   );

  // runner.select_all_problems();
	// runner.refine_problems_by_keyword ( "constrained"         );

 //runner.exclude_problems_by_size    ( 21 , 1000 );
//	runner.exclude_problem_by_name ( "WELL" );
  // runner.exclude_problems_by_keyword ( "batch"   );
  // runner.select_problem_by_name ( "RHEOLOGY" );
  // runner.select_problem_by_name ( "GRIEWANK" );
  // runner.select_problems_by_size  ( 0 , 60 );
  
	
  /*------------------*/
  /*  MADS-DFO paper  */
  /*------------------*/
  runner.select_problems_by_keyword ( "MADS_DFO_PAPER" );
  runner.exclude_problem_by_name ( "STYRENE" ); // strange zero appears
  runner.exclude_problem_by_name ( "DIFFICULT2" ); // strange zero appears
  //runner.exclude_problems_by_size  ( 13 , 1000 ); // small problems only 
  // runner.exclude_problems_by_size  ( 0 , 19 );    // big problems only
  //runner.exclude_problems_by_keyword ( "constrained" );	

  /*---------------------*/
  /*  More & Wild paper  */
  /*---------------------*/
//    runner.select_problem_by_name ( "MORE_WILD_1_LINEAR_FULL_SMOOTH" );
//	runner.select_problem_by_name ("MORE_WILD_2_LINEAR_FULL_SMOOTH");
//	runner.select_problem_by_name ("MORE_WILD_3_LINEAR_R1_SMOOTH");
//	runner.select_problem_by_name ("MORE_WILD_4_LINEAR_R1_SMOOTH");
//	runner.select_problem_by_name ( "MORE_WILD_2_LINEAR_FULL_SMOOTH");
				
// runner.select_problems_by_keyword ( "MORE_WILD_PAPER" );
    
//    runner.exclude_problems_by_size  ( 0 , 7 ); // small problems only
    
// runner.exclude_problems_by_keyword ( "NOISY3" );
  // runner.refine_problems_by_keyword ( "NONDIFF"         );
// runner.refine_problems_by_keyword ( "NOISY3"         );
  
// runner.refine_problems_by_keyword ( "SMOOTH"          );
//   runner.exclude_problems_by_keyword ( "constrained" );
//runner.refine_problems_by_keyword ( "WILD3"           );
//runner.exclude_problems_by_keyword ( "NONDIFF"  );
//runner.exclude_problems_by_keyword ( "NOISY3" );

  // runner.exclude_problem_by_name ( "MORE_WILD_35_BROWN_SMOOTH"  ); // bad x0 in SID-PSM
  // runner.exclude_problem_by_name ( "MORE_WILD_35_BROWN_NONDIFF" ); // bad x0 in SID-PSM
  // runner.exclude_problem_by_name ( "MORE_WILD_35_BROWN_WILD3"   ); // bad x0 in SID-PSM

  // runner.exclude_problem_by_name ( "MORE_WILD_43_CUBE_SMOOTH" ); // strange zero appears
  // runner.exclude_problem_by_name ( "MORE_WILD_43_CUBE_WILD3"  ); // strange zero appears
  // runner.exclude_problem_by_name ( "MORE_WILD_44_CUBE_SMOOTH" ); // strange zero appears
  // runner.exclude_problem_by_name ( "MORE_WILD_44_CUBE_WILD3"  ); // strange zero appears
  // runner.exclude_problem_by_name ( "MORE_WILD_45_CUBE_SMOOTH" ); // strange zero appears
  // runner.exclude_problem_by_name ( "MORE_WILD_45_CUBE_WILD3"  ); // strange zero appears

  /*---------------------*/
  /*  OrhoMADS problems  */
  /*---------------------*/

  // runner.select_problems_by_keyword ( "orthomads_paper" );

  // OrthoMADS table 1/4 (smooth unconstrained):
  // runner.refine_problems_by_keyword ( "smooth" );
  // runner.exclude_problems_by_keyword ( "constrained" );

  
  // OrthoMADS table 2/4 (nonsmooth unconstrained):
   // runner.exclude_problems_by_keyword ( "constrained" );
  // runner.exclude_problems_by_keyword ( "smooth"      );

  // OrthoMADS table 3/4 (constrained):
  //  runner.refine_problems_by_keyword ( "constrained"      );
  //  runner.exclude_problems_by_keyword ( "real_application" );

  // OrthoMADS table 4/4 (applications):
  // runner.refine_problems_by_keyword ( "real_application" );
  
	
  /*---------------------*/
  /*  OrhoMADS n+1       */
  /*---------------------*/
//	runner.select_problem_by_name ( "ARWHEAD10" );
//	runner.select_problem_by_name ( "BDQRTIC10" );
//	runner.select_problem_by_name ( "BIGGS6" );
//	runner.select_problem_by_name ( "BRANIN" );
//	runner.select_problem_by_name ( "BROWNAL10" );
//	runner.select_problem_by_name ( "CRESCENT10" );
//	runner.select_problem_by_name ( "DIFFICULT2" );
//	runner.select_problem_by_name ( "DISK10" );
//	runner.select_problem_by_name ( "ELATTAR" );
//	runner.select_problem_by_name ( "EVD61" );
//	runner.select_problem_by_name ( "FILTER" );
//	runner.select_problem_by_name ( "G210" );
//	runner.select_problem_by_name ( "GRIEWANK" );
//	runner.select_problem_by_name ( "HS78" );
//	runner.select_problem_by_name ( "HS114" );
//	runner.select_problem_by_name ( "MAD6" );
//	runner.select_problem_by_name ( "OSBORNE2" );
//	runner.select_problem_by_name ( "PBC1" );
//	runner.select_problem_by_name ( "PENALTY110" );
//	runner.select_problem_by_name ( "PENALTY210" );
//	runner.select_problem_by_name ( "PENTAGON" );
//	runner.select_problem_by_name ( "POLAK2" );
//	runner.select_problem_by_name ( "POWELLSG12" );
//	runner.select_problem_by_name ( "RASTRIGIN" );
//	runner.select_problem_by_name ( "SHOR" );
//	runner.select_problem_by_name ( "SNAKE" );
//	runner.select_problem_by_name ( "SROSENBR10" );
//	runner.select_problem_by_name ( "TRIDIA10" );
//	runner.select_problem_by_name ( "VARDIM10" );
//	runner.select_problem_by_name ( "WONG1" );
//	runner.select_problem_by_name ( "WONG2" );
//	runner.select_problem_by_name ( "WOODS12" );
//	
//	runner.refine_problems_by_keyword ( "constrained"      );


  /*-----------------------------------------------------------*/

  // display selected problems:
  runner.display_selected_problems();

  // display available tests:
  // runner.display_available_tests();

  // select test configs (entered as arguments; a default
  // test config is created if no arguments):
  if ( argc == 1 ) {
    runner.create_def_test_config();
  }
  else {
    for ( int i = 1 ; i < argc ; ++i ) {
      std::string error_msg;
      if ( !runner.read_test_config ( argv[i] , error_msg ) ) {
	if ( rank == 0 )
	  out << "warning: " << error_msg << std::endl;
      }
    }
  }

  // display test configs:
  runner.display_test_configs();

//  MPI_Finalize();
//  return 0;

  // run:
  std::string error_msg;
  if ( !runner.run ( error_msg ) && rank==0 )
    out << "runner.run() returned the error \""
	<< error_msg << "\"" << std::endl;

  // display stats:
  else {

    // display results:
    runner.display_algo_diff();
     //runner.display_perf_prof();
     runner.display_perf_prof_MW( 1 );
runner.display_perf_prof_MW( 2 );
runner.display_perf_prof_MW( 3 );
runner.display_perf_prof_MW( 4 );
runner.display_perf_prof_MW( 5 );
runner.display_perf_prof_MW( 6 );
runner.display_perf_prof_MW( 7 );
runner.display_perf_prof_MW( 8 );
	//runner.display_perf_prof_MW( 3 );
	//runner.display_perf_prof_MW( 5 );
	//runner.display_perf_prof_MW( 7 );

	  
	  if (MW_DATA_PROFILE)
	  {
		  runner.display_data_prof_MW ( 1);
		  runner.display_data_prof_MW ( 2);
		  runner.display_data_prof_MW ( 3);
		  runner.display_data_prof_MW ( 4);
		  runner.display_data_prof_MW ( 5);
		  runner.display_data_prof_MW ( 6);
		  runner.display_data_prof_MW ( 7);
		  runner.display_data_prof_MW ( 8);
	  }
	  else
	  {
		  runner.display_data_prof ( 1 );
		  runner.display_data_prof ( 3 );
		  runner.display_data_prof ( 5 );
		  runner.display_data_prof ( 7 );
	  }

		  
    // runner.display_perf_prof_s();
    // runner.display_results_table();
  }

  MPI_Finalize();
  return 0;
}

// Procedure to add a new problem:
// -------------------------------
//
//  1. create and complete directory in problems/X
//  2. update makefile
//  3. add #include "X.hpp" in Runner.hpp
//  4. add in Runner::Runner():
//     X * x = new X;
//     _all_pbs.push_back ( x );
//  5. test evaluation with Problem::test_eval_x()


// MENU:
// -----
//
//   RUNNER version X.X
//
//   1. Select problems
//      1. Select all problems
//      2. Select a single problem
//      3. Select a group of problems
//      1. Load selection
//      2. Save selection
//   2. Define test configurations
//      1. New test configuration
//      2. Load configurations
//      3. Save configurations
//   3. Display data
//      1. All problems
//      2. Selected problems
//      3. Test configurations
//      4. Available tests
//   4. Clear data
//      1. Selected problems
//      2. Test configurations
//      3. Remove invalid tests
//      4. Remove all tests
//   5. Execute tests
//   6. Display results
//
//   7. Quit
//
//   > _
