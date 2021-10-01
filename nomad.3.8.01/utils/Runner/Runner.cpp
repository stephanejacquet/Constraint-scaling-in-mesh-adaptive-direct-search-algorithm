#include "Runner.hpp"



/*----------------------------------*/
/*            constructor           */
/*----------------------------------*/
Runner::Runner ( NOMAD::Display & out ) : _out     ( out  ) ,
_n_pb       ( 0    ) ,
_n_algo     ( 0    ) ,
_n_seed_run ( 0    ) ,
_results    ( NULL ) ,
_test_id    ( NULL )
{
    // rank and np:
    MPI_Comm_rank ( MPI_COMM_WORLD , &_rank );
    MPI_Comm_size ( MPI_COMM_WORLD , &_np   );
    
    // construct list of all problems:
    _all_pbs.push_back ( new Arwhead   (  10 ) );
    _all_pbs.push_back ( new Arwhead   (  20 ) );
    _all_pbs.push_back ( new B250              );
    _all_pbs.push_back ( new B500              );
    _all_pbs.push_back ( new Bdqrtic   (  10 ) );
    _all_pbs.push_back ( new Bdqrtic   (  20 ) );
    _all_pbs.push_back ( new Biggs6            );
    _all_pbs.push_back ( new Branin            );
    _all_pbs.push_back ( new Brownal   (  10 ) );
    _all_pbs.push_back ( new Brownal   (  20 ) );
    _all_pbs.push_back ( new Crescent  (  10 ) );
    _all_pbs.push_back ( new Difficult2        );
    _all_pbs.push_back ( new Disk      (  10 ) );
    _all_pbs.push_back ( new ElAttar           );
    _all_pbs.push_back ( new Evd61             );
    _all_pbs.push_back ( new Filter            );
    _all_pbs.push_back ( new G2        (  10 ) );
    _all_pbs.push_back ( new G2        (  20 ) );
    _all_pbs.push_back ( new G2        (  50 ) );
    _all_pbs.push_back ( new G2        ( 250 ) );
    _all_pbs.push_back ( new G2        ( 500 ) );
    _all_pbs.push_back ( new Goffin            );
    _all_pbs.push_back ( new Griewank          );
    _all_pbs.push_back ( new Hs78              ) ;
    _all_pbs.push_back ( new Hs114             );
    _all_pbs.push_back ( new L1Hilb            );
    _all_pbs.push_back ( new Mad6              );
    _all_pbs.push_back ( new Mdo               );
    _all_pbs.push_back ( new MxHilb            );
    _all_pbs.push_back ( new OptEng_RBF        );
    _all_pbs.push_back ( new Osborne2          );
    _all_pbs.push_back ( new Pbc1              );
    _all_pbs.push_back ( new Penalty1  (  10 ) );
    _all_pbs.push_back ( new Penalty1  (  20 ) );
    _all_pbs.push_back ( new Penalty2  (  10 ) );
    _all_pbs.push_back ( new Penalty2  (  20 ) );
    _all_pbs.push_back ( new Pentagon          );
    _all_pbs.push_back ( new Pigache           );
    _all_pbs.push_back ( new Polak2            );
    _all_pbs.push_back ( new Powellsg  (  12 ) );
    _all_pbs.push_back ( new Powellsg  (  20 ) );
    _all_pbs.push_back ( new Radar7            );
    _all_pbs.push_back ( new Rana              );
    _all_pbs.push_back ( new Rastrigin         );
    _all_pbs.push_back ( new Rheology          );
    _all_pbs.push_back ( new Rosenbrock        );
    _all_pbs.push_back ( new Shor              );
    _all_pbs.push_back ( new Snake             );
    _all_pbs.push_back ( new Srosenbr  (  10 ) );
    _all_pbs.push_back ( new Srosenbr  (  20 ) );
    _all_pbs.push_back ( new Styrene           );
    _all_pbs.push_back ( new Trefethen         );
    _all_pbs.push_back ( new Tridia    (  10 ) );
    _all_pbs.push_back ( new Tridia    (  20 ) );
    _all_pbs.push_back ( new Vardim    (  10 ) );
    _all_pbs.push_back ( new Vardim    (  20 ) );
    _all_pbs.push_back ( new Watson12          );
    _all_pbs.push_back ( new Well              );
    _all_pbs.push_back ( new Wong1             );
    _all_pbs.push_back ( new Wong2             );
    _all_pbs.push_back ( new Woods     (  12 ) );
    _all_pbs.push_back ( new Woods     (  20 ) );
    _all_pbs.push_back ( new XuWang_f1         );
    _all_pbs.push_back ( new XuWang_f3         );
    _all_pbs.push_back ( new XuWang_f5         );
    _all_pbs.push_back ( new XuWang_f7 (   9 ) );
    _all_pbs.push_back ( new XuWang_f8         );
    _all_pbs.push_back ( new XuWang_f10        );
    _all_pbs.push_back ( new XuWang_f11        );
    _all_pbs.push_back ( new XuWang_f12        );
    _all_pbs.push_back ( new XuWang_f13        );
    _all_pbs.push_back ( new XuWang_f14        );
    _all_pbs.push_back ( new Barnes_Mixed_Case11 );
    _all_pbs.push_back ( new Barnes_Mixed_Case21 );
    // TODO categorical _all_pbs.push_back ( new Barnes_Mixed_Case31 );
    _all_pbs.push_back ( new CarSideImpact1 );
   // TODO categorical _all_pbs.push_back ( new CarSideImpactDC1 );
    _all_pbs.push_back ( new G07_Mixed_Case31 );
    // TODO categorical _all_pbs.push_back ( new G07_Mixed_Case41 );
    _all_pbs.push_back ( new G9_Mixed_Case1 );
    // TODO categorical _all_pbs.push_back ( new G9_Mixed_CaseIC1 );
    _all_pbs.push_back ( new Mystery_Mixed_Case11 );
    _all_pbs.push_back ( new Mystery_Mixed_Case21 );
    //  TODO categorical _all_pbs.push_back ( new Mystery_Mixed_Case31 );
    _all_pbs.push_back ( new Mystery_Mixed_Case61 );
    _all_pbs.push_back ( new PressureVessel_Mixed_Case1 );
    // TODO categorical _all_pbs.push_back ( new PressureVessel_Mixed_CaseIC1 );
    _all_pbs.push_back ( new Rastrigin_Mixed_Case11 );
    _all_pbs.push_back ( new Rastrigin_Mixed_Case21 );
    // TODO categorical _all_pbs.push_back ( new Rastrigin_Mixed_Case31 );
    // TODO categorical _all_pbs.push_back ( new Rastrigin_Mixed_Case121 );
    // TODO categorical _all_pbs.push_back ( new Rastrigin_Mixed_Case221 );
    _all_pbs.push_back ( new ReinforcedConcreteBeamMOD1 );
    // TODO categorical  _all_pbs.push_back ( new ReinforcedConcreteBeamMODDC1 );
    // TODO categorical  _all_pbs.push_back ( new ReinforcedConcreteBeamMODIC1 );
    // TODO categorical  _all_pbs.push_back ( new ReinforcedConcreteBeamMODIDC1 );
    _all_pbs.push_back ( new Rosenbrock_Mixed_Case11 );
    _all_pbs.push_back ( new Rosenbrock_Mixed_Case21 );
    // TODO categorical  _all_pbs.push_back ( new Rosenbrock_Mixed_Case21 );
    // TODO categorical  _all_pbs.push_back ( new Rosenbrock_Mixed_Case121 );
    // TODO categorical  _all_pbs.push_back ( new Rosenbrock_Mixed_Case221 );
    _all_pbs.push_back ( new SpeedReducerMOD_Mixed_Case1 );
    // TODO categorical  _all_pbs.push_back ( new SpeedReducerMOD_Mixed_CaseIC1 );
    _all_pbs.push_back ( new SpringMOD_Mixed_Case1 );
    // TODO categorical  _all_pbs.push_back ( new SpringMOD_Mixed_CaseIC1 );
    _all_pbs.push_back ( new SteppedCantileverBeamMOD1 );
    // TODO categorical  _all_pbs.push_back ( new SteppedCantileverBeamMODDC1 );
    // TODO categorical  _all_pbs.push_back ( new SteppedCantileverBeamMODIC1 );
    // TODO categorical  _all_pbs.push_back ( new SteppedCantileverBeamMODIDC1 );
    
    // More & Wild problems:
    for ( int n_instance = 1 ; n_instance <= 53 ; ++n_instance ) {
        _all_pbs.push_back ( new More_Wild ( n_instance , MW_SMOOTH  ) );
        _all_pbs.push_back ( new More_Wild ( n_instance , MW_NONDIFF ) );
        _all_pbs.push_back ( new More_Wild ( n_instance , MW_WILD3   ) );
        _all_pbs.push_back ( new More_Wild ( n_instance , MW_NOISY3  ) );
    }
    
}

/*----------------------------------*/
/*             destructor           */
/*----------------------------------*/
Runner::~Runner ( void )
{
    
    clear_memory();
    
    // clear list of all problems:
    size_t n = _all_pbs.size();
    for ( size_t k = 0 ; k < n ; ++k )
        delete _all_pbs[k];
    _all_pbs.clear();
}

/*-------------------------------------*/
/*        clear memory (private)       */
/*-------------------------------------*/
void Runner::clear_memory ( void )
{
    
    int i_pb , i_algo;
    
    for ( i_pb = 0 ; i_pb < _n_pb ; ++i_pb )
    {
        if ( _results && _results[i_pb] )
        {
            for ( i_algo = 0 ; i_algo < _n_algo ; ++i_algo )
                if ( _results[i_pb][i_algo] )
                    delete [] _results[i_pb][i_algo];
            delete [] _results [i_pb];
        }
        if ( _test_id && _test_id[i_pb] )
            delete [] _test_id[i_pb];
    }
    if ( _results )
        delete [] _results;
    if ( _test_id )
        delete [] _test_id;
    
    _results = NULL;
    _test_id = NULL;
    
    clear_selected_problems();
    clear_test_configs();
}

/*-----------------------------------*/
/*     create default test config    */
/*-----------------------------------*/
void Runner::create_def_test_config  ( void ) {
    
    Algo_Parameters ap ( "NOMAD" , NOMAD::VERSION );
    ap.set_max_bb_eval ( 1000 , true );
    
    // ap.set_model_search    ( false );
    // ap.set_model_eval_sort ( false );
    
    add_test_config ( ap );
}

/*-------------------------------------------------*/
/*  add a test config to the list of test configs  */
/*  (private)                                      */
/*-------------------------------------------------*/
void Runner::add_test_config ( Algo_Parameters & ap ) {
    ap.set_index ( _n_algo++ );
    _test_configs.push_back ( ap );
}

/*-----------------------------------*/
/*         select all problems       */
/*-----------------------------------*/
void Runner::select_all_problems ( void ) {
    clear_selected_problems();
    size_t n = _all_pbs.size();
    for ( size_t k = 0 ; k < n ; ++k )
        add_selected_problem ( *_all_pbs[k] );
}

/*------------------------------------*/
/*  select one problem with its name  */
/*------------------------------------*/
bool Runner::select_problem_by_name ( std::string name )
{
    NOMAD::toupper ( name );
    size_t n = _all_pbs.size();
    for ( size_t k = 0 ; k < n ; ++k )
        if ( _all_pbs[k]->get_id() == name )
        {
            add_selected_problem ( *_all_pbs[k] );
            return true;
        }
    return false;
}

/*-----------------------------------*/
/*      exclude problem by name      */
/*-----------------------------------*/
bool Runner::exclude_problem_by_name ( std::string name )
{
    
    NOMAD::toupper ( name );
    
    std::list<Problem *> tmp;
    size_t np = _selected_pbs.size();
    
    for ( size_t k = 0 ; k < np ; ++k )
        if ( _selected_pbs[k]->get_id() != name )
            tmp.push_back ( _selected_pbs[k] );
    
    clear_selected_problems();
    
    std::list<Problem *>::const_iterator it , end = tmp.end();
    
    for ( it = tmp.begin() ; it != end ; ++it )
        add_selected_problem ( **it );
    
    return !tmp.empty();
}

/*----------------------------------*/
/*     select problems by size      */
/*----------------------------------*/
bool Runner::select_problems_by_size ( int n_min , int n_max )
{
    
    int    n;
    bool   chk = false;
    size_t np  = _all_pbs.size();
    
    for ( size_t k = 0 ; k < np ; ++k )
    {
        n = _all_pbs[k]->get_n();
        if ( n >= n_min && n <= n_max )
        {
            add_selected_problem ( *_all_pbs[k] );
            chk = true;
        }
    }
    
    return chk;
}

/*-----------------------------------*/
/*     exclude problems by size      */
/*-----------------------------------*/
bool Runner::exclude_problems_by_size ( int n_min , int n_max )
{
    
    std::list<Problem *> tmp;
    
    int    n;
    size_t np = _selected_pbs.size();
    
    for ( size_t k = 0 ; k < np ; ++k )
    {
        n = _selected_pbs[k]->get_n();
        if ( n < n_min || n > n_max )
            tmp.push_back ( _selected_pbs[k] );
    }
    
    clear_selected_problems();
    
    std::list<Problem *>::const_iterator it , end = tmp.end();
    
    for ( it = tmp.begin() ; it != end ; ++it )
        add_selected_problem ( **it );
    
    return !tmp.empty();
}

/*----------------------------------*/
/*  select problems from a keyword  */
/*----------------------------------*/
bool Runner::select_problems_by_keyword ( std::string kw )
{
    NOMAD::toupper ( kw );
    bool chk = false;
    size_t n = _all_pbs.size();
    for ( size_t k = 0 ; k < n ; ++k )
        if ( _all_pbs[k]->has_keyword ( kw ) )
        {
            add_selected_problem ( *_all_pbs[k] );
            chk = true;
        }
    return chk;
}

/*----------------------------------*/
/*  refine problems from a keyword  */
/*----------------------------------*/
bool Runner::refine_problems_by_keyword ( std::string kw )
{
    
    NOMAD::toupper ( kw );
    
    std::list<Problem *> tmp;
    
    size_t n = _selected_pbs.size();
    for ( size_t k = 0 ; k < n ; ++k )
        if ( _selected_pbs[k]->has_keyword ( kw ) )
            tmp.push_back ( _selected_pbs[k] );
    
    clear_selected_problems();
    
    std::list<Problem *>::const_iterator it , end = tmp.end();
    
    for ( it = tmp.begin() ; it != end ; ++it )
        add_selected_problem ( **it );
    
    return !tmp.empty();
}

/*-----------------------------------*/
/*  exclude problems from a keyword  */
/*-----------------------------------*/
bool Runner::exclude_problems_by_keyword ( std::string kw )
{
    
    NOMAD::toupper ( kw );
    
    std::list<Problem *> tmp;
    
    size_t n = _selected_pbs.size();
    for ( size_t k = 0 ; k < n ; ++k )
        if ( !_selected_pbs[k]->has_keyword ( kw ) )
            tmp.push_back ( _selected_pbs[k] );
    
    clear_selected_problems();
    
    std::list<Problem *>::const_iterator it , end = tmp.end();
    
    for ( it = tmp.begin() ; it != end ; ++it )
        add_selected_problem ( **it );
    
    return !tmp.empty();
}

/*--------------------------------------------------*/
/*  add a problem to the list of selected problems  */
/*  (private)                                       */
/*--------------------------------------------------*/
void Runner::add_selected_problem ( Problem & pb )
{
    if ( pb.get_index() < 0 )
    {
        pb.set_index ( _n_pb++ );
        _selected_pbs.push_back ( &pb );
    }
}

/*---------------------------------------*/
/*  clear the list of selected problems  */
/*---------------------------------------*/
void Runner::clear_selected_problems ( void )
{
    size_t n = _selected_pbs.size();
    for ( size_t k = 0 ; k < n ; ++k )
        _selected_pbs[k]->set_index ( -1 );
    _selected_pbs.clear();
    _n_pb = 0;
}

/*----------------------------------*/
/*  clear the list of test configs  */
/*----------------------------------*/
void Runner::clear_test_configs ( void )
{
    _test_configs.clear();
    _n_algo = 0;
}

/*----------------------------------*/
/*                 run              */
/*----------------------------------*/
bool Runner::run ( std::string & error_msg )
{
	
	error_msg.clear();
	
	int          i_pb;
	int          i_algo;
	int          itab[5];
	char         signal;
	MPI::Status  status;
	int          source;
	
	_n_algo = _test_configs.size();
	if ( _n_algo == 0 )
    {
		error_msg = "no test configurations";
		return false;
	}
	
	_n_pb = _selected_pbs.size();
	if ( _n_pb == 0 ) {
		error_msg = "no selected problems";
		return false;
	}
    
    _n_seed_run = sizeof(RUN_SEEDS)/sizeof(RUN_SEEDS[0]);
	int nn = _n_pb * _n_algo * _n_seed_run;

	
	/*----------------------------*/
	/*           master           */
	/*----------------------------*/
	if ( _rank == 0 )
	{
		
		std::ostringstream msg;
		msg << "test execution";
		if ( nn > 1 )
			msg << "s(" << nn << ")";
        
		_out << NOMAD::open_block ( msg.str() );
		
		std::list<int> run_list;
        
		_results = new Result     ** [_n_pb];
		_test_id = new std::string * [_n_pb];
		for ( i_pb = 0 ; i_pb < _n_pb ; ++i_pb )
        {
			_results [i_pb] = NULL;
			_test_id [i_pb] = NULL;
		}
		

        
        
        // 1. check for previously made tests and construct
		//    the list of runs to execute in parallel:
		// ------------------------------------------------
		
		// loop on the problems:
		for ( i_pb = 0 ; i_pb < _n_pb ; ++i_pb )
		{
			
			_results [i_pb] = new Result    * [_n_algo];
			_test_id [i_pb] = new std::string [_n_algo];
            
			// list of existing tests for this problem:
			std::list<std::string> list_of_tests;
			Runner::construct_list_of_subdirs ( list_of_tests , _selected_pbs[i_pb]->get_tests_dir() );
                        
            
			// loop on the algorithm parameters:
			for ( i_algo = 0 ; i_algo < _n_algo ; ++i_algo )
            {
                
				// result:
				_results [i_pb][i_algo] = new Result [ _n_seed_run ];
                
				// check if the run can be avoided:
				bool do_run = true;
				
				std::list<std::string>::const_iterator it , end = list_of_tests.end();
				for ( it = list_of_tests.begin() ; it != end ; ++it )
				{
                    
                    for ( int i_seed = 0 ; i_seed < _n_seed_run ; ++i_seed )
                        _results[i_pb][i_algo][i_seed].reset();
                    
                    
                    if ( Runner::get_results ( *it                      ,
                                              *_selected_pbs [i_pb  ]   ,
                                              _test_configs  [i_algo]   ,
                                              _results       [i_pb][i_algo]    ) )
                    {
                        do_run = false;
                        _test_id[i_pb][i_algo] = *it;
                        break;
                    }

				}
                
				
				// a run will be performed:
				if ( do_run )
                {
                    for ( int i_seed = 0 ; i_seed < _n_seed_run ; ++i_seed )
                        _results[i_pb][i_algo][i_seed].reset();
					
					run_list.push_back ( i_pb   );
					run_list.push_back ( i_algo );
				}
				else
                {
                    set_result ( _test_id       [i_pb][i_algo] ,
                                _results        [i_pb][i_algo],
                                *_selected_pbs  [i_pb  ]       ,
                                _test_configs   [i_algo]         );
				}
				
			} // end of loop on algorithm parameters
			
		} // end of loop on problems
		
		
		// 2. execute all NOMAD runs in parallel:
		// --------------------------------------
		std::list<int>::const_iterator it = run_list.begin() , end = run_list.end();
		while ( it != end )
        {
            
			// problem and algo indexes:
			i_pb   = *it;
			++it;
			i_algo = *it;
			++it;
			/*
			// check that the run to perform corresponds
			// to the current version of NOMAD:
			if ( _test_configs[i_algo].get_solver_name   () != "NOMAD" ||
				_test_configs[i_algo].get_solver_version() !=  NOMAD::VERSION )
            {
				display_instance_name ( i_pb , i_algo );
				_out << ": cannot perform run with solver "
				<< _test_configs[i_algo].get_solver_name();
				if ( _test_configs[i_algo].get_solver_version() != "*" )
					_out << " " << _test_configs[i_algo].get_solver_version();
				_out << std::endl << NOMAD::close_block() << std::endl;
				error_msg = "cannot execute test";
				stop_slaves();
				return false;
			}
			*/
			MPI::COMM_WORLD.Recv ( &signal         ,
								  1               ,
								  MPI::CHAR       ,
								  MPI::ANY_SOURCE ,
								  MPI::ANY_TAG    ,
								  status            );
			
			source = status.Get_source();
			
			// construct test_id:
			time_t t;
			time(&t);
			
			itab[0] = t;
			itab[1] = NOMAD::get_pid() + source;
			itab[2] = i_pb   + 1;
			itab[3] = i_algo + 1;
			itab[4] = _out.get_indent_str().size();
			
			std::ostringstream id;
			id << itab[0] << "_" << itab[1] << "_" << itab[2] << "_" << itab[3];
			
			_test_id[i_pb][i_algo] = id.str();
			
			// send test_id:
			MPI::COMM_WORLD.Rsend ( itab  , 5 , MPI::INT , source , 0 );
		}
		
		// 3. wait and stop the slaves:
		// ----------------------------
		stop_slaves();
		
		// 4. get the new results:
		// -----------------------
		it = run_list.begin() , end = run_list.end();
		while ( it != end )
        {
			
			i_pb   = *it;
			++it;
			i_algo = *it;
			++it;
			
            
			if ( !Runner::get_results ( _test_id       [i_pb][i_algo] ,
									   *_selected_pbs [i_pb]         ,
									   _test_configs  [i_algo]       ,
									   _results      [i_pb][i_algo]   ) )
            {
				display_instance_name ( i_pb , i_algo );
				_out << ": cannot get result " << _test_id[i_pb][i_algo]
				<< std::endl << NOMAD::close_block() << std::endl;
				error_msg = "cannot get result";
                return false;
			}
			
			// set the result:
			set_result ( _test_id       [i_pb][i_algo] ,
						_results        [i_pb][i_algo] ,
						*_selected_pbs  [i_pb  ]       ,
						_test_configs   [i_algo]         );
		}
		
		_out << NOMAD::close_block() << std::endl;
    
        
	}
	
	/*----------------------------*/
	/*           slaves           */
	/*----------------------------*/
	else {
		
		int          i;
		std::string  indent_str , old_indent_str;
		MPI::Request request = MPI::REQUEST_NULL;
		signal               = 0;
		
		while ( true )
        {
			
			// wait for a signal:
			request = MPI::COMM_WORLD.Irecv ( itab  , 5 , MPI::INT , 0 , 0 );
			MPI::COMM_WORLD.Send ( &signal , 1 , MPI::CHAR , 0 , 0 );
			request.Wait();
			
			// indentation string:
			indent_str.clear();
			for ( i = 0 ; i < itab[4] ; ++i )
				indent_str.push_back ( '\t' );
			old_indent_str = _out.get_indent_str();
			_out.set_indent_str ( indent_str );
			
			// STOP signal:
			if ( itab[0] < 0 )
            {
				// _out << "proc ";
				// _out.display_int_w ( _rank , _np );
				// _out << ": STOP" << std::endl;
				_out.set_indent_str ( old_indent_str );
				break;
			}
			
			// RUN signal:
			else
            {
				
				i_pb   = itab[2]-1;
				i_algo = itab[3]-1;
				
				std::ostringstream id;
				id << itab[0] << "_" << itab[1] << "_" << itab[2] << "_" << itab[3];
				
				display_instance_name ( i_pb , i_algo );
				_out << ": proc " << _rank << ": run #" << id.str() << std::endl;
				
				if ( !run_nomad ( nn == _n_seed_run                 ,  // display seed runs if nn==_n_seed_run (a 1 algo and 1 pb)
								 id.str()                ,
								 *_selected_pbs [i_pb  ] ,
								 _test_configs  [i_algo]   ) )
                {
					_out << "proc ";
					_out.display_int_w ( _rank , _np );
					_out << ": problem with at least one seed run" << std::endl;
				}
				else
                {
					_out << "proc ";
					_out.display_int_w ( _rank , _np );
					_out << ": all seed runs OK" << std::endl;
				}
				
				_out.set_indent_str ( old_indent_str );
			}
		}
	}
	return true;
}

/*-----------------------------------------------------*/
/*  check if fx is at alpha % relatively close to fxe  */
/*  (static, private)                                  */
/*-----------------------------------------------------*/
bool Runner::is_within ( const NOMAD::Double & fx    ,
                        const NOMAD::Double & fxe   ,
                        const NOMAD::Double & alpha   ) {
    if ( fxe != 0.0 )
        return ( ( (fx - fxe) / fxe ).abs() <= alpha / 100.0 );
    
    return ( fx.abs() <= alpha / 100.0 );
}

/*---------------------------------------------*/
/*  compute relative error between fx and fxe  */
/*  (static, private)                          */
/*---------------------------------------------*/
NOMAD::Double Runner::compute_alpha ( const NOMAD::Double & fx  ,
                                     const NOMAD::Double & fxe   ) {
    if ( fxe != 0.0 )
        return 100.0 * ( (fx - fxe) / fxe ).abs();
    return fx.abs() * 100.0;
}

/*---------------------------------------*/
/*      display algorithm differences    */
/*---------------------------------------*/
void Runner::display_algo_diff ( void ) const {
    
    if ( _rank != 0 )
        return;
    
    if ( _n_algo == 1 ) {
        _out << "algorithm differences: not displayed for one algorithm"
        << std::endl << std::endl;
        return;
    }
    
    _out << NOMAD::open_block ( "algorithm differences" ) << std::endl;
    
    int i , j , k , cnt;
    
    for ( i = 0 ; i < _n_algo-1 ; ++i )
        for ( j = i+1 ; j < _n_algo ; ++j ) {
            _out << "[algo #";
            _out.display_int_w ( i+1 , _n_algo );
            _out << ", algo #";
            _out.display_int_w ( j+1 , _n_algo );
            _out << "] ";
            cnt = 0;
            for ( k = 0 ; k < _n_pb ; ++k )
                if ( _results[k][i] && _results[k][j] &&
                    *_results[k][i] == *_results[k][j] )
                    ++cnt;
            if ( cnt == _n_pb )
                _out << "identical results";
            else if ( cnt == 0 )
                _out << "differences for all problems";
            else {
                std::ostringstream msg;
                msg << "differences on " << _n_pb - cnt << " problem";
                if ( _n_pb - cnt > 1 )
                    msg << "s";
                msg << " out of " << _n_pb;
                _out.open_block ( msg.str() );
                for ( k = 0 ; k < _n_pb ; ++k )
                    if ( _results[k][i] && _results[k][j] &&
                        !(*_results[k][i] == *_results[k][j]) ) {
                        _out << "pb #";
                        _out.display_int_w ( k , _n_pb );
                        _out << " ["
                        << _selected_pbs[k]->get_id() << "]"
                        << std::endl;
                    }
                _out.close_block();
            }
            _out << std::endl;
        }
    _out << NOMAD::close_block() << std::endl;
}

/*---------------------------------------*/
/*       display performance profiles    */
/*---------------------------------------*/
void Runner::display_perf_prof ( void ) const
{
    
    if ( _rank != 0 )
        return;
    
    std::ofstream fout ( PP_OUT_FILE.c_str() );
    if ( fout.fail() )
    {
        std::cerr << "Warning: cannot create perf. profile output file "
        << PP_OUT_FILE << std::endl;
        return;
    }
    
    _out << "writing of " << PP_OUT_FILE << " ..." << std::flush;
    
    int           i_pb , i_algo , i_seed;
    NOMAD::Double alpha , alpha_max;
    
	// C. Tribes june 2012 : change for including option USE_BEST_EVER_SOL
	// get the best solution for each problem (best ever if : USE_BEST_EVER_SOL=TRUE
	NOMAD::Point  fxe=get_best_fx();
	if (!fxe.is_defined())
	{
		fout << "fail(1)" << std::endl;
		fout.close();
		return;
	}
    
    
    // compute alpha_max and check that best
    // solution and all results are available:
    std::list<int> miss_list;
    for ( i_pb = 0 ; i_pb < _n_pb ; ++i_pb )
    {
        
        for ( i_algo = 0 ; i_algo < _n_algo ; ++i_algo )
        {
            
            if ( !_results[i_pb][i_algo] )
            {
                miss_list.push_back ( i_pb   );
                miss_list.push_back ( i_algo );
            }
            else
            {
                for ( i_seed = 0 ; i_seed < _n_seed_run ; ++i_seed )
                {
                    if ( _results[i_pb][i_algo][i_seed].has_solution() )
                    {
                        if ( USE_BEST_EVER_SOL )
                        {
                            alpha = Runner::compute_alpha ( _results[i_pb][i_algo][i_seed].get_sol_fx() , fxe[i_pb]);
                            if ( !alpha_max.is_defined() || alpha > alpha_max )
                                alpha_max = alpha;
                        }
                    }
                    else
                    {
                        miss_list.push_back ( i_pb   );
                        miss_list.push_back ( i_algo );
                        break;
                    }
                }
            }
        }
    }
    
    if ( USE_BEST_EVER_SOL )
    {
        if ( !alpha_max.is_defined() )
        {
            _out << "... no solution for alpha_max is available" << std::endl;
            fout << "fail(2)" << std::endl;
            fout.close();
            return;
        }
        if ( alpha_max > PP_MAX_ALPHA )
            alpha_max = PP_MAX_ALPHA;
    }
    else
        alpha_max = PP_MAX_ALPHA;
    
    // _out << "alpha_max=" << alpha_max << std::endl;
    
    if ( !miss_list.empty() )
    {
        _out << NOMAD::open_block ( "... the following results are missing" );
        std::list<int>::const_iterator it , end = miss_list.end();
        for ( it = miss_list.begin() ; it != end ; ++it )
        {
            i_pb = *it;
            ++it;
            i_algo = *it;
            display_instance_name ( i_pb , i_algo );
            _out << std::endl;
        }
        _out << NOMAD::close_block() << std::endl;
        fout << "fail(3)" << std::endl;
        fout.close();
        return;
    }
    
	
	
    // header:
    //   fout << std::setw(8) << "alpha";
    //   for ( i_algo = 0 ; i_algo < _n_algo ; ++i_algo ) {
    //     std::ostringstream salgo;
    //     salgo << "algo #" << i_algo+1;
    //     fout << " " << std::setw(8) << salgo.str();
    //   }
    //   fout << std::endl << std::endl;
    
    int           cnt;
    NOMAD::Double dalpha = alpha_max / (PP_NB_LINES-1.0);
    alpha                = 0.0;
    
    for ( int i = 0 ; i < PP_NB_LINES ; ++i )
    {
        
        if ( alpha_max > 1 )
            alpha.display ( fout , "%8.2f" );
        else
            alpha.display ( fout , "%8.2g" );
        
        for ( i_algo = 0 ; i_algo < _n_algo ; ++i_algo )
        {
            cnt = 0;
            for ( i_pb = 0 ; i_pb < _n_pb ; ++i_pb )
                for ( i_seed = 0 ; i_seed < _n_seed_run ; ++i_seed )
                    if ( _results[i_pb][i_algo][i_seed].has_solution() )
                    {
                        
                        // old test:
                        // if ( Runner::is_within ( _results[i_pb][i_algo]->get_sol_fx() ,
                        // 			   _selected_pbs[i_pb]->get_fxe()       ,
                        // 			   alpha                                  ) )
                        //   ++cnt;
                        
                        
                        // C. Tribes june 2012 : change for compactness (fxe accounts for best ever solution if USE_BEST_EVER_SOL is true )
                        // new test:
                        if ( fxe[i_pb].rel_err ( _results[i_pb][i_algo][i_seed].get_sol_fx()  ) <= alpha/100.0 )
                            ++cnt;
                    }
            
            fout << " ";
            NOMAD::Double(100.0*cnt/_n_pb).display ( fout , "%8.2f" );
        }
        fout << std::endl;
        alpha += dalpha;
    }
    
    fout.close();
    
    _out << "... done" << std::endl;
}

/*---------------------------------------*/
/*       display performance profiles    */
/*---------------------------------------*/
void Runner::display_perf_prof_MW ( int tau_exp ) const
{
	
	if ( _rank != 0 || tau_exp <= 0 || _n_pb == 0 || _n_algo == 0 )
		return;
    
	
	std::ostringstream file_name;
	// file_name << DP_OUT_FILE << ".1E-" << tau_exp << ".txt";
	file_name << PP_OUT_FILE << tau_exp << ".txt";

    
	std::ofstream fout ( file_name.str().c_str() );
	if ( fout.fail() )
	{
		std::cerr << "Warning: cannot create performance profile (MW) output file "
		<< file_name.str() << std::endl;
		return;
	}
    
	
	if (  !USE_REL_ERROR_FX0_DP)
	{
		std::cerr << "Warning: cannot create performance profile (MW) if option USE_REL_ERROR_FX0_DP not set to true " << std::endl;
		return;
	}
		
	_out << "writing of " << file_name.str() << " ..." << std::flush;
	
	int i_pb , i_algo, i_seed;
	
	// check that best solution and all results are available:
	std::list<int> miss_list;
    std::list<int> infeas_list;
	for ( i_pb = 0 ; i_pb < _n_pb ; ++i_pb )
	{
		if (!_selected_pbs[i_pb]->get_fxe().is_defined() )
		{
			_out << "... no best known solution defined for problem #" << i_pb
			<< std::endl;
			fout << "fail(1)" << std::endl;
			fout.close();
			return;
		}

        for ( i_algo = 0 ; i_algo < _n_algo ; ++i_algo )
        {
            for ( i_seed = 0 ; i_seed < _n_seed_run ; ++i_seed )
            {
                
                if ( !_results[i_pb][i_algo][i_seed].has_solution()  )
                {
                    miss_list.push_back ( i_pb   );
                    miss_list.push_back ( i_algo );
                    miss_list.push_back ( i_seed );
                    
                    // An infeasible run has no solution -> special flag in miss_list is set
                    if ( _results[i_pb][i_algo][i_seed].is_infeas() )
                        miss_list.push_back( 1 );
                    else
                        miss_list.push_back( 0 );
                    
                    
                }
            }
        }
	}
    
	if ( !miss_list.empty() )
	{
		_out << NOMAD::open_block ( "... the following results are missing" );
		std::list<int>::const_iterator it , end = miss_list.end();
        bool need_for_fix = false;
		for ( it = miss_list.begin() ; it != end ; ++it )
		{
			i_pb = *it;
			++it;
			i_algo = *it;
			display_instance_name ( i_pb , i_algo );
            ++it;
            i_seed = *it;
			_out << " seed run#" << i_seed ;
            ++it;
            if ( *it==1 )
                _out << " --> no feasible point found " << endl;
            else
            {
                _out << " --> run failed, fix it to get data profile " << endl;
                need_for_fix = true;
            }
		}
		_out << NOMAD::close_block() << std::endl;
        
        if ( need_for_fix )
            fout << "fail(2)" << std::endl;
        
		fout.close();

        if ( need_for_fix )
            return;
	}
	
    //return;

	// Get fx0s for all problems
	NOMAD::Point  fx0s=get_fx0s();
	if (USE_REL_ERROR_FX0_DP && !fx0s.is_defined())
	{
		fout << "fail(4)" << std::endl;
		fout.close();
		return;
	}
	
	// get the best solution for each problem:
	NOMAD::Point  fxe=get_best_fx_MW();
	
    
	
	// compute tpsMin and alpha_max (Moré and Wild  2009, eq. 2.1)
	// -------------------------
	const NOMAD::Double tau = pow ( 10.0 , -tau_exp );
	std::vector<int> tpsMin;
	int tpsMinTmp;
	NOMAD::Double alpha_max;
	int bbe_max=get_bbe_max();
	for ( i_pb = 0 ; i_pb < _n_pb ; ++i_pb )
	{
		tpsMinTmp=bbe_max+1;
		for ( i_algo = 0 ; i_algo < _n_algo ; ++i_algo )
		{
            for ( i_seed = 0 ; i_seed < _n_seed_run ; ++i_seed )
            {
                if ( _results[i_pb][i_algo][i_seed].has_solution() )
                {
                    int bbe=1;
                    for ( bbe = 1 ; bbe <= bbe_max ; ++bbe )
                    {
                        if (fx0s[i_pb]-_results[i_pb][i_algo][i_seed].get_sol(bbe) >= (1-tau)*(fx0s[i_pb]-fxe[i_pb]) )
                        {
                            if (bbe<tpsMinTmp)
                            {
                                tpsMinTmp=bbe;
//                                if ( i_pb==32 )
//                                    std::cerr<<"i_pb="<<i_pb<<" i_algo="<<i_algo<<" i_seed="<<i_seed<<" bbe="<<bbe<<" tpsMin="<<tpsMinTmp <<" fx0s[i_pb]="<<fx0s[i_pb]<<" fxe[i_pb]="<<fxe[i_pb]<< std::endl;
                            }
                            break;
                        }
                    }
                    if (!alpha_max.is_defined() ||
                        (tpsMinTmp!=bbe_max+1 && alpha_max < bbe/tpsMinTmp))
                    {
                        alpha_max=bbe/tpsMinTmp;
 
                    }
                }
            }
		}
		tpsMin.push_back(tpsMinTmp);
	}
    
    
	int           cnt;
	NOMAD::Double dalpha = 20 / (PP_NB_LINES-1.0);
    // NOMAD::Double dalpha = alpha_max / (PP_NB_LINES-1.0);
    NOMAD::Double alpha                = 0.0;
	
	for ( int i = 0 ; i < PP_NB_LINES ; ++i )
	{
		
		if ( alpha_max > 1 )
			alpha.display ( fout , "%8.2f" );
		else
			alpha.display ( fout , "%8.2g" );
		
		for ( i_algo = 0 ; i_algo < _n_algo ; ++i_algo )
		{
			cnt = 0;
			for ( i_pb = 0 ; i_pb < _n_pb ; ++i_pb )
                for ( i_seed = 0 ; i_seed < _n_seed_run ; ++i_seed )
                    if ( _results[i_pb][i_algo][i_seed].has_solution() )
                    {
                        
                        for ( int bbe = 1 ; bbe <= bbe_max ; ++bbe )
                        {
                            if (fx0s[i_pb]-_results[i_pb][i_algo][i_seed].get_sol(bbe) >= (1-tau)*(fx0s[i_pb]-fxe[i_pb]) )
                            {
                                if (bbe/tpsMin[i_pb]<=alpha)
                                    cnt++;
                                break;
                            }
                        }
                    }
			
			fout << " ";
            // std::cerr <<"i=" << i << " i_algo=" << i_algo << "--->" << NOMAD::Double(100.0*cnt/(_n_pb*_n_seed_run))<<std::endl;
			NOMAD::Double(100.0*cnt/(_n_pb*_n_seed_run)).display ( fout , "%8.2f" );
		}
		fout << std::endl;
		alpha += dalpha;
	}
	
	
	fout.close();
	
	_out << "... done" << std::endl;
}



/*-------------------------------------------------*/
/*               display data profiles             */
/*-------------------------------------------------*/
void Runner::display_data_prof ( int tau_exp ) const
{
    
    if ( _rank != 0 || tau_exp <= 0 || _n_pb == 0 || _n_algo == 0 )
        return;
    
    std::ostringstream file_name;
    // file_name << DP_OUT_FILE << ".1E-" << tau_exp << ".txt";
    file_name << DP_OUT_FILE << tau_exp << ".txt";
    
    std::ofstream fout ( file_name.str().c_str() );
    if ( fout.fail() ) {
        std::cerr << "Warning: cannot create data profile output file "
        << file_name.str() << std::endl;
        return;
    }
    
    _out << "writing of " << file_name.str() << " ..." << std::flush;
    
    int i_pb , i_algo, i_seed;
    
    // check that best solution and all results are available:
    std::list<int> miss_list;
    for ( i_pb = 0 ; i_pb < _n_pb ; ++i_pb ) {
        
        if ( USE_BEST_EVER_SOL && !_selected_pbs[i_pb]->get_fxe().is_defined() ) {
            _out << "... no best known solution defined for problem #" << i_pb
            << std::endl;
            fout << "fail(1)" << std::endl;
            fout.close();
            return;
        }
        
        for ( i_algo = 0 ; i_algo < _n_algo ; ++i_algo )
        {
            for ( i_seed = 0 ; i_seed < _n_seed_run ; ++i_seed )
            {
                
                if ( !_results[i_pb][i_algo][i_seed].has_solution()  )
                {
                    miss_list.push_back ( i_pb   );
                    miss_list.push_back ( i_algo );
                    miss_list.push_back ( i_seed );
                    
                    // An infeasible run has no solution -> special flag in miss_list is set
                    if ( _results[i_pb][i_algo][i_seed].is_infeas() )
                        miss_list.push_back( 1 );
                    else
                        miss_list.push_back( 0 );
                    
                    
                }
            }
        }
    }
    
    if ( !miss_list.empty() )
    {
        _out << NOMAD::open_block ( "... the following results are missing" );
        std::list<int>::const_iterator it , end = miss_list.end();
        bool need_for_fix = false;
        for ( it = miss_list.begin() ; it != end ; ++it )
        {
            i_pb = *it;
            ++it;
            i_algo = *it;
            display_instance_name ( i_pb , i_algo );
            ++it;
            i_seed = *it;
            _out << " seed run#" << i_seed ;
            ++it;
            if ( *it==1 )
                _out << " --> no feasible point found " << endl;
            else
            {
                _out << " --> run failed, fix it to get data profile " << endl;
                need_for_fix = true;
            }
        }
        _out << NOMAD::close_block() << std::endl;
        
        if ( need_for_fix )
            fout << "fail(2)" << std::endl;
        
        fout.close();
        
        if ( need_for_fix )
            return;
    }

    
    return;
	
	// C. Tribes june 2012 : change for including option USE_REL_ERROR_FX0_DP and USE_BEST_EVER_F
	// Get fx0s for all problems
	NOMAD::Point  fx0s=get_fx0s();
	if (USE_REL_ERROR_FX0_DP && !fx0s.is_defined())
	{
		fout << "fail(4)" << std::endl;
		fout.close();
		return;
	}
    
    // C. Tribes june 2012 : change for including option USE_REL_ERROR_FX0_DP and USE_BEST_EVER_F
    // get the best solution for each problem:
	NOMAD::Point  fxe=get_best_fx();
	
    
    // compute the data profile:
    // -------------------------
    const NOMAD::Double tau = pow ( 10.0 , -tau_exp );
    
    int cnt;
    int bbe_max=get_bbe_max();
    for ( int bbe = 1 ; bbe <= bbe_max ; ++bbe )
    {
        fout << bbe << " ";
        for ( i_algo = 0 ; i_algo < _n_algo ; ++i_algo )
        {
            cnt = 0;
            for ( i_pb = 0 ; i_pb < _n_pb ; ++i_pb )
            {
                for ( i_seed =0 ; i_seed < _n_seed_run ; ++i_seed )
                {
                    // C. Tribes june 2012 : change for including option USE_REL_ERROR_FX0_DP and USE_BEST_EVER_F
                    if (USE_REL_ERROR_FX0_DP && fx0s[i_pb]-_results[i_pb][i_algo][i_seed].get_sol(bbe) >= (1-tau)*(fx0s[i_pb]-fxe[i_pb]) )
                        ++cnt;
                    if (!USE_REL_ERROR_FX0_DP  && _results[i_pb][i_algo][i_seed].get_sol(bbe).rel_err ( fxe[i_pb] ) <= tau )
                        ++cnt;
                    
                }
                
            }
            
            fout << (100.0 * cnt ) / (_n_pb*_n_seed_run) << " ";
        }
        fout << std::endl;
    }
    
    fout.close();
    
    _out << "... done" << std::endl;
}

// C. Tribes new since 2.4
/*-------------------------------------------------*/
/*               display data profiles             */
/* Use Moré and Wild SIAM JOPT 2009 eq 2.9         */
/*-------------------------------------------------*/
void Runner::display_data_prof_MW ( int tau_exp) const
{
	
	if ( _rank != 0 || tau_exp <= 0 || _n_pb == 0 || _n_algo == 0 )
		return;
	
	std::ostringstream file_name;
	// file_name << DP_OUT_FILE << ".1E-" << tau_exp << ".txt";
	file_name << DP_OUT_FILE << tau_exp << ".txt";
	
	std::ofstream fout ( file_name.str().c_str() );
	if ( fout.fail() ) {
		std::cerr << "Warning: cannot create data profile output file "
		<< file_name.str() << std::endl;
		return;
	}
	
	_out << "writing of " << file_name.str() << " ..." << std::flush;
	
	int i_pb , i_algo, i_seed;
	
	// check that best solution and all results are available:
	std::list<int> miss_list;
	for ( i_pb = 0 ; i_pb < _n_pb ; ++i_pb )
	{
        for ( i_algo = 0 ; i_algo < _n_algo ; ++i_algo )
        {
            for ( i_seed = 0 ; i_seed < _n_seed_run ; ++i_seed )
            {
                
                if ( !_results[i_pb][i_algo][i_seed].has_solution()  )
                {
                    miss_list.push_back ( i_pb   );
                    miss_list.push_back ( i_algo );
                    miss_list.push_back ( i_seed );
                    
                    // An infeasible run has no solution -> special flag in miss_list is set
                    if ( _results[i_pb][i_algo][i_seed].is_infeas() )
                        miss_list.push_back( 1 );
                    else
                        miss_list.push_back( 0 );
                    
                    
                }
            }
        }
	}
	
    if ( !miss_list.empty() )
    {
        _out << NOMAD::open_block ( "... the following results are missing" );
        std::list<int>::const_iterator it , end = miss_list.end();
        bool need_for_fix = false;
        for ( it = miss_list.begin() ; it != end ; ++it )
        {
            i_pb = *it;
            ++it;
            i_algo = *it;
            display_instance_name ( i_pb , i_algo );
            ++it;
            i_seed = *it;
            _out << " seed run#" << i_seed ;
            ++it;
            if ( *it==1 )
                _out << " --> no feasible point found " << endl;
            else
            {
                _out << " --> run failed, fix it to get data profile " << endl;
                need_for_fix = true;
            }
        }
        _out << NOMAD::close_block() ;
        
        if ( need_for_fix )
        {
            fout << "fail(2)" << std::endl;
            fout.close();
            return;
        }
    }

    
	// Get fx0s for all problems
	NOMAD::Point  fx0s=get_fx0s();
	if ( !fx0s.is_defined())
	{
		fout << "fail(4)" << std::endl;
		fout.close();
		return;
	}
	
    // _out << fx0s << endl;
    
	// get the best solution for each problem:
	NOMAD::Point  fxe=get_best_fx_MW();
	
	
	// compute the data profile:
	// -------------------------
	const NOMAD::Double tau = pow ( 10.0 , -tau_exp );
	
	int cnt,cnt_sp;
	for ( int alpha = 1 ; alpha <= NB_SIMPLEX_GRAD ; ++alpha )
	{
        //_out << alpha << " ";
		fout << alpha << " ";
		for ( i_algo = 0 ; i_algo < _n_algo ; ++i_algo )
		{
			cnt = 0;
            cnt_sp = 0;
			for ( i_pb = 0 ; i_pb < _n_pb ; ++i_pb )
			{
                int dimPb=_selected_pbs[i_pb]->get_n();
				for ( i_seed = 0 ; i_seed < _n_seed_run ; ++i_seed )
                {
                    //if ( fx0s[i_pb]==NOMAD::INF )
                    //    _out << "algo #"<<i_algo<< ", problem #"<<i_pb<<", seed run#"<<i_seed<<" ---> "<< endl;
                    if (fx0s[i_pb]-_results[i_pb][i_algo][i_seed].get_sol(alpha*(dimPb+1)) >= (1-tau)*(fx0s[i_pb]-fxe[i_pb]) )
                        ++cnt;
                }
                
			}
            fout << (100.0 * cnt ) / (_n_pb*_n_seed_run) << " " ; // << cnt_sp << " ";
            // _out << (100.0 * cnt ) / (_n_pb*_n_seed_run) << " " ;
		}
		fout << std::endl;
        //_out << std::endl;
        
	}
	fout.close();
	
	_out << "... done" << std::endl << std::endl;
    
    
}


// C. Tribes june 2012 add for convenience
/*-------------------------------------------------------*/
/* get the value of x at x0 for all problems (private)   */
/*-------------------------------------------------------*/
NOMAD::Point Runner::get_fx0s() const
{
	NOMAD::Point  fx0s ( _n_pb );
	NOMAD::Double fx0;
	
	for ( int i_pb = 0 ; i_pb < _n_pb ; ++i_pb )
	{
		fx0 = _results[i_pb][0][0].get_sol(1);
		if ( !fx0.is_defined() )
		{
			_out << "... problem with starting point of problem #" << i_pb+1 << std::endl;
			return fx0s;
		}
		fx0s[i_pb] = fx0;
        // _out << "seed #0, algo#0, problem #"<<i_pb << "--> fx0=" << fx0s[i_pb] <<endl;
		for (int  i_algo = 1 ; i_algo < _n_algo ; ++i_algo )
		{
            
            for ( int i_seed=1 ; i_seed < _n_seed_run ; ++i_seed)
            {
                fx0 = _results[i_pb][i_algo][i_seed].get_sol(1);
                
                if ( !fx0.is_defined() || fx0.rel_err ( fx0s[i_pb] ) > 1e-1 )
                {
                    _out << "... inconsistent starting points between seed runs for problem #" << i_pb+1 << " and algo#" << i_algo+1 << " fx0=" << fx0 << " fx0s[ipb]=" << fx0s[i_pb] << std::endl;
                    fx0s.reset();
                    return fx0s;
                }
            }
        }
        
        // fx0 for problems with constraints may not be available (case infeasible initial point -- > NOMAD::INF)
        // fx0--> average of first feasible point obj
        if ( fx0s[i_pb]==NOMAD::INF )
        {
            NOMAD::Double first_fx;
            size_t nb_first_fx = 0;
            fx0s[i_pb]=0.0;
            for (int  i_algo = 1 ; i_algo < _n_algo ; ++i_algo )
            {
                for ( int i_seed=1 ; i_seed < _n_seed_run ; ++i_seed)
                {
                    first_fx = _results[i_pb][i_algo][i_seed].get_first_fx();
                    if ( first_fx != NOMAD::INF && first_fx.is_defined() )
                    {
                        nb_first_fx++;
                        fx0s[i_pb]+=first_fx;
                    }
                }
            }
            if ( nb_first_fx > 0 )
                fx0s[i_pb]/=nb_first_fx;
            else
                fx0s[i_pb]=NOMAD::INF;
            
            // _out << "problem #"<< i_pb+1 << " ---> fx0="<<fx0s[i_pb] <<endl;
        }
        
	}
	return fx0s;
	
}

// C. Tribes june 2012 add for convenience
/*-------------------------------------------------------*/
/* get the best solution for all problems (private)      */
/*-------------------------------------------------------*/
NOMAD::Point Runner::get_best_fx() const
{
	NOMAD::Point fxe( _n_pb );
	NOMAD::Double fxe_tmp,fxe_bb;
	
	int bbe_max=get_bbe_max();
	
	for (int  i_pb = 0 ; i_pb < _n_pb ; ++i_pb )
	{
		if ( USE_BEST_EVER_SOL )
		{
			
			fxe_bb=_selected_pbs[i_pb]->get_fxe();
			if ( !fxe_bb.is_defined() )
			{
				_out << "... no best solution defined for problem #" << i_pb << std::endl;
				fxe.reset();
				return fxe;
			}
		}
		
		for (int i_algo = 0 ; i_algo < _n_algo ; ++i_algo )
		{
			for ( int i_seed = 0 ; i_seed < _n_seed_run ; ++i_seed)
            {
                
                fxe_tmp = _results[i_pb][i_algo][i_seed].get_sol(bbe_max);
                
                if ( fxe_tmp.is_defined() &&
                    ( !fxe[i_pb].is_defined() || fxe_tmp < fxe[i_pb] ) )
                    fxe[i_pb] = fxe_tmp;
            }
		}
		if ( fxe_bb.is_defined() && fxe_bb < fxe[i_pb] )
			fxe[i_pb] = fxe_bb;
	}
	return fxe;
    
}


// C. Tribes june 2012 add for convenience
/*-------------------------------------------------------*/
/* get the best solution for all problems (private)      */
/*-------------------------------------------------------*/
NOMAD::Point Runner::get_best_fx_MW() const
{
	NOMAD::Point fxe( _n_pb );
	NOMAD::Double fxe_tmp,fxe_bb;
	
	for (int  i_pb = 0 ; i_pb < _n_pb ; ++i_pb )
	{
		for (int i_algo = 0 ; i_algo < _n_algo ; ++i_algo )
		{
            for ( int i_seed = 0 ; i_seed < _n_seed_run ; ++i_seed )
            {
                int dimPb=_selected_pbs[i_pb]->get_n();
                fxe_tmp = _results[i_pb][i_algo][i_seed].get_sol(NB_SIMPLEX_GRAD*(dimPb+1));
                
                if ( fxe_tmp.is_defined() &&
                    ( !fxe[i_pb].is_defined() || fxe_tmp < fxe[i_pb] ) )
                    fxe[i_pb] = fxe_tmp;
            }
		}
        
	}
	return fxe;
}


// C. Tribes june 2012 add for convenience
/*----------------------------------------------------------------------*/
/* get the overall maximum of iteration for all problems (private)      */
/*----------------------------------------------------------------------*/
int Runner::get_bbe_max() const
{
	
	int tmp , bbe_max = 0;
	for (int  i_pb = 0 ; i_pb < _n_pb ; ++i_pb )
		for (int  i_algo = 0 ; i_algo < _n_algo ; ++i_algo )
		{
            for ( int i_seed = 0 ; i_seed < _n_seed_run ; ++i_seed )
            {
                tmp = _results[i_pb][i_algo][i_seed].get_sol_bbe();
                if ( tmp > bbe_max )
                    bbe_max = tmp;
            }
		}
	return bbe_max;
}

// C. Tribes june 2012 add for convenience
/*----------------------------------------------------------------------*/
/* get the minimum problems dimension                                   */
/*----------------------------------------------------------------------*/
int Runner::get_dimPbMin() const
{
	
	int tmp , dimMin = _selected_pbs[0]->get_n() ;;
	for (int  i_pb = 1 ; i_pb < _n_pb ; ++i_pb )
	{
        tmp = _selected_pbs[i_pb]->get_n() ;
        if ( tmp < dimMin )
            dimMin = tmp;
	}
	return dimMin;
}



/*-------------------------------------------------*/
/*  display performance profiles (with surf stat)  */
/*-------------------------------------------------*/
void Runner::display_perf_prof_s ( void ) const {
    
    if ( _rank != 0 )
        return;
    
    if ( _n_pb * _n_algo <= 1 )
    {
        _out << "performance profiles (SURF): not created for one instance"
        << std::endl;
        return;
    }
    
    std::ofstream fout ( PPS_OUT_FILE.c_str() );
    if ( fout.fail() )
    {
        std::cerr << "Warning: cannot create surf. perf. profile output file "
        << PPS_OUT_FILE << std::endl;
        return;
    }
    
    _out << "writing of " << PPS_OUT_FILE << " ..." << std::flush;
    
    int           n , i_pb , i_algo , i_seed , max_bb_eval , bbe;
    bool          times_n;
    NOMAD::Double surf , alpha , alpha_max;
    NOMAD::Point  surf_min ( _n_pb );
    
    // compute surf_min, check max_bb_eval and that
    // best solution and all results are available:
    std::list<int> miss_list;
    for ( i_pb = 0 ; i_pb < _n_pb ; ++i_pb )
    {
        
        n   = _selected_pbs[i_pb]->get_n();
        bbe = -10;
        
        for ( i_algo = 0 ; i_algo < _n_algo ; ++i_algo )
        {
            for ( i_seed = 0 ; i_seed < _n_seed_run ; ++i_seed )
            {
                if ( !_results[i_pb][i_algo][i_seed].has_solution() )
                {
                    miss_list.push_back ( i_pb   );
                    miss_list.push_back ( i_algo );
                    break;
                }
                else if ( _results[i_pb][i_algo][i_seed].has_solution() )
                {
                    
                    _test_configs[i_algo].get_max_bb_eval ( max_bb_eval , times_n );
                    if ( times_n )
                        max_bb_eval *= n;
                    
                    if ( bbe == -10 )
                        bbe = max_bb_eval;
                    else if ( bbe != max_bb_eval )
                    {
                        _out << "... problem #" << i_pb+1
                        << ": MAX_BB_EVAL inconsistent for some configs"
                        << std::endl;
                        fout << "fail(1)" << std::endl;
                        fout.close();
                        return;
                    }
                    
                    surf = _results[i_pb][i_algo][i_seed].get_sol_surf();
                    
                    if ( surf.is_defined() &&
                        ( !surf_min[i_pb].is_defined() || surf < surf_min[i_pb] ) )
                        surf_min[i_pb] = surf;
                }
            }
        }
        
        if ( !surf_min[i_pb].is_defined() )
            surf_min[i_pb] = 1.0;
        
        // compute alpha_max:
        for ( i_algo = 0 ; i_algo < _n_algo ; ++i_algo )
        {
            for ( i_seed = 0 ; i_seed < _n_seed_run ; ++i_seed )
            {
                if ( _results[i_pb][i_algo][i_seed].has_solution()    )
                {
                    alpha = Runner::compute_alpha ( _results[i_pb][i_algo][i_seed].get_sol_surf() ,
                                                   surf_min[i_pb] );
                    if ( !alpha_max.is_defined() || alpha > alpha_max )
                        alpha_max = alpha;
                }
            }
        }
    }
    
    if ( alpha_max > PP_MAX_ALPHA )
        alpha_max = PP_MAX_ALPHA;
    
    if ( !miss_list.empty() )
    {
        _out << NOMAD::open_block ( "... the following results are missing" );
        std::list<int>::const_iterator it , end = miss_list.end();
        for ( it = miss_list.begin() ; it != end ; ++it ) {
            i_pb = *it;
            ++it;
            i_algo = *it;
            display_instance_name ( i_pb , i_algo );
            _out << std::endl;
        }
        _out << NOMAD::close_block() << std::endl;
        return;
    }
    
    int           cnt;
    NOMAD::Double dalpha = alpha_max / (PP_NB_LINES-1.0);
    alpha                = 0.0;
    
    for ( int i = 0 ; i < PP_NB_LINES ; ++i )
    {
        
        if ( alpha_max > 1 )
            alpha.display ( fout , "%8.2f" );
        else
            alpha.display ( fout , "%8.2g" );
        
        for ( i_algo = 0 ; i_algo < _n_algo ; ++i_algo )
        {
            cnt = 0;
            for ( i_pb = 0 ; i_pb < _n_pb ; ++i_pb )
                for ( i_seed = 0 ; i_seed < _n_seed_run ; ++i_seed )
                    if ( _results[i_pb][i_algo][i_seed].has_solution() )
                        if ( Runner::is_within ( _results[i_pb][i_algo][i_seed].get_sol_surf() ,
                                                surf_min[i_pb]                         ,
                                                alpha                                    ) )
                            ++cnt;
            fout << " ";
            NOMAD::Double(100.0*cnt/(_n_pb*_n_seed_run)).display ( fout , "%8.2f" );
        }
        fout << std::endl;
        alpha += dalpha;
        
        if ( alpha_max <= 0.0 )
            break;
    }
    
    fout.close();
    
    _out << "... done" << std::endl;
}

/*----------------------------------------------*/
/*  display a summarizing table of the results  */
/*  (LateX tabular)                             */
/*----------------------------------------------*/
void Runner::display_results_table ( void ) const
{
    
    const bool star_for_best = false;
    
    if ( _rank != 0 )
        return;
    
    std::ofstream out ( TABLE_OUT_FILE.c_str() );
    if ( out.fail() ) {
        std::cerr << "Warning: cannot create results output file "
        << TABLE_OUT_FILE << std::endl;
        return;
    }
    
    _out << "writing of " << TABLE_OUT_FILE << " ..." << std::flush;
    
    NOMAD::Double f , surf , best_f , best_surf;
    int           i_pb , i_algo, i_seed;
    size_t        w_name = 0;
    
    for ( i_pb = 0 ; i_pb < _n_pb ; ++i_pb )
        if ( _selected_pbs[i_pb]->get_id(true).size() > w_name )
            w_name = _selected_pbs[i_pb]->get_id(true).size();
    
    out << "\\begin{tabular}{|r|";
    for ( i_algo = 0 ; i_algo < _n_algo ; ++i_algo )
        out << "|rrr";
    out << "|}" << std::endl << "\\hline" << std::endl;
    
    out << "problem";
    if ( _n_algo > 1 )
    {
        for ( i_algo = 0 ; i_algo < _n_algo ; ++i_algo )
            out << " & \\multicolumn{3}{c|}{algo " << i_algo+1 << "}";
        out << "\\\\" << std::endl << "       ";
    }
    for ( i_algo = 0 ; i_algo < _n_algo ; ++i_algo )
        out << " & eval & obj & surf";
    out << " \\\\" << std::endl << "\\hline" << std::endl;
    
    for ( i_pb = 0 ; i_pb < _n_pb ; ++i_pb ) {
        
        out << "  " << std::setw ( w_name )
        << _selected_pbs[i_pb]->get_id(true) << " & ";
        
        best_f.clear();
        best_surf.clear();
        if ( _n_algo > 1 )
            for ( i_algo = 0 ; i_algo < _n_algo ; ++i_algo )
            {
                for ( i_seed =0 ; i_seed < _n_seed_run ; ++i_seed )
                {
                    if ( _results[i_pb][i_algo][i_seed].has_solution() )
                    {
                        f    = _results[i_pb][i_algo][i_seed].get_sol_fx  ();
                        surf = _results[i_pb][i_algo][i_seed].get_sol_surf();
                        if ( !best_f.is_defined() || ( f.is_defined() && f < best_f ) )
                            best_f = f;
                        if ( !best_surf.is_defined()                   ||
                            ( surf.is_defined() && surf < best_surf )    )
                            best_surf = surf;
                    }
                }
            }
        
        for ( i_algo = 0 ; i_algo < _n_algo ; ++i_algo )
        {
            
            if ( i_algo > 0 )
                out << " & ";
            for ( i_seed =0 ; i_seed < _n_seed_run ; ++i_seed )
            {
                if ( _results[i_pb][i_algo][i_seed].has_solution() )
                {
                    
                    f    = _results[i_pb][i_algo][i_seed].get_sol_fx  ();
                    surf = _results[i_pb][i_algo][i_seed].get_sol_surf();
                    
                    out << _results[i_pb][i_algo][i_seed].get_sol_bbe() << " & ";
                    
                    if ( star_for_best )
                    {
                        f.display ( out , "%10.2E" );
                        if ( best_f.is_defined() && f.is_defined() && f == best_f )
                            out << "*";
                        else
                            out << " ";
                    }
                    else {
                        
                        if ( best_f.is_defined() && f.is_defined() && f == best_f ) {
                            out << "{\\bf ";
                            f.display ( out , "%10.2E" );
                            out << "}";
                        }
                        else
                            f.display ( out , "%10.2E" );
                    }
                    
                    out << " & ";
                    
                    if ( star_for_best )
                    {
                        surf.display ( out , "%8.2f" );
                        if ( best_surf.is_defined() && surf.is_defined() && surf == best_surf )
                            out << "*";
                        else
                            out << " ";
                    }
                    else {
                        if ( best_surf.is_defined() && surf.is_defined() && surf == best_surf ) {
                            out << "{\\bf ";
                            surf.display ( out , "%8.2f" );
                            out << "}";
                        }
                        else
                            surf.display ( out , "%8.2f" );
                    }
                }
                else
                    out << "- & - & - ";
            }
            out << " \\\\" << std::endl;
        }
    }
    
    out << "\\hline" << std::endl << "\\end{tabular}" << std::endl;
    
    out.close();
    
    _out << "... done" << std::endl;
}

/*---------------------------------------*/
/*        display selected problems      */
/*---------------------------------------*/
void Runner::display_selected_problems ( void ) const {
    
    if ( _rank != 0 )
        return;
    
    size_t n = _selected_pbs.size();
    
    _out << std::endl;
    
    if ( n == 0 )
        _out << "no problem has been selected" << std::endl;
    else {
        std::ostringstream msg;
        msg << "selected problem";
        if ( n > 1 )
            msg << "s(" << n << ")";
        _out << NOMAD::open_block ( msg.str() );
        display_pbs ( _selected_pbs );
        _out.close_block();
    }
    _out << std::endl;
}

/*---------------------------------------*/
/*           display all problems        */
/*---------------------------------------*/
void Runner::display_all_problems ( void ) const
{
    
    if ( _rank != 0 )
        return;
    
    size_t n = _all_pbs.size();
    
    if ( n == 0 )
        _out << "there is no problem";
    else {
        std::ostringstream msg;
        msg << "all problem";
        if ( n > 1 )
            msg << "s(" << n << ")";
        _out << NOMAD::open_block ( msg.str() );
        display_pbs ( _all_pbs );
        _out << std::endl;
        Problem::display_all_keywords ( _out );
        _out.close_block();
    }
    _out << std::endl;
}

/*---------------------------------------*/
/*       display a list of problems      */
/*       (private)                       */
/*---------------------------------------*/
void Runner::display_pbs ( const std::vector<Problem *> & pbs ) const
{
    size_t k , n = pbs.size() , w_id = 0;
    int    w_batch = 3 , max_n = 0 , max_m = 0;
    
    for ( k = 0 ; k < n ; ++k ) {
        if ( pbs[k]->get_id().size() > w_id )
            w_id = pbs[k]->get_id().size();
        if ( pbs[k]->get_n() > max_n )
            max_n = pbs[k]->get_n();
        if ( pbs[k]->get_m() > max_m )
            max_m = pbs[k]->get_m();
        if ( pbs[k]->is_batch() )
            w_batch = 5;
    }
    
    for ( k = 0 ; k < n ; ++k ) {
        _out << "pb #";
        _out.display_int_w ( k+1 , n );
        _out << ": ";
        pbs[k]->display ( _out , w_id , w_batch , max_n , max_m );
        _out << std::endl;
    }
}

/*------------------------------------------------*/
/*          display instance name (private)       */
/*------------------------------------------------*/
void Runner::display_instance_name ( int i_pb , int i_algo , int i_seed ) const
{
    _out << "pb #";
    _out.display_int_w ( i_pb+1 , _n_pb );
    _out << ", algo #";
    _out.display_int_w ( i_algo+1 , _n_algo );
    if ( i_seed >= 0 )
    {
        _out << ", seed run#";
        _out.display_int_w ( i_seed );
    }
}

/*------------------------------------*/
/*        display test configs        */
/*------------------------------------*/
void Runner::display_test_configs ( void ) const {
    
    if ( _rank != 0 )
        return;
    
    size_t n = _test_configs.size();
    
    _out << std::endl;
    
    if ( n == 0 )
        _out << "there is no test configuration" << std::endl;
    else {
        
        size_t k , w = 0;
        for ( k = 0 ; k < n ; ++k ) {
            if ( _test_configs[k].get_solver_name_version().size() > w )
                w = _test_configs[k].get_solver_name_version().size();
        }
        
        std::ostringstream msg;
        msg << "test configuration";
        if ( n > 1 )
            msg << "s(" << n << ")";
        _out.open_block ( msg.str() );
        for ( k = 0 ; k < n ; ++k ) {
            _out << "algo #";
            _out.display_int_w ( k+1 , n );
            _out << ": ";
            _test_configs[k].display ( _out , w );
            _out << std::endl;
        }
        _out << NOMAD::close_block();
    }
    _out << std::endl;
}

/*---------------------------------------*/
/*      display all available tests      */
/*---------------------------------------*/
void Runner::display_available_tests ( void ) const
{
    
    if ( _rank != 0 )
        return;
    
    std::list<std::string> list_of_pbs , list_of_tests;
    Runner::construct_list_of_subdirs ( list_of_pbs , PROBLEMS_DIR );
    
    if ( list_of_pbs.empty() ) {
        _out << "there is no available test" << std::endl;
        return;
    }
    
    _out << NOMAD::open_block ( "available tests" );
    
    int          n;
    std::string  error_msg;
    std::list<std::string>::const_iterator
    it , it2 , end = list_of_pbs.end() , end2;
    
    size_t max_size;
    
    for ( it = list_of_pbs.begin() ; it != end ; ++it ) {
        
        Runner::construct_list_of_subdirs ( list_of_tests ,
                                           PROBLEMS_DIR + *it + "/" + TESTS_DIR );
        
        n = list_of_tests.size();
        
        if ( n == 0 )
            _out << *it << ": no test has been found" << std::endl;
        
        else {
            
            std::ostringstream msg;
            msg << *it << "(" << n << ")";
            
            _out << NOMAD::open_block ( msg.str() );
            
            max_size = 0;
            end2 = list_of_tests.end();
            for ( it2 = list_of_tests.begin() ; it2 != end2 ; ++it2 )
                if ( it2->size() > max_size )
                    max_size = it2->size();
            
            end2 = list_of_tests.end();
            for ( it2 = list_of_tests.begin() ; it2 != end2 ; ++it2 ) {
                _out << *it2 << std::setw(max_size-it2->size()+1)
                << ":" << " ";
                if ( !check_test_dir ( PROBLEMS_DIR + *it + "/" + TESTS_DIR + *it2 ,
                                      *it                                         ,
                                      true                                        ,
                                      error_msg                                     ) )
                    _out << "invalid: " << error_msg;
                _out << std::endl;
            }
            
            _out.close_block();
        }
    }
    _out << NOMAD::close_block() << std::endl;
}

/*--------------------------------------*/
/*            clear all tests           */
/*--------------------------------------*/
void Runner::clear_all_tests ( void ) const {
    if ( _rank != 0 )
        return;
    std::string cmd;
    std::list<std::string> list_of_pbs;
    Runner::construct_list_of_subdirs ( list_of_pbs , PROBLEMS_DIR );
    std::list<std::string>::const_iterator
    it , end = list_of_pbs.end();
    for ( it = list_of_pbs.begin() ; it != end ; ++it ) {
        cmd = "\\rm -r " + PROBLEMS_DIR + *it + "/" + TESTS_DIR + "* 2> /dev/null";
        system ( cmd.c_str() );
    }
}

/*-----------------------------------------------*/
/*  clear tests corresponding to a given solver  */
/*-----------------------------------------------*/
void Runner::clear_tests_by_solver ( std::string solver_name    ,
                                    std::string solver_version   ) const {
    if ( _rank != 0 )
        return;
    
    NOMAD::toupper ( solver_name    );
    NOMAD::toupper ( solver_version );
    
    std::string            s , cmd , test_dir , name , version;
    std::list<std::string> list_of_pbs , list_of_tests , to_clear;
    
    // 1. construct list of tests to clear:
    Runner::construct_list_of_subdirs ( list_of_pbs , PROBLEMS_DIR );
    
    std::list<std::string>::const_iterator
    it , it2 , end2 , end = list_of_pbs.end();
    
    for ( it = list_of_pbs.begin() ; it != end ; ++it ) {
        
        Runner::construct_list_of_subdirs ( list_of_tests ,
                                           PROBLEMS_DIR+*it+"/"+TESTS_DIR );
        end2 = list_of_tests.end();
        
        for ( it2 = list_of_tests.begin() ; it2 != end2 ; ++it2 ) {
            
            test_dir = PROBLEMS_DIR+*it+"/"+TESTS_DIR+*it2;
            
            // read id file 4/4:
            std::ifstream in ( (test_dir+"/"+ID_FILE).c_str() );
            s.clear();
            while ( !in.eof() && s != "RUNNER" ) {
                in >> s;
                NOMAD::toupper(s);
            }
            
            if ( s == "RUNNER" ) {
                in >> s;
                if ( s == RUNNER_VERSION ) {
                    in >> name >> version;
                    if ( !in.fail() ) {
                        NOMAD::toupper ( name    );
                        NOMAD::toupper ( version );
                        
                        if ( ( solver_name    == "*" || name    == solver_name    ) &&
                            ( solver_version == "*" || version == solver_version )    )
                            to_clear.push_back ( test_dir );
                    }
                }
            }
            in.close();
        }
    }
    
    // 2. clear the tests:
    end = to_clear.end();
    for ( it = to_clear.begin() ; it != end ; ++it ) {
        cmd = "\\rm -r " + *it + " 2> /dev/null";
        system ( cmd.c_str() );
    }
}

/*-----------------------------------------------*/
/*  clear tests corresponding to a given solver  */
/*-----------------------------------------------*/
void Runner::clear_tests_by_config_and_selected_pbs ( std::string solver_name    ,
                                                     std::string solver_version ) const
{
    if ( _rank != 0 )
        return;
    
    NOMAD::toupper ( solver_name    );
    NOMAD::toupper ( solver_version );
    
    
    std::string            s , cmd , test_dir , name , version;
    std::list<std::string> list_of_pbs , list_of_tests , to_clear;
    
    std::vector<Problem *>::const_iterator it_selected_pbs;
    std::vector<Algo_Parameters>::const_iterator it_test_configs;
    
    
    std::list<std::string>::const_iterator
    it , it2 , end2 , end = list_of_pbs.end();
    
    std::string current_pb_dir;
    
    for ( it_selected_pbs = _selected_pbs.begin() ; it_selected_pbs!=_selected_pbs.end() ; ++it_selected_pbs )
    {

        if ( current_pb_dir.compare((*it_selected_pbs)->get_pb_dir())==0 )
            continue;
        
        current_pb_dir = (*it_selected_pbs)->get_pb_dir();
    
        Runner::construct_list_of_subdirs ( list_of_tests , current_pb_dir+"/"+TESTS_DIR );
        
        end2 = list_of_tests.end();
    
        
        cout << "Number of tests for pb dir("<<current_pb_dir<<"): "<< list_of_tests.size() <<endl;
        
        for ( it2 = list_of_tests.begin() ; it2 != end2 ; ++it2 )
        {
            
            test_dir = (*it_selected_pbs)->get_pb_dir()+"/"+TESTS_DIR+*it2;
            

            // Modif C.Tribes --> fix linux compilation error
            // Before
            // read id file 4/4:
            // std::ifstream in ( (test_dir+"/"+ID_FILE+"."+std::to_string( RUN_SEEDS[0] )).c_str() );
            // After
            stringstream ss;
            ss << test_dir << "/" << ID_FILE << "." <<  RUN_SEEDS[0] ;
            std::ifstream in ( ss.str().c_str() );
            
            
            Algo_Parameters ap(solver_name,solver_version);
            
            s.clear();
            while ( !in.eof() && s != "RUNNER" )
            {
                in >> s;
                NOMAD::toupper(s);
            }
            
            if ( s == "RUNNER" )
            {
                in >> s;
                if ( s == RUNNER_VERSION )
                {
                    in >> name >> version;
                    
                    if ( !in.fail() )
                    {
                        NOMAD::toupper ( name    );
                        NOMAD::toupper ( version );
                        
                        // cout << name << " " << version << endl;
                        
                        if ( ( solver_name    == "*" || name    == solver_name    ) &&
                            ( solver_version == "*" || version == solver_version )    )
                        {
                            
                            // get the line of comments:
                            std::string description;
                            getline ( in , s );
                            getline ( in , s );
                            getline ( in , description );
                            
                            if ( in.fail() )
                                description.clear();
                            
                            // get and check max_bb_eval:
                            int  max_bb_eval;
                            bool times_n;
                            if ( ! read_max_bb_eval ( in , max_bb_eval , times_n ) )
                            {
                                max_bb_eval = -1;
                                times_n     = false;
                            }
                            
                            // get and check non-default x0:
                            NOMAD::Point x0;
                            if ( ! read_x0 ( in , x0 ) ||
                                ( x0.size() > 0 && !x0.is_complete() ) )
                                x0.clear();
                            // Modif C.Tribes --> fix linux compilation error
                            // Before
                            //if ( ! ap.read_id(in) )
                            //    cout << "Read " << test_dir+"/"+ID_FILE+"."+std::to_string( RUN_SEEDS[0] ) << " not ok" << endl;
                            // After
                            if ( ! ap.read_id(in) )
                            {
                                stringstream ss;
                                ss << "Read " << test_dir << "/" << ID_FILE << "." << RUN_SEEDS[0] << " not ok" ;
                                cout << ss.str() << endl;
                                
                            }
                            
                            
                            // Test if read id file corresponds to one of the selected configs
                            for ( it_test_configs = _test_configs.begin() ; it_test_configs != _test_configs.end() ; ++it_test_configs )
                            {
                                
                                if ( ap.is_compatible( (*it_test_configs), *(*it_selected_pbs) ) )
                                {
                                    to_clear.push_back(test_dir);
                                    break;
                                }
                            }

                        }
                    }
                }
            }
            in.close();

        }
    }
    
    
    // 2. clear the tests:
    end = to_clear.end();
    cout << "Number of tests to clear:" << to_clear.size() <<endl;
    for ( it = to_clear.begin() ; it != end ; ++it )
    {
        cmd = "\\rm -r " + *it + " 2> /dev/null";
        system ( cmd.c_str() );
        // cout << "Cleared " << (*it) <<endl;

    }
}


/*--------------------------------------*/
/*  clear invalid test sub-directories  */
/*--------------------------------------*/
void Runner::clear_invalid_tests ( void ) const {
    
    if ( _rank != 0 )
        return;
    
    std::string error_msg , test_dir , cmd;
    
    std::list<std::string> list_of_pbs , list_of_tests , to_clear;
    Runner::construct_list_of_subdirs ( list_of_pbs , PROBLEMS_DIR );
    
    std::list<std::string>::const_iterator
    it , it2 , end = list_of_pbs.end() , end2;
    
    // 1. construct list of tests to clear:
    for ( it = list_of_pbs.begin() ; it != end ; ++it ) {
        
        Runner::construct_list_of_subdirs ( list_of_tests ,
                                           PROBLEMS_DIR + *it + "/" + TESTS_DIR );
        
        end2 = list_of_tests.end();
        for ( it2 = list_of_tests.begin() ; it2 != end2 ; ++it2 ) {
            
            test_dir = PROBLEMS_DIR + *it + "/" + TESTS_DIR + *it2;
            
            if ( !check_test_dir ( test_dir  ,
                                  *it       ,
                                  false     ,
                                  error_msg   ) )
                to_clear.push_back ( test_dir );
        }
    }
    
    // 2. clear the tests:
    end = to_clear.end();
    for ( it = to_clear.begin() ; it != end ; ++it ) {
        cmd = "\\rm -r " + *it + " 2> /dev/null";
        system ( cmd.c_str() );
    }
}

/*---------------------------------------*/
/*  construct a list of sub-directories  */
/*  (static, private)                    */
/*---------------------------------------*/
bool Runner::construct_list_of_subdirs ( std::list<std::string> & list_of_dirs ,
                                        const std::string      & directory      )
{
    
    
    if ( ! list_of_dirs.empty() )
       list_of_dirs.clear();
    
    
    
    std::string s , cmd = "ls " + directory + " > " + TMP_FILE;

        
    system ( cmd.c_str() );
    
    std::ifstream fin ( TMP_FILE.c_str() );
    
    while ( !fin.eof() )
    {
        fin >> s >> std::ws;
        if ( !s.empty() )
            list_of_dirs.push_back ( s );
    }
    
    fin.close();
    remove ( TMP_FILE.c_str() );
    
    if ( fin.fail() )
    {
        list_of_dirs.clear();
        return false;
    }
    return true;
}

/*------------------------------------------------*/
/*              set a result (private)            */
/*------------------------------------------------*/
void Runner::set_result (const std::string     & test_id ,
                         Result                  result[],
                         Problem               & pb      ,
                         const Algo_Parameters & ap        ) const
{
    
    // result.display ( _out );
    
    int  i_pb   = pb.get_index();
    int  i_algo = ap.get_index();
    bool nomad  = ap.is_nomad();
    int  bbe;
    bool times_n;
    ap.get_max_bb_eval ( bbe , times_n );
    if ( times_n )
        bbe *= pb.get_n();
    
    // bbe and fx correspond to the desired max number of bb evaluations,
    //  and not necessarily to the last entry in the stats file, while
    //  xe and fxe  correspond to the best solution (last entry in the
    //  stats file).
    
    for ( int i_seed=0 ; i_seed < _n_seed_run ; ++i_seed )
    {
        display_instance_name ( i_pb , i_algo , i_seed );
        _out << ": found in " << Runner::get_test_dir ( test_id , pb ) << ": ";
        
        if ( result[i_seed].compute_solution ( pb.get_n   () ,
                                              pb.get_f_lb() ,
                                              bbe           ,
                                              nomad           ) )
        {
            _out << "bbe="   << result[i_seed].get_sol_bbe ()
            << " f="    << result[i_seed].get_sol_fx  ()
            << " fx0=" << result[i_seed].get_sol(1)
            << " ffx=" << result[i_seed].get_first_fx();
            //<< " surf=" << result[i_seed].get_sol_surf();
            if ( nomad && pb.update_xe ( result[i_seed].get_sol_xe() , result[i_seed].get_sol_fxe() ) )
                _out << " (new best solution)";
            _out << std::endl;
        }
        else
            _out << "no solution" << std::endl;
    }
}

/*-------------------------------------------------*/
/*  read max_bb_eval in ID file (static, private)  */
/*-------------------------------------------------*/
bool Runner::read_max_bb_eval ( std::ifstream & in          ,
                               int           & max_bb_eval ,
                               bool          & times_n       ) {
    max_bb_eval = -1;
    times_n     = false;
    
    std::string s;
    
    in >> s;
    getline ( in , s );
    
    if ( in.fail() )
        return false;
    
    s.erase ( s.begin() );
    
    size_t nm1 = s.size() - 1;
    if ( toupper ( s[nm1] ) == 'N' ) {
        s.resize ( nm1 );
        times_n = true;
    }
    
    NOMAD::Double bbe;
    
    if ( !bbe.atof ( s ) || !bbe.is_defined() )
        return false;
    
    max_bb_eval = bbe.round();
    
    if ( max_bb_eval == -1 )
        times_n = false;
    
    return true;
}

/*----------------------------------------*/
/*  read x0 in ID file (static, private)  */
/*----------------------------------------*/
bool Runner::read_x0 ( std::ifstream & in , NOMAD::Point & x0 ) {
    
    x0.clear();
    
    std::string s , sx0;
    in >> s;
    getline ( in , sx0 );
    
    if ( in.fail() || sx0.empty() )
        return false;
    
    if ( sx0 == " -" )
        return true;
    
    std::list<std::string> words;
    NOMAD::get_words ( sx0 , words );
    
    x0.reset ( words.size() );
    
    int i = 0;
    std::list<std::string>::const_iterator it , end = words.end();
    for ( it = words.begin() ; it != end ; ++it , ++i ) {
        if ( !x0[i].atof ( *it ) || !x0[i].is_defined() ) {
            x0.clear();
            return false;
        }
    }
    return true;
}

/*-------------------------------------------------------*/
/*               read and add config test file           */
/*                   (read id file 1/4)                  */
/*-------------------------------------------------------*/
bool Runner::read_test_config ( const std::string & config_file_name ,
                               std::string       & error_msg          ) {
    
    error_msg.clear();
    
    std::ifstream fin ( config_file_name.c_str() );
    
    if ( fin.fail() ) {
        fin.close();
        error_msg = "cannot read file " + config_file_name;
        return false;
    }
    
    std::string s , runner_version;
    
    while ( s != "RUNNER" && !fin.eof() ) {
        
        fin >> s;
        NOMAD::toupper(s);
        
        if ( fin.fail() ) {
            fin.close();
            error_msg = "error(1) in file " + config_file_name;
            return false;
        }
    }
    
    fin >> runner_version;
    if ( fin.fail() ) {
        fin.close();
        error_msg = "error(2) in file " + config_file_name;
        return false;
    }
    
    if ( runner_version != RUNNER_VERSION ) {
        fin.close();
        error_msg = "error(3) in file " + config_file_name;
        return false;
    }
    
    std::string file_solver_name , file_solver_version;
    fin >> file_solver_name >> file_solver_version;
    
    if ( fin.fail() ) {
        fin.close();
        error_msg = "error(4) in file " + config_file_name;
        return false;
    }
    
    // create the algorithm parameters from the id file:
    Algo_Parameters ap ( file_solver_name , file_solver_version );
    
    // get the line of comments:
    std::string description;
    getline ( fin , s );
    getline ( fin , s );
    getline ( fin , description );
    
    if ( fin.fail() )
        description.clear();
    
    // get and check max_bb_eval:
    int  max_bb_eval;
    bool times_n;
    if ( !Runner::read_max_bb_eval ( fin , max_bb_eval , times_n ) ) {
        max_bb_eval = -1;
        times_n     = false;
    }
    
    // get and check non-default x0:
    NOMAD::Point x0;
    if ( !Runner::read_x0 ( fin , x0 ) ||
        ( x0.size() > 0 && !x0.is_complete() ) )
        x0.clear();
    
    // other parameters:
    if ( file_solver_name == "NOMAD" ) {
        
        if ( !ap.read_id ( fin ) || fin.fail() ) {
            fin.close();
            error_msg = "error(5) in file " + config_file_name;
            return false;
        }
    }
    
    fin.close();
    
    if ( !description.empty() )
        ap.set_description ( description );
    
    if ( max_bb_eval > 0 )
        ap.set_max_bb_eval ( max_bb_eval , times_n );
    
    if ( x0.is_defined() )
        ap.set_x0 ( x0 );
    
    
    add_test_config ( ap );
    
    return true;
}

/*-------------------------------------*/
/*        check a test directory       */
/*        (private)                    */
/*        (read id file 2/4)           */
/*-------------------------------------*/
bool Runner::check_test_dir ( const std::string & test_dir  ,
                             const std::string & pb_dir    ,
                             bool                display   ,
                             std::string       & error_msg   ) const {
    error_msg.clear();
    
    // 1. check id.#seed.txt:
    // ----------------
    
    std::ifstream fin ( ( test_dir + "/" + ID_FILE ).c_str() );
    
    if ( fin.fail() ) {
        fin.close();
        error_msg = "cannot read " + ID_FILE;
        return false;
    }
    
    std::string s , runner_version , problem_id;
    getline ( fin , s ); // date
    fin >> problem_id;
    
    if ( problem_id != pb_dir ) {
        fin.close();
        error_msg = ID_FILE + ": wrong problem id";
        return false;
    }
    
    Problem * pb = find_problem ( problem_id );
    if ( !pb && display )
        _out << "[unknown problem] ";
    
    fin >> s >> runner_version;
    if ( fin.fail() || s != "RUNNER" || runner_version != RUNNER_VERSION ) {
        fin.close();
        error_msg = ID_FILE + ": wrong runner version";
        return false;
    }
    
    std::string file_solver_name , file_solver_version;
    fin >> file_solver_name >> file_solver_version;
    
    if ( display )
        _out << "[" << file_solver_name
        << " " << file_solver_version << "] ";
    
    // get the line of comments:
    getline ( fin , s );
    getline ( fin , s );
    getline ( fin , s );
    
    if ( fin.fail() ) {
        fin.close();
        error_msg = ID_FILE + ": problem with comments";
        return false;
    }
    
    // get and check max_bb_eval:
    int  file_max_bb_eval;
    bool times_n;
    if ( !Runner::read_max_bb_eval ( fin , file_max_bb_eval , times_n ) ) {
        fin.close();
        error_msg = ID_FILE + ": problem with MAX_BB_EVAL";
        return false;
    }
    
    // get and check non-default x0:
    NOMAD::Point file_x0;
    if ( !Runner::read_x0 ( fin , file_x0 ) ||
        ( file_x0.size() > 0 && !file_x0.is_complete() ) ) {
        fin.close();
        error_msg = ID_FILE + ": problem with non-default x0";
        return false;
    }
    
    if ( file_solver_name == "NOMAD" ) {
        
        // create temporary algorithm parameters from the id file:
        Algo_Parameters ap_tmp ( "NOMAD" , file_solver_version );
        
        if ( !ap_tmp.read_id ( fin ) || fin.fail() ) {
            fin.close();
            error_msg = ID_FILE + ": problem with parameters";
            return false;
        }
    }
    
    fin.close();
    
    // 2. check the stats file:
    // ------------------------
    
    fin.open ( ( test_dir + "/" + STATS_FILE ).c_str() );
    
    if ( fin.fail() ) {
        fin.close();
        error_msg = "cannot read " + STATS_FILE;
        return false;
    }
    
    Result result;
    
    if ( !result.read ( fin , -1 , true ) || fin.fail() ) {
        fin.close();
        
        std::ifstream fin2 ( ( test_dir + "/" + STATS_FILE ).c_str() );
        if ( !result.read ( fin2 , -1 , false ) || fin2.fail() ) {
            fin2.close();
            error_msg = "problem with " + STATS_FILE;
            return false;
        }
        
        fin2.close();
    }
    
    fin.close();
    
    if ( pb && display ) {
        
        int bbe = -1;
        
        if ( result.compute_solution ( pb->get_n   () ,
                                      pb->get_f_lb() ,
                                      bbe            ,
                                      true             ) )
            _out << "[bbe=" << result.get_sol_bbe()
            << " f=" << result.get_sol_fx() << "]";
        else {
            
            bbe = -1;
            
            if ( result.compute_solution ( pb->get_n   () ,
                                          pb->get_f_lb() ,
                                          bbe            ,
                                          false             ) )
                _out << "[bbe=" << result.get_sol_bbe()
                << " f=" << result.get_sol_fx() << "]";
            
            else
                _out << "[no solution]";
        }
    }
    return true;
}

/*-------------------------------------*/
/*  get the results (static, private)  */
/*    (read id file 3/4)               */
/*-------------------------------------*/
bool Runner::get_results ( const std::string     & test_id ,
                          const Problem         & pb      ,
                          const Algo_Parameters & ap      ,
                          Result                  result[])
{
   

    int n_seed_run= sizeof(RUN_SEEDS)/sizeof(RUN_SEEDS[0]);
    
    for ( int i_seed = 0 ; i_seed < n_seed_run ; ++i_seed )
    {
        
      
        std::ifstream fin ( Runner::get_id_file_name ( test_id , pb , RUN_SEEDS[i_seed] ).c_str() );
        
        if ( fin.fail() )
        {
            fin.close();
            return false;
        }
        
        std::string s , pb_id;
        getline ( fin , s ); // date
        fin >> pb_id;
        
        if ( fin.fail() || pb_id != pb.get_id() )
        {
            fin.close();
            return false;
        }
        
        fin >> s;
        if ( fin.fail() || s != "RUNNER" )
        {
            fin.close();
            return false;
        }
        
        fin >> s;
        if ( fin.fail() || s != RUNNER_VERSION )
        {
            fin.close();
            return false;
        }
        
        std::string file_solver_name , file_solver_version;
        
        fin >> file_solver_name >> file_solver_version;
        
        if ( fin.fail() || file_solver_name != ap.get_solver_name() )
        {
            fin.close();
            return false;
        }
        
        if ( ap.get_solver_version() != "*" &&
            file_solver_version != ap.get_solver_version() )
        {
            fin.close();
            return false;
        }
        
        
        // at this point:
        //   ap.solver_name    == file_solver_name
        //   ap.solver_version == file_solver_version
        
        // get the line of comments:
        getline ( fin , s );
        getline ( fin , s );
        getline ( fin , s );
        
        int n = pb.get_n();
        
        // get max_bb_eval (it's then checked at the end of the function):
        int  file_max_bb_eval;
        bool times_n;
        
        if ( !Runner::read_max_bb_eval ( fin , file_max_bb_eval , times_n ) )
        {
            fin.close();
            return false;
        }
        
        if ( times_n )
            file_max_bb_eval *= n;
        
        // get and check non-default x0:
        NOMAD::Point file_x0;
        if ( !Runner::read_x0 ( fin , file_x0 ) )
        {
            fin.close();
            return false;
        }
        
        
        if ( file_x0.size() > 0 &&
            ( !file_x0.is_complete() || file_x0.size() != n ) )
        {
            fin.close();
            return false;
        }
        
        const NOMAD::Point pb_x0 = pb.get_x0();
        const NOMAD::Point ap_x0 = ap.get_x0();
        
        
        if ( ap_x0.size() > 0 || file_x0.size() > 0 )
        {
            
            if ( ap_x0.size() == n && ap_x0.is_complete() )
            {
                if ( file_x0.size() == n )
                {
                    if ( ap_x0 != file_x0 )
                    {
                        fin.close();
                        return false;
                    }
                }
                else if ( ap_x0 != pb_x0 )
                {
                    fin.close();
                    return false;
                }
            }
            
            else if ( file_x0.size() == n && pb_x0 != file_x0 )
            {
                fin.close();
                return false;
            }
        }
        
        
        // if the solver of interest is not NOMAD, parameters
        // are not checked (except MAX_BB_EVAL) and any directory
        // with the good solver and the good version is considered:
        if ( file_solver_name == "NOMAD" )
        {
            
            // at this point:
            //   ap.solver_name    == file_solver_name    == "NOMAD"
            //   ap.solver_version == file_solver_version =?  NOMAD::VERSION
            
            // create temporary algorithm parameters from the id file:
            Algo_Parameters ap_tmp ( "NOMAD" , file_solver_version );
            
            if ( !ap_tmp.read_id ( fin ) || fin.fail() )
            {
                fin.close();
                return false;
            }
                        
            ap_tmp.set_max_bb_eval ( file_max_bb_eval , false );
            
            // check the compatibility between ap and ap_tmp:
            if ( !ap.is_compatible ( ap_tmp , pb ) )
            {
                fin.close();
                return false;
            }
            
            
        }
        
        
        fin.close();
        
        int ap_max_bb_eval;
        ap.get_max_bb_eval ( ap_max_bb_eval , times_n );
        if ( times_n )
            ap_max_bb_eval *= n;
        
        
        // check the results (stats file):
        fin.open ( Runner::get_stats_file_name ( test_id , pb , RUN_SEEDS[i_seed] ).c_str() );
        
        if ( fin.fail() )
        {
            fin.close();
            result[i_seed].reset();
            return false;
        }
        else if ( !result[i_seed].read ( fin , ap_max_bb_eval , ap.is_nomad()) )
        {
            fin.close();
            return false;
        }
        fin.close();
        
     
        int res_bbe = result[i_seed].get_last_bbe();
        
        //        std::cout << "I_SEED=" << i_seed << std::endl;
        //        std::cout << "AP_MAX_BB_EVAL   = " << ap_max_bb_eval << std::endl;
        //        std::cout << "FILE_MAX_BB_EVAL = " << file_max_bb_eval << std::endl;
        //        std::cout << "RES_BBE          = " << res_bbe << std::endl;
        //        << std::endl;
        
        
        if ( file_max_bb_eval > 0 && ap_max_bb_eval <= 0 && res_bbe < ap_max_bb_eval && file_max_bb_eval < ap_max_bb_eval && res_bbe > file_max_bb_eval )
        {
            result[i_seed].reset();
            return false;
        }
        
        if ( res_bbe <= 0 || res_bbe == file_max_bb_eval )
        {
            result[i_seed].reset();
            return false;
        }
        
    }
    
    return true;
}

/*----------------------------------------------*/
/*  find a problem in the list of all problems  */
/*  (private)                                   */
/*----------------------------------------------*/
Problem * Runner::find_problem ( const std::string & problem_id ) const {
    size_t n = _all_pbs.size();
    for ( size_t k = 0 ; k < n ; ++k )
        if ( _all_pbs[k]->get_id() == problem_id )
            return _all_pbs[k];
    return NULL;
}

/*-----------------------------------*/
/*          stop the slaves          */
/*          (private)                */
/*-----------------------------------*/
void Runner::stop_slaves ( void ) const {
    
    MPI::Status  status;
    char         signal;
    int          itab[5];
    int          source;
    int          nb_stops = 0;
    
    while ( nb_stops < _np - 1 ) {
        MPI::COMM_WORLD.Recv ( &signal         ,
                              1               ,
                              MPI::CHAR       ,
                              MPI::ANY_SOURCE ,
                              MPI::ANY_TAG    ,
                              status            );
        source = status.Get_source();
        itab[0] = itab[1] = itab[2] = itab[3] = -1;
        itab[4] = _out.get_indent_str().size();
        MPI::COMM_WORLD.Rsend (  itab  , 5 , MPI::INT , source , 0 );
        ++nb_stops;
    }
}

/*-------------------------------------*/
/*         run NOMAD (private)         */
/*-------------------------------------*/
bool Runner::run_nomad ( bool                display ,
                        const std::string & test_id ,
                        Problem           & pb      ,
                        Algo_Parameters   & ap        ) const
{
    
    // create output directory:
    std::string cmd = "mkdir " + Runner::get_test_dir ( test_id , pb );
    
    if ( system ( cmd.c_str() ) )
    {
        std::cerr << "cannot create directory "
        << Runner::get_test_dir ( test_id , pb )
        << ": abort" << std::endl;
        return false;
    }
    
    for ( int i_seed=0 ; i_seed < _n_seed_run ; ++i_seed )
    {
        
        
        // create id file:
        std::ofstream fout ( Runner::get_id_file_name ( test_id , pb , RUN_SEEDS[i_seed] ).c_str() );
        
        fout << get_date()  << std::endl
        << pb.get_id() << std::endl
        << "RUNNER " << RUNNER_VERSION << std::endl
        << "NOMAD "  << NOMAD::VERSION << std::endl << std::endl;
        ap.display_id ( fout );
        
        fout.close();
        
        if ( fout.fail() )
        {
            std::cerr << "cannot create id file "
            << Runner::get_id_file_name ( test_id , pb , RUN_SEEDS[i_seed] )
            << ": abort" << std::endl;
            return false;
        }
        
        // C.Tribes nov 05, 2013 --- added to make sure that evaluation numbers are reset
        NOMAD::Eval_Point::reset_tags_and_bbes();

// C.Tribes oct 1, 2014 --- not needed with new set_SEED that reset seed everytime
//        // C.Tribes jan 29, 2014 --- added to reset the random number generator for each mads run.
//        // The seed will always be to its default value
//        NOMAD::RNG::reset_seed_to_default();
//        
//        // C.Tribes feb 6, 2014 --- added to make run with seed change
//        NOMAD::RNG::set_seed(RUN_SEEDS[i_seed]);
//        
        
        // NOMAD parameters:
        NOMAD::Parameters *p =new NOMAD::Parameters ( _out );
        
        ap.set_parameters ( pb                                                              ,
                           Runner::get_stats_file_name ( test_id , pb , RUN_SEEDS[i_seed] ) ,
                           display                                                          ,
                           *p                                                               );
        
        // C.Tribes TMP for running Hybride 3.6.2 Halton seed
        // p->set_HALTON_SEED(RUN_SEEDS[i_seed]);
        
        p->set_SEED(RUN_SEEDS[i_seed]);
        
        NOMAD::stop_type   stop_reason;
        NOMAD::Model_Stats ms;
        
        try {
            
            p->check();
            
            // custom evaluator creation:
            Custom_Evaluator ev ( *p , pb );
            
            // C.Tribes NOT WORKING WITH CURRENT DESIGN OF RUNNER
            // custom extended poll
            // Custom_Extended_Poll ep ( *p , pb );
            
            // algorithm creation and execution:
            NOMAD::Mads mads ( *p , &ev );
            
            stop_reason = mads.run();
            
            // save model stats:
            ms = mads.get_stats().get_model_stats();
            
            // C.Tribes nov 05, 2013 --- Make sure to clear the caches
            mads.get_evaluator_control().get_cache().clear();
            
        }
        catch ( std::exception & e )
        {
            std::cerr << std::endl << "NOMAD has been interrupted: "
            << e.what() << std::endl;
            return false;
        }
        
        // write the 'RUN OK' flag:
        if ( stop_reason != NOMAD::ERROR &&
            stop_reason != NOMAD::CTRL_C &&
            stop_reason != NOMAD::USER_STOPPED &&
            stop_reason != NOMAD::X0_FAIL &&
            stop_reason != NOMAD::MAX_CACHE_MEMORY_REACHED )
        {
            
            if (stop_reason == NOMAD::UNKNOWN_STOP_REASON)
                std::cerr << "Warning: Mads stop reason unknown. Let us try anyway" << std::cerr;
            
            fout.open ( Runner::get_id_file_name ( test_id , pb , RUN_SEEDS[i_seed] ).c_str() , std::ios::app );
            
            if ( !fout.fail() )
                fout << std::endl << "RUN SEED #" << i_seed << " OK" << std::endl;
            
            fout.close();
            
            // write model stats file:
            if ( ap.use_models() )
            {
                
                std::string model_stats_file
                = Runner::get_test_dir ( test_id , pb ) + MODEL_STATS_FILE + static_cast<ostringstream*>( &(ostringstream() << i_seed ) )->str() ;
                std::ofstream fout2 ( model_stats_file.c_str() );
                
                if ( fout2.fail() )
                    std::cerr << "Warning: cannot create model stats file "
                    << model_stats_file << std::endl;
                else
                {
                    fout2 << "1. n         = " << p->get_dimension()         << std::endl
                    << "2. nb_models = " << ms.get_nb_models()        << std::endl
                    << "3. nb_MFN    = " << ms.get_nb_MFN()           << std::endl
                    << "4. nb_WP_regr= " << ms.get_nb_WP_regression() << std::endl
                    << "5. nb_regr   = " << ms.get_nb_regression()    << std::endl
                    << "6. min |Y|   = " << ms.get_min_nY()           << std::endl
                    << "7. max |Y|   = " << ms.get_max_nY()           << std::endl
                    << "8. avg |Y|   = " << ms.get_avg_nY()           << std::endl
                    << std::endl;
                }
                fout2.close();
            }
            
            delete p;
        }
        else
            return false;
    }
    
    return true;
}

/*-----------------------------------------*/
/*        access to the date (private)     */
/*        (CPU name is also added)         */
/*-----------------------------------------*/
std::string Runner::get_date ( void ) const {
    
    std::ostringstream tmp_file;
    tmp_file << TMP_FILE << "." << _rank;
    
    std::string s ,
    cmd = "date \"+%Y-%m-%d, %H:%M:%S\" > " + tmp_file.str() + " 2> /dev/null";
    
    system ( cmd.c_str() );
    
    std::ifstream in ( (tmp_file.str()).c_str() );
    
    getline ( in , s );
    
    in.close();
    
    remove ( (tmp_file.str()).c_str() );
    
    if ( in.fail() )
        return "";
    
    char * name = new char[MPI_MAX_PROCESSOR_NAME];
    int    n;
    
    MPI_Get_processor_name ( name , &n );
    s += std::string(", ") + name;
    delete [] name;
    
    return s;
}
