#ifndef __RUNNER__
#define __RUNNER__

#include <mpi.h>
#include "Algo_Parameters.hpp"
#include "Result.hpp"
#include "problems/ARWHEAD/Arwhead.hpp"
#include "problems/B250/B250.hpp"
#include "problems/B500/B500.hpp"
#include "problems/BDQRTIC/Bdqrtic.hpp"
#include "problems/BIGGS6/Biggs6.hpp"
#include "problems/BRANIN/Branin.hpp"
#include "problems/BROWNAL/Brownal.hpp"
#include "problems/CRESCENT/Crescent.hpp"
#include "problems/DIFFICULT2/Difficult2.hpp"
#include "problems/DISK/Disk.hpp"
#include "problems/ELATTAR/ElAttar.hpp"
#include "problems/EVD61/Evd61.hpp"
#include "problems/FILTER/Filter.hpp"
#include "problems/G2/G2.hpp"
#include "problems/GOFFIN/Goffin.hpp"
#include "problems/GRIEWANK/Griewank.hpp"
#include "problems/HS78/Hs78.hpp"
#include "problems/HS114/Hs114.hpp"
#include "problems/L1HILB/L1Hilb.hpp"
#include "problems/MAD6/Mad6.hpp"
#include "problems/MDO/Mdo.hpp"
#include "problems/MORE_WILD/More_Wild.hpp"
#include "problems/MXHILB/MxHilb.hpp"
#include "problems/OPTENG_RBF/OptEng_RBF.hpp"
#include "problems/OSBORNE2/Osborne2.hpp"
#include "problems/PBC1/Pbc1.hpp"
#include "problems/PENALTY1/Penalty1.hpp"
#include "problems/PENALTY2/Penalty2.hpp"
#include "problems/PENTAGON/Pentagon.hpp"
#include "problems/PIGACHE/Pigache.hpp"
#include "problems/POLAK2/Polak2.hpp"
#include "problems/POWELLSG/Powellsg.hpp"
#include "problems/RADAR7/Radar7.hpp"
#include "problems/RANA/Rana.hpp"
#include "problems/RASTRIGIN/Rastrigin.hpp"
#include "problems/RHEOLOGY/Rheology.hpp"
#include "problems/ROSENBROCK/Rosenbrock.hpp"
#include "problems/SHOR/Shor.hpp"
#include "problems/SNAKE/Snake.hpp"
#include "problems/SROSENBR/Srosenbr.hpp"
#include "problems/STYRENE/Styrene.hpp"
#include "problems/TREFETHEN/Trefethen.hpp"
#include "problems/TRIDIA/Tridia.hpp"
#include "problems/VARDIM/Vardim.hpp"
#include "problems/WATSON12/Watson12.hpp"
#include "problems/WELL/Well.hpp"
#include "problems/WONG1/Wong1.hpp"
#include "problems/WONG2/Wong2.hpp"
#include "problems/WOODS/Woods.hpp"
#include "problems/XUWANG_F1/XuWang_f1.hpp"
#include "problems/XUWANG_F3/XuWang_f3.hpp"
#include "problems/XUWANG_F5/XuWang_f5.hpp"
#include "problems/XUWANG_F7/XuWang_f7.hpp"
#include "problems/XUWANG_F8/XuWang_f8.hpp"
#include "problems/XUWANG_F10/XuWang_f10.hpp"
#include "problems/XUWANG_F11/XuWang_f11.hpp"
#include "problems/XUWANG_F12/XuWang_f12.hpp"
#include "problems/XUWANG_F13/XuWang_f13.hpp"
#include "problems/XUWANG_F14/XuWang_f14.hpp"

// Pbs from Anne-Sophie Crelot
#include "problems/Barnes_Mixed_Case11/Barnes_Mixed_Case11.hpp"
#include "problems/Barnes_Mixed_Case21/Barnes_Mixed_Case21.hpp"
#include "problems/Barnes_Mixed_Case31/Barnes_Mixed_Case31.hpp"
#include "problems/CarSideImpact1/CarSideImpact1.hpp"
#include "problems/CarSideImpactDC1/CarSideImpactDC1.hpp"
#include "problems/G07_Mixed_Case31/G07_Mixed_Case31.hpp"
#include "problems/G07_Mixed_Case41/G07_Mixed_Case41.hpp"
#include "problems/G9_Mixed_Case1/G9_Mixed_Case1.hpp"
#include "problems/G9_Mixed_CaseIC1/G9_Mixed_CaseIC1.hpp"
#include "problems/Mystery_Mixed_Case11/Mystery_Mixed_Case11.hpp"
#include "problems/Mystery_Mixed_Case21/Mystery_Mixed_Case21.hpp"
#include "problems/Mystery_Mixed_Case31/Mystery_Mixed_Case31.hpp"
#include "problems/Mystery_Mixed_Case61/Mystery_Mixed_Case61.hpp"
#include "problems/PressureVessel_Mixed_Case1/PressureVessel_Mixed_Case1.hpp"
#include "problems/PressureVessel_Mixed_CaseIC1/PressureVessel_Mixed_CaseIC1.hpp"
#include "problems/Rastrigin_Mixed_Case11/Rastrigin_Mixed_Case11.hpp"
#include "problems/Rastrigin_Mixed_Case21/Rastrigin_Mixed_Case21.hpp"
#include "problems/Rastrigin_Mixed_Case31/Rastrigin_Mixed_Case31.hpp"
#include "problems/Rastrigin_Mixed_Case121/Rastrigin_Mixed_Case121.hpp"
#include "problems/Rastrigin_Mixed_Case221/Rastrigin_Mixed_Case221.hpp"
#include "problems/ReinforcedConcreteBeamMOD1/ReinforcedConcreteBeamMOD1.hpp"
#include "problems/ReinforcedConcreteBeamMODDC1/ReinforcedConcreteBeamMODDC1.hpp"
#include "problems/ReinforcedConcreteBeamMODIC1/ReinforcedConcreteBeamMODIC1.hpp"
#include "problems/ReinforcedConcreteBeamMODIDC1/ReinforcedConcreteBeamMODIDC1.hpp"
#include "problems/Rosenbrock_Mixed_Case11/Rosenbrock_Mixed_Case11.hpp"
#include "problems/Rosenbrock_Mixed_Case21/Rosenbrock_Mixed_Case21.hpp"
#include "problems/Rosenbrock_Mixed_Case31/Rosenbrock_Mixed_Case31.hpp"
#include "problems/Rosenbrock_Mixed_Case121/Rosenbrock_Mixed_Case121.hpp"
#include "problems/Rosenbrock_Mixed_Case221/Rosenbrock_Mixed_Case221.hpp"
#include "problems/SpeedReducerMOD_Mixed_Case1/SpeedReducerMOD_Mixed_Case1.hpp"
#include "problems/SpeedReducerMOD_Mixed_CaseIC1/SpeedReducerMOD_Mixed_CaseIC1.hpp"
#include "problems/SpringMOD_Mixed_Case1/SpringMOD_Mixed_Case1.hpp"
#include "problems/SpringMOD_Mixed_CaseIC1/SpringMOD_Mixed_CaseIC1.hpp"
#include "problems/SteppedCantileverBeamMOD1/SteppedCantileverBeamMOD1.hpp"
#include "problems/SteppedCantileverBeamMODDC1/SteppedCantileverBeamMODDC1.hpp"
#include "problems/SteppedCantileverBeamMODIC1/SteppedCantileverBeamMODIC1.hpp"
#include "problems/SteppedCantileverBeamMODIDC1/SteppedCantileverBeamMODIDC1.hpp"

/*--------------------------------------*/
/*   class for custom NOMAD evaluator   */
/*--------------------------------------*/
class Custom_Evaluator : public NOMAD::Evaluator {
    
private:
    const Problem & _pb;
    
public:
    Custom_Evaluator  ( const NOMAD::Parameters & p  ,
                       const Problem            & pb   ) :
    NOMAD::Evaluator ( p ) , _pb ( pb ) {}
    
    ~Custom_Evaluator ( void ) {}
    
    bool eval_x ( NOMAD::Eval_Point   & x          ,
                 const NOMAD::Double & h_max      ,
                 bool                & count_eval   ) const {
        if ( _pb.get_bb_exe().empty() )
            return _pb.eval_x ( x , count_eval );
        return NOMAD::Evaluator::eval_x ( x , h_max , count_eval );
    }
};

// C.Tribes NOT WORKING WITH CURRENT DESIGN OF RUNNER
//
///*--------------------------------------*/
///*   class for custom NOMAD evaluator   */
///*--------------------------------------*/
//class Custom_Extended_Poll : public NOMAD::Extended_Poll {
//    
//private:
//    const Problem & _pb;
//    
//public:
//    Custom_Extended_Poll  ( NOMAD::Parameters & p ,
//                           const Problem      & pb  ) :
//    NOMAD::Extended_Poll (p), _pb(pb) {}
//    
//    virtual ~Custom_Extended_Poll ( void ) {}
//    
//    virtual void construct_extended_points ( const NOMAD::Eval_Point & x )
//    {
//        // _pb.construct_extended_points ( x );
//    }
//    
//};

/*--------------------------------------*/
/*           the Runner class           */
/*--------------------------------------*/
class Runner : public NOMAD::Uncopyable
{
    
private:
    
    NOMAD::Display             & _out;          // display
    int                          _rank;         // MPI rank
    int                          _np;           // MPI np
    std::vector<Problem *>       _all_pbs;      // list of all problems
    std::vector<Problem *>       _selected_pbs; // list of selected problems
    std::vector<Algo_Parameters> _test_configs; // list of test configs
    int                          _n_pb;         // number of selected pbs
    int                          _n_algo;       // number of test configs
    int                          _n_seed_run;
    Result                   *** _results;      // results
    std::string               ** _test_id;      // the test names
    
    // run NOMAD:
    bool run_nomad ( bool                display ,
                    const std::string & test_id ,
                    Problem           & pb      ,
                    Algo_Parameters   & ap        ) const;
    
    // clear memory:
    void clear_memory ( void );
    
    // add a problem to the list of selected problems:
    void add_selected_problem ( Problem & pb );
    
    // add a test config to the list of test configs:
    void add_test_config ( Algo_Parameters & ap );
    
    // find a problem in the list of all problems:
    Problem * find_problem ( const std::string & problem_id ) const;
    
    // display a list of problems:
    void display_pbs ( const std::vector<Problem *> & pbs ) const;
    
    // display instance name:
    void display_instance_name ( int i_pb , int i_algo , int seed=-1 ) const;
    
    // stop the slaves:
    void stop_slaves ( void ) const;
    
    // set a result:
    void set_result ( const std::string     & test_id ,
                     Result                   result[],
                     Problem               & pb      ,
                     const Algo_Parameters & ap        ) const;
    
    // check if fx is at alpha % relatively close to fxe:
    static bool is_within ( const NOMAD::Double & fx    ,
                           const NOMAD::Double & fxe   ,
                           const NOMAD::Double & alpha   );
    
    // compute relative error between fx and fxe:
    static NOMAD::Double compute_alpha ( const NOMAD::Double & fx  ,
                                        const NOMAD::Double & fxe   );
    
    // check a test directory:
    bool check_test_dir ( const std::string & test_dir     ,
                         const std::string & pb_dir       ,
                         bool                display      ,
                         std::string       & error_msg      ) const;
    
    // get the results:
    static bool get_results ( const std::string     & test_id ,
                             const Problem         & pb      ,
                             const Algo_Parameters & ap      ,
                             Result                  result[]);
    
    // read max_bb_eval in ID file:
    static bool read_max_bb_eval ( std::ifstream & in          ,
                                  int           & max_bb_eval ,
                                  bool          & times_n       );
    
    // read x0 in ID file:
    static bool read_x0 ( std::ifstream & in , NOMAD::Point & x0 );
    
    // construct a list of sub-directories:
    static bool construct_list_of_subdirs ( std::list<std::string> & list_of_dirs ,
                                           const std::string      & directory      );
    
    // functions to access directory and file names:
    static std::string get_test_dir ( const std::string & test_id ,
                                     const Problem     & pb      )
    {
        return pb.get_tests_dir() + test_id +  "/";
    }
    
    static std::string get_id_file_name ( const std::string & test_id ,
                                         const Problem     & pb       ,
                                         int                 seed  )
    {
        return Runner::get_test_dir ( test_id , pb ) + ID_FILE + "." + static_cast<ostringstream*>( &(ostringstream() << seed ) )->str();
    }
    
    static std::string get_stats_file_name ( const std::string & test_id ,
                                            const Problem      & pb      ,
                                            int                  seed  )
    {
        return Runner::get_test_dir ( test_id , pb ) + STATS_FILE + "." + static_cast<ostringstream*>( &(ostringstream() << seed ) )->str();
    }
    
    // access to the date:
    std::string get_date ( void ) const;
	
    // C. Tribes june 2012 add the 3 functions for convenience
    NOMAD::Point get_fx0s() const ;
    NOMAD::Point get_best_fx() const ;
    NOMAD::Point get_best_fx_MW() const ;
    int get_bbe_max() const ;
    int get_dimPbMin() const;
    
	
public:
    
    // constructor:
    Runner ( NOMAD::Display & out );
    
    // destructor:
    virtual ~Runner ( void );
    
    // run:
    bool run ( std::string & error_msg );
    
    // display all problems:
    void display_all_problems ( void ) const;
    
    // display results:
    void display_algo_diff     ( void ) const;
    void display_perf_prof     ( void ) const;
    void display_perf_prof_s   ( void ) const;
    void display_perf_prof_MW  ( int tau_exp) const;
    void display_results_table ( void ) const;
    void display_data_prof     ( int tau_exp ) const; // tau = 1E-tau_exp
    void display_data_prof_MW  ( int tau_exp ) const; // tau = 1E-tau_exp
    
    // select problems:
    // ----------------
    void select_all_problems         ( void             );
    bool select_problem_by_name      ( std::string name );
    bool select_problems_by_keyword  ( std::string kw   );
    bool select_problems_by_size     ( int n_min , int n_max );
    
    bool refine_problems_by_keyword  ( std::string kw   );
    
    bool exclude_problem_by_name     ( std::string name );
    bool exclude_problems_by_keyword ( std::string kw   );
    bool exclude_problems_by_size    ( int n_min , int n_max );
    
    void clear_selected_problems     ( void );
    void display_selected_problems   ( void ) const;
    
    // available tests:
    // ----------------
    void display_available_tests ( void ) const;
    void clear_invalid_tests     ( void ) const;
    void clear_all_tests         ( void ) const;
    
    void clear_tests_by_solver ( std::string solver_name    ,
                                std::string solver_version   ) const;
    
    void clear_tests_by_config_and_selected_pbs ( std::string solver_name    ,
                                                 std::string solver_version ) const;
    
    bool read_test_config        ( const std::string & config_file_name ,
                                  std::string       & error_msg          );
    
    // test configs:
    // -------------
    void create_def_test_config ( void );
    void clear_test_configs     ( void );
    void display_test_configs   ( void ) const;
};

#endif
