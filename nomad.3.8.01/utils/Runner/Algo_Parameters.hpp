#ifndef __ALGO_PARAMETERS__
#define __ALGO_PARAMETERS__

#include "Problem.hpp"

class Algo_Parameters {
    
private:
    
    int                   _index;
    
    std::string           _solver_name;
    std::string           _solver_version;
    
    std::string           _description;
    
    bool                  _is_random;
    
    int                   _max_bb_eval;
    bool                  _times_n; // if true, max_bb_eval *= n
    
    NOMAD::Point          _x0; // if defined, replaces the default Problem::_x0
    
    bool                  _VNS_search;
    NOMAD::Double         _VNS_trigger;
    
    int                   _LH_search[2];
    bool                  _opp_LH;
    
    int                  _opp_eval;
    
    NOMAD::direction_type _dir_type;
    
    
    NOMAD::Double         _d0_rel; // initial mesh size (relative value)
    NOMAD::Double         _d0_abs; // initial mesh size (absolute value)
    
    bool					_anisotropic_mesh;
    int                   _initial_mesh_index;
    int                   _mesh_coarsening_exponent;
    int                   _mesh_refining_exponent;
    NOMAD::Double         _mesh_update_basis;
    NOMAD::Double         _poll_update_basis;
    
    bool                  _use_granular_variables;
    
    NOMAD::hnorm_type     _h_norm;
    NOMAD::Double         _rho;
    
    bool                  _speculative_search;
    bool                  _snap_to_bounds;
    
    NOMAD::model_type     _model_search;
    NOMAD::model_type     _model_eval_sort;
    bool                  _model_eval_sort_cautious;
    bool                  _model_proj_to_mesh;
    bool                  _model_search_optimistic;
    int                   _model_max_trial_pts;
    int                   _model_max_Y_size;
    int                   _model_min_Y_size;
    bool                  _model_use_WP;
    NOMAD::Double         _model_radius_factor;
    
    
    bool                  _has_sgte;
    int                   _user_param;
  
    int                   _diversification;
    
    
    // reset:
    void reset ( void );
    
    // compute the initial mesh size from _d0_rel and _d0_abs:
    void compute_initial_mesh_size ( const Problem & pb ,
                                    NOMAD::Point  & d0   ) const;
    
public:
    
    // constructor:
    Algo_Parameters ( const std::string & solver_name    ,
                     const std::string & solver_version   ) :
    _solver_name    ( solver_name    ) ,
    _solver_version ( solver_version )   { reset(); }
    
    // copy constructor:
    Algo_Parameters ( const Algo_Parameters & ap ) { *this = ap; }
    
    // affectation operator:
    Algo_Parameters & operator = ( const Algo_Parameters & ap );
    
    // destructor:
    virtual ~Algo_Parameters ( void ) {}
    
    // GET methods:
    int                  get_index          ( void ) const { return _index;       }
    const NOMAD::Point & get_x0             ( void ) const { return _x0;          }
    const std::string  & get_solver_name    ( void ) const { return _solver_name; }
    const std::string  & get_solver_version ( void ) const
    { return _solver_version; }
    
    bool is_nomad ( void ) const;
    
    std::string get_solver_name_version ( void ) const;
    
    void get_max_bb_eval ( int & max_bbe , bool & times_n ) const;
    
    bool use_models ( void ) const {
        return _model_search != NOMAD::NO_MODEL || _model_eval_sort != NOMAD::NO_MODEL;
    }
    
    // SET methods:
    
    void set_description ( const std::string & s ) { _description = s; }
    
    void set_index ( int i ) { _index = i; }
    
    void set_x0 ( const NOMAD::Point & x0 ) { _x0 = x0; }
    
    void set_opp_eval ( bool oe  ) { _opp_eval = oe;  }
    
    void set_max_bb_eval ( int bbe , bool times_n );
    
    void set_LH_search ( int p0 , int pi , bool opp );
    
    void set_VNS_search ( bool s , const NOMAD::Double & t );
    
    void set_initial_mesh_size ( const NOMAD::Double & d0 , bool rel );
    
    void set_initial_mesh_index ( int l0 )
    { _initial_mesh_index = l0; }
    
    void set_mesh_coarsening_exponent ( int e  )
    { _mesh_coarsening_exponent = e; }
    
    void set_mesh_refining_exponent ( int e  )
    { _mesh_refining_exponent = e; }
    
    void set_mesh_update_basis ( const NOMAD::Double & t )
    { _mesh_update_basis = t; }
    
    void set_dir_type ( NOMAD::direction_type dt );
    void set_h_norm   ( NOMAD::hnorm_type     t  ) { _h_norm = t; }
    void set_rho      ( const NOMAD::Double & r  ) { _rho    = r; }
    
    
    void set_speculative_search ( bool s ) { _speculative_search = s; }
    void set_snap_to_bounds     ( bool s ) { _snap_to_bounds     = s; }
    
    void set_model_search             ( NOMAD::model_type s ) { _model_search    = s; }
    void set_model_eval_sort          ( NOMAD::model_type s ) { _model_eval_sort = s; }
    void set_model_eval_sort_cautious ( bool c ) { _model_eval_sort_cautious = c; }
    void set_model_proj_to_mesh       ( bool p ) { _model_proj_to_mesh       = p; }
    void set_model_search_optimistic  ( bool o ) { _model_search_optimistic  = o; }
    void set_model_max_trial_pts      ( int  p ) { _model_max_trial_pts      = p; }
    void set_model_max_Y_size         ( int  p ) { _model_max_Y_size         = p; }
    void set_model_use_WP             ( bool u ) { _model_use_WP             = u; }
    
    void set_has_sgte                 ( bool s ) { _has_sgte = s; }
    
    void set_use_granular_variables   ( bool g ) { _use_granular_variables = g; }
    
    void set_model_min_Y_size ( int  p );
    
    void set_model_radius_factor ( const NOMAD::Double & r )
    { _model_radius_factor = r; }
    
    // set the parameters:
    void set_parameters ( const Problem     & pb              ,
                         const std::string & stats_file_name ,
                         bool                display         ,
                         NOMAD::Parameters & p                 ) const;
    
    // display:
    void display ( const NOMAD::Display & out , int w = -1 ) const;
    
    // display id:
    void display_id ( const NOMAD::Display & out ) const;
    
    // read id:
    bool read_id ( std::ifstream & in );
    
    // check compatibility with other Algo_Parameters object:
    bool is_compatible ( const Algo_Parameters & ap ,
                        const Problem         & pb   ) const;
};

#endif
