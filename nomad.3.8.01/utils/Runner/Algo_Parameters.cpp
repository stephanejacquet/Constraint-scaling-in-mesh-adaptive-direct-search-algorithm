#include "Algo_Parameters.hpp"

/*---------------------------------------------------------*/
/*                    affectation operator                 */
/*---------------------------------------------------------*/
Algo_Parameters & Algo_Parameters::operator = ( const Algo_Parameters & ap ) {
	
	if ( this == & ap )
		return *this;
	
	_description              = ap._description;
	_index                    = ap._index;
	_solver_name              = ap._solver_name;
	_solver_version           = ap._solver_version;
	_is_random                = ap._is_random;
	_max_bb_eval              = ap._max_bb_eval;
	_times_n                  = ap._times_n;
	_x0                       = ap._x0;
	_LH_search[0]             = ap._LH_search[0];
	_LH_search[1]             = ap._LH_search[1];
	_VNS_search               = ap._VNS_search;
	_VNS_trigger              = ap._VNS_trigger;
	_opp_LH                   = ap._opp_LH;
	_opp_eval                 = ap._opp_eval;
	_dir_type                 = ap._dir_type;	
	_d0_rel                   = ap._d0_rel;
	_d0_abs                   = ap._d0_abs;
	_anisotropic_mesh			= ap._anisotropic_mesh;
	_initial_mesh_index       = ap._initial_mesh_index;
	_mesh_coarsening_exponent = ap._mesh_coarsening_exponent;
	_mesh_refining_exponent   = ap._mesh_refining_exponent;
	_mesh_update_basis        = ap._mesh_update_basis;
	_poll_update_basis        = ap._poll_update_basis;
	_h_norm                   = ap._h_norm;
	_rho                      = ap._rho;
	_speculative_search       = ap._speculative_search;
	_snap_to_bounds           = ap._snap_to_bounds;
	_model_search             = ap._model_search;
	_model_eval_sort          = ap._model_eval_sort;
	_model_eval_sort_cautious = ap._model_eval_sort_cautious;
	_model_proj_to_mesh       = ap._model_proj_to_mesh;
	_model_search_optimistic  = ap._model_search_optimistic;
	_model_max_trial_pts      = ap._model_max_trial_pts;
	_model_max_Y_size         = ap._model_max_Y_size;
	_model_min_Y_size         = ap._model_min_Y_size;
	_model_use_WP             = ap._model_use_WP;
	_model_radius_factor      = ap._model_radius_factor;
    _has_sgte                 = ap._has_sgte;
	_user_param               = ap._user_param;
        _diversification	  = ap._diversification;
    _use_granular_variables   = ap._use_granular_variables;
	
	return *this;
}

/*----------------------------------------------*/
/*                 reset (private)              */
/*----------------------------------------------*/
void Algo_Parameters::reset ( void ) {
	
	_description = "COMMENTS HERE (max one line)";
	
	_is_random                = false;
	_index                    = -1;
	_max_bb_eval              = -1;
	_times_n                  = false;
	_dir_type                 = NOMAD::ORTHO_NP1_QUAD;	
	_opp_LH                   = true;
    // C.Tribes may 14, 2015 --- modif to handle opportunistic_min_eval
	// _opp_eval                 = true;
    _opp_eval                 = 1 ;
	_VNS_search               = false;
	_speculative_search       = true;
	_snap_to_bounds           = true;
	_model_search             = NOMAD::QUADRATIC_MODEL;
	_model_eval_sort          = NOMAD::QUADRATIC_MODEL;
	_model_eval_sort_cautious = false;
	_model_proj_to_mesh       = true;
	_model_search_optimistic  = true;
	_model_use_WP             = false;
	_model_max_trial_pts      = 4;
	_model_max_Y_size         = 500;
	_model_min_Y_size         = -1;
	_model_radius_factor      = 2.0;
	
	_LH_search[0] = _LH_search[1] = -1;
	
	
	_anisotropic_mesh			= true;
	_mesh_update_basis        = 4.0;
	_poll_update_basis        = 2.0;	
	_mesh_coarsening_exponent =  1;
	_mesh_refining_exponent   = -1;
	_initial_mesh_index       =  0;
    
    _use_granular_variables   = true;
	
	_d0_rel.clear();
	_d0_abs.clear();
	
	_x0.clear();
	
	_h_norm = NOMAD::L2;
	_rho    = 0.1;
	
    _has_sgte = false;
	_user_param=0;
	_diversification=0;
	
}


/*---------------------------------------------*/
/*        check if the solver is NOMAD         */
/*---------------------------------------------*/
bool Algo_Parameters::is_nomad ( void ) const {
	std::string solver_name = _solver_name;
	NOMAD::toupper ( solver_name );
	return solver_name == "NOMAD";
}

/*-------------------------------------------------*/
/*  get a string with the solver name and version  */
/*-------------------------------------------------*/
std::string Algo_Parameters::get_solver_name_version ( void ) const {
	std::ostringstream msg;
	msg << _solver_name << " " << _solver_version;
	return msg.str();
}

/*----------------------------------------------*/
/*                    display                   */
/*----------------------------------------------*/
void Algo_Parameters::display ( const NOMAD::Display & out , int w ) const {
	
	out << "[" << std::setw(w) << get_solver_name_version()
	<< "] [max_bbe=";
	
	if ( _max_bb_eval < 0 )
		out << "-";
	else {
		out << _max_bb_eval;
		if ( _times_n )
			out << "N";
	}
	out << "]";
	
	// display non-default parameters:
	if ( _x0.size() > 0 && _x0.is_complete() )
		out << " [x0=(" << _x0 << ")]";
	
    out << " [dir_type=" << _dir_type << "]";
 	
	if ( _opp_eval !=1 )
		out << " [opp_eval="<< _opp_eval << "]";
	
	if ( _LH_search[0] > 0 || _LH_search[1] > 0 )
		out << " [LH=" << _LH_search[0] << "," << _LH_search[1]
		<< "," << _opp_LH << "]";
	
	if ( _VNS_search )
		out << " [VNS=" << _VNS_trigger << "]";
	
	if ( !_speculative_search )
		out << " [spec_search=0]";
	
	if ( !_snap_to_bounds )
		out << " [snap_bnds=0]";
	
	if ( _model_search != NOMAD::NO_MODEL ) {
		out << " [MS=" << _model_search << "]";
		if ( !_model_proj_to_mesh )
			out << " [MS_proj_mesh=0]";
		if ( _model_max_trial_pts != 4 )
			out << " [MS_max_trial_pts=" << _model_max_trial_pts << "]";
		if ( !_model_search_optimistic )
			out << " [MS_opp=0]";
	}
	else
		out << " [MS=0]";
	
	if ( _model_eval_sort != NOMAD::NO_MODEL ) {
		out << " [model_ev_sort=" << _model_eval_sort << "]";
		if ( !_model_eval_sort_cautious )
			out << " [model_ev_sort_cautious=0]";
	}
	else
		out << " [model_ev_sort=0]";
	
	if ( use_models() ) {
		if ( _model_max_Y_size != 500 )
			out << " [model_max_Y_size=" << _model_max_Y_size << "]";
		if ( _model_min_Y_size != -1 )
			out << " [model_min_Y_size=" << _model_min_Y_size << "]";
		if ( _model_use_WP )
			out << " [model_use_WP=1]";
		if ( _model_radius_factor != 2.0 )
			out << " [model_radius_factor=" << _model_radius_factor << "]";
	}
	
	if ( _mesh_update_basis != 4.0 )
		out << " [mesh_UB=" << _mesh_update_basis << "]";
	if ( _poll_update_basis != 2.0 )
		out << " [poll_UB=" << _poll_update_basis << "]";
	if ( _mesh_coarsening_exponent != 1 )
		out << " [mesh_CE=" << _mesh_coarsening_exponent << "]";
	if ( _mesh_refining_exponent != -1 )
		out << " [mesh_RE=" << _mesh_refining_exponent << "]";
	if ( _initial_mesh_index != 0 )
		out << " [ell_0=" << _initial_mesh_index << "]";
	if ( ! _anisotropic_mesh )
		out << " [anis_mesh=0]";
	
	if ( _d0_rel.is_defined() )
		out << " [d0_rel=" << _d0_rel << "]";
	if ( _d0_abs.is_defined() )
		out << " [d0_abs=" << _d0_abs << "]";
	
	if ( _h_norm != NOMAD::L2 )
		out << " [h_norm=" << _h_norm << "]";
	if ( _rho != 0.1 )
		out << " [rho=" << _rho << "]";
	
    if ( _has_sgte )
        out << " [has_sgte=1]";
    
    if ( _use_granular_variables )
        out << " [use_granular_variables=1]";
    
	if ( _user_param != 0 )
		out << " [user_param=" << _user_param << "]"; 
	
		out << " [diversification=" << _diversification << "]";
}

/*----------------------------------------------*/
/*                   display id                 */
/*----------------------------------------------*/
void Algo_Parameters::display_id ( const NOMAD::Display & out ) const {
	out << _description << std::endl << std::endl
	<< "max_bb_eval " << _max_bb_eval;
	if ( _times_n )
		out << "N";
	out << std::endl << "x0 ";
	if ( _x0.size() > 0 && _x0.is_complete() )
		out << _x0;
	else
		out << "-";
	out << std::endl
	<< "VNS_search "               << _VNS_search   << " "
	<< _VNS_trigger  << std::endl
	<< "LH_search "                << _LH_search[0] << " " << _LH_search[1]
	<< " " << _opp_LH            << std::endl
	<< "opp_eval "                 << _opp_eval                 << std::endl
	<< "dir_type "                 << _dir_type                 << std::endl
	<< "rel_initial_mesh_size "    << _d0_rel                   << std::endl
	<< "abs_initial_mesh_size "    << _d0_abs                   << std::endl
	<< "initial_mesh_index "       << _initial_mesh_index       << std::endl
	<< "mesh_coarsening_exponent " << _mesh_coarsening_exponent << std::endl
	<< "mesh_refining_exponent "   << _mesh_refining_exponent   << std::endl
	<< "mesh_update_basis "        << _mesh_update_basis        << std::endl
	<< "poll_update_basis "        << _poll_update_basis        << std::endl
	<< "anisotropic_mesh "         << _anisotropic_mesh			<< std::endl
	<< "h_norm "                   << _h_norm                   << std::endl
	<< "rho "                      << _rho                      << std::endl
	<< "speculative_search "       << _speculative_search       << std::endl
	<< "snap_to_bounds "           << _snap_to_bounds           << std::endl;
	
	out << "model_search ";
	if ( _model_search == NOMAD::QUADRATIC_MODEL )
		out << "quad";
	else
		out << "0";
	out << std::endl
	<< "model_eval_sort ";
	if ( _model_eval_sort == NOMAD::QUADRATIC_MODEL )
		out << "quad";
	else
		out << "0";
	out << std::endl
	<< "model_eval_sort_cautious " << _model_eval_sort_cautious << std::endl
	<< "model_proj_to_mesh "       << _model_proj_to_mesh       << std::endl
	<< "model_search_optimistic "  << _model_search_optimistic  << std::endl
	<< "model_max_trial_pts "      << _model_max_trial_pts      << std::endl
	<< "model_max_Y_size "         << _model_max_Y_size         << std::endl
	<< "model_min_Y_size "         << _model_min_Y_size         << std::endl
	<< "model_use_WP "             << _model_use_WP             << std::endl
	<< "model_radius_factor "      << _model_radius_factor      << std::endl
    << "has_sgte "                 << _has_sgte                 << std::endl
    << "use_granular_variables "   << _use_granular_variables   << std::endl
	<< "user_param "               << _user_param << std::endl
	<< "diversification "               << _diversification;
}

/*---------------------------------------------------------*/
/*  check compatibility with other Algo_Parameters object  */
/*---------------------------------------------------------*/
bool Algo_Parameters::is_compatible ( const Algo_Parameters & ap ,
									 const Problem         & pb   ) const {
	
   
	// non-reproductivity is enabled:
	if ( _is_random || ap._is_random )
		return false;
	
	if ( _VNS_search != ap._VNS_search )
		return false;
	
	if ( _VNS_search ) {
		
		if (_VNS_trigger.is_defined() != ap._VNS_trigger.is_defined() )
			return false;
		
		if ( _VNS_trigger.is_defined() && _VNS_trigger != ap._VNS_trigger )
			return false;
	}
    
	if ( _LH_search[0] != ap._LH_search[0] ||
		_LH_search[1] != ap._LH_search[1]    )
		return false;
	
	if ( ( _LH_search[0] > 0 || _LH_search[1] > 0 ) &&
		_opp_LH != ap._opp_LH                         )
		return false;
	
	if ( _opp_eval != ap._opp_eval )
		return false;
	
	if ( _dir_type != ap._dir_type )
		return false;
	
	
	
	if ( _d0_rel.is_defined() != ap._d0_rel.is_defined() ||
		_d0_abs.is_defined() != ap._d0_abs.is_defined()    )
		return false;
	
	if ( ( _d0_rel.is_defined() && _d0_rel != ap._d0_rel ) ||
		( _d0_abs.is_defined() && _d0_abs != ap._d0_abs )    )
		return false;
	
	if ( _initial_mesh_index != ap._initial_mesh_index )
		return false;
	
	if ( _mesh_coarsening_exponent != ap._mesh_coarsening_exponent )
		return false;
	
	if ( _mesh_refining_exponent != ap._mesh_refining_exponent )
		return false;
	
	if ( _mesh_update_basis != ap._mesh_update_basis )
		return false;

	if ( _poll_update_basis != ap._poll_update_basis )
		return false;
	
	if ( _anisotropic_mesh != ap._anisotropic_mesh )
		return false;
	
	if ( pb.has_constraints() ) {
		
		if ( _h_norm != ap._h_norm )
			return false;
		
		if ( _rho != ap._rho )
			return false;
	}
	
	if ( _speculative_search != ap._speculative_search )
		return false;
	
	if ( pb.has_bounds() && _snap_to_bounds != ap._snap_to_bounds )
		return false;
	
	if ( _model_search != ap._model_search )
		return false;
	
	if ( _model_eval_sort != ap._model_eval_sort )
		return false;
	
	if ( use_models() ) {
		
		if ( _model_eval_sort_cautious != ap._model_eval_sort_cautious )
			return false;
		
		if ( _model_proj_to_mesh != ap._model_proj_to_mesh )
			return false;
		
		if ( _model_search_optimistic != ap._model_search_optimistic )
			return false;
		
		if ( _model_max_trial_pts != ap._model_max_trial_pts )
			return false;
		
		if ( _model_max_Y_size != ap._model_max_Y_size )
			return false;
		
		if ( _model_min_Y_size != ap._model_min_Y_size )
			return false;
		
		if ( _model_use_WP != ap._model_use_WP )
			return false;
		
		if ( _model_radius_factor != ap._model_radius_factor )
			return false;
	}
    if ( _has_sgte != ap._has_sgte )
        return false;
    
    if ( _use_granular_variables != ap._use_granular_variables )
        return false;

	if ( _user_param != ap._user_param)
		return false;

	if ( _diversification != ap._diversification)
		return false;	
	
	return true;
}

/*----------------------------------------------*/
/*                   read id file               */
/*               (after MAX_BB_EVAL)            */
/*----------------------------------------------*/
bool Algo_Parameters::read_id ( std::ifstream & in ) {
	
	reset();
	
	std::string s;
	
	in  >> s >> _VNS_search >> _VNS_trigger
	>> s >> _LH_search[0] >> _LH_search[1] >> _opp_LH
	>> s >> _opp_eval
	>> s;
	
	getline ( in , s );
	
	string::iterator it=s.end()-1;
	while (s.compare(s.size()-1,1," ")==0 && it!=s.begin())
	{
		s.erase(it);
		--it;
	}
	
	
	NOMAD::toupper ( s );
	if ( s == " ORTHO-MADS 1" )
		_dir_type = NOMAD::ORTHO_1;
	else if ( s == " ORTHO-MADS 2" )
		_dir_type = NOMAD::ORTHO_2;
	else if ( s == " ORTHO-MADS 2N" )
		_dir_type = NOMAD::ORTHO_2N;
	else if ( s == " ORTHO-MADS N+1 QUAD" )
		_dir_type = NOMAD::ORTHO_NP1_QUAD;
	else if ( s == " ORTHO-MADS N+1 NEG" )
		_dir_type = NOMAD::ORTHO_NP1_NEG;
    // C.Tribes june 19, 2015 --- added
    else if ( s == " ORTHO-MADS N+1 UNI" )
        _dir_type = NOMAD::ORTHO_NP1_UNI;
    else if ( s == " LT-MADS 1" )
		_dir_type = NOMAD::LT_1;
	else if ( s == " LT-MADS 2" )
		_dir_type = NOMAD::LT_2;
	else if ( s == " LT-MADS 2N" )
		_dir_type = NOMAD::LT_2N;
	else if ( s == " LT-MADS N+1" )
		_dir_type = NOMAD::LT_NP1;
	else if ( s == " GPS N, BINARY" )
		_dir_type = NOMAD::GPS_BINARY;
	else if ( s == " GPS 2N, STATIC" )
		_dir_type = NOMAD::GPS_2N_STATIC;
	else if ( s == " GPS 2N, RANDOM" )
		_dir_type = NOMAD::GPS_2N_RAND;
	else if ( s == " GPS N+1, STATIC, UNIFORM ANGLES" )
		_dir_type = NOMAD::GPS_NP1_STATIC_UNIFORM;
	else if ( s == " GPS N+1, STATIC" )
		_dir_type = NOMAD::GPS_NP1_STATIC;
	else if ( s ==  " GPS N+1, RANDOM, UNIFORM ANGLES" )
		_dir_type = NOMAD::GPS_NP1_RAND_UNIFORM;
	else if ( s == " GPS N+1, RANDOM" )
		_dir_type = NOMAD::GPS_NP1_RAND;
	else 
		return false;
	
	
	
	in >> s >> _d0_rel
	>> s >> _d0_abs
	>> s >> _initial_mesh_index
	>> s >> _mesh_coarsening_exponent
	>> s >> _mesh_refining_exponent
	>> s >> _mesh_update_basis
	>> s >> _poll_update_basis
	>> s >> _anisotropic_mesh
	>> s >> s;
	
	NOMAD::toupper ( s );
	
	if ( s == "L1" )
		_h_norm = NOMAD::L1;
	else if ( s == "L2" )
		_h_norm = NOMAD::L2;
	else if ( s == "LINF" )
		_h_norm = NOMAD::LINF;
	else
		return false;
	
	in >> s >> _rho
	>> s >> _speculative_search
	>> s >> _snap_to_bounds;
	
	// model search and model eval sort (0 or quad or tgp):
	in >> s;
	getline ( in , s );
	NOMAD::toupper ( s );
	
	if ( s == " QUAD" || s==" 1")
		_model_search = NOMAD::QUADRATIC_MODEL;
	else
		_model_search = NOMAD::NO_MODEL;
	
	in >> s;
	getline ( in , s );
	NOMAD::toupper ( s );
	if ( s == " QUAD" || s==" 1")
		_model_eval_sort = NOMAD::QUADRATIC_MODEL;
	else
		_model_eval_sort = NOMAD::NO_MODEL; 
	
	in >> s >> _model_eval_sort_cautious
	>> s >> _model_proj_to_mesh
	>> s >> _model_search_optimistic
	>> s >> _model_max_trial_pts
	>> s >> _model_max_Y_size
	>> s >> _model_min_Y_size
	>> s >> _model_use_WP
    >> s >> _model_radius_factor;
    
    in >> s >> _has_sgte;
    
    in >> s >> _use_granular_variables;
    
	in >> s >> _user_param;

	in >> s >> _diversification;
	
	// get the 'RUN OK' flag:
	in >> s;
	if ( s != "RUN" )
		return false;
    in >> s;
	in >> s;  // run seed number not tested
	in >> s;
	if ( s != "OK" )
		return false;
	
	return !in.fail();
}

/*-------------------------------------------*/
/*         set the initial mesh size         */
/*-------------------------------------------*/
void Algo_Parameters::set_initial_mesh_size ( const NOMAD::Double & d0  ,
											 bool                  rel   ) {
	if ( rel )
		_d0_rel = d0;
	else
		_d0_abs = d0;
}

/*----------------------------------------------------------*/
/*  compute the initial mesh size from _d0_rel and _d0_abs  */
/*  (private)                                               */
/*----------------------------------------------------------*/
void Algo_Parameters::compute_initial_mesh_size ( const Problem & pb ,
												 NOMAD::Point  & d0   ) const {
	int               i , n = pb.get_n();
	const NOMAD::Point & lb = pb.get_lb();
	const NOMAD::Point & ub = pb.get_ub();
	
	d0 = NOMAD::Point ( n );
	
	if ( _d0_rel.is_defined() ) {
		for ( i = 0 ; i < n ; ++i )
			if ( lb[i].is_defined() && ub[i].is_defined() )
				d0[i] = _d0_rel*(ub[i]-lb[i]);
	}
	else if ( _d0_abs.is_defined() ) {
		for ( i = 0 ; i < n ; ++i )
			d0[i] = _d0_abs;
	}
	
	// if d0[i] is not defined, a default value will be computed by
	// NOMAD::Parameters::check()
}

/*----------------------------------------------*/
/*       get the max number of evaluations      */
/*----------------------------------------------*/
void Algo_Parameters::get_max_bb_eval ( int  & max_bbe ,
									   bool & times_n   ) const {
	max_bbe = _max_bb_eval;
	times_n = _times_n;
}

/*----------------------------------------------*/
/*       set the max number of evaluations      */
/*----------------------------------------------*/
void Algo_Parameters::set_max_bb_eval ( int bbe , bool times_n ) {
	_max_bb_eval = bbe;
	_times_n     = times_n;
	if ( bbe <= 0 ) {
		_max_bb_eval = -1;
		_times_n     = false;
	}
}

/*----------------------------------------------*/
/*              set the min Y size              */
/*----------------------------------------------*/
void Algo_Parameters::set_model_min_Y_size ( int  p ) {
	_model_min_Y_size = ( p < 0 ) ? -1 : p;
}

/*----------------------------------------------*/
/*            set the directions type           */
/*----------------------------------------------*/
void Algo_Parameters::set_dir_type ( NOMAD::direction_type dt ) {
	_dir_type = dt;
	_is_random = NOMAD::dir_is_random ( _dir_type );
}

/*----------------------------------------------*/
/*               set the LH search              */
/*----------------------------------------------*/
void Algo_Parameters::set_LH_search ( int p0 , int pi , bool opp ) {
	_LH_search[0] = p0;
	_LH_search[1] = pi;
	_opp_LH       = opp;
	if ( _LH_search[0] > 0 || _LH_search[1] > 0 )
		_is_random = true;
}

/*----------------------------------------------*/
/*               set the VNS search             */
/*----------------------------------------------*/
void Algo_Parameters::set_VNS_search ( bool s , const NOMAD::Double & t ) {
	_VNS_search = s;
	if ( _VNS_search ) {
		if ( t.is_defined() )
			_VNS_trigger = t;
		else
			_VNS_trigger = 0.75;
	}
	else
		_VNS_trigger.clear();
}

/*----------------------------------------------*/
/*               set the parameters             */
/*----------------------------------------------*/
void Algo_Parameters::set_parameters ( const Problem     & pb              ,
                                      const std::string & stats_file_name ,
                                      bool                display         ,
                                      NOMAD::Parameters & p                 ) const
{
    
	NOMAD::Point::set_display_limit ( -1 );
	
	int n = pb.get_n();
	
	p.set_DIMENSION      ( n );
	p.set_BB_INPUT_TYPE  ( pb.get_bbit() );
	p.set_BB_OUTPUT_TYPE ( pb.get_bbot() );
	
	const NOMAD::Point & lb = pb.get_lb();
	const NOMAD::Point & ub = pb.get_ub();
	
	if ( lb.is_defined() )
		p.set_LOWER_BOUND ( lb );
	
	if ( ub.is_defined() )
		p.set_UPPER_BOUND ( ub );
	
	const std::string & bb_exe = pb.get_bb_exe();
	if ( !bb_exe.empty() )
		p.set_BB_EXE ( bb_exe );
	
	if ( _x0.size() == n && _x0.is_complete() )
		p.set_X0 ( _x0 );
	else
		p.set_X0 ( pb.get_x0() );
	
	p.set_TMP_DIR ( "/tmp" );
	if ( n <= 20 )
		p.set_DISPLAY_STATS  ( "bbe ( sol ) obj" );
	else
		p.set_DISPLAY_STATS  ( "bbe obj" );
	
	p.set_DISPLAY_DEGREE ( display ? 2 : 0   );
	// p.set_DISPLAY_DEGREE ( 3 );
    	
	p.set_ADD_SEED_TO_FILE_NAMES ( false             );
  p.set_STATS_FILE ( stats_file_name , "time bbe %fobj %ffsol" );
	
	// max number of evaluations:
	int max_bb_eval = MAX_BB_EVAL;  // Default 1
  if ( MW_DATA_PROFILE )
	  max_bb_eval=(n+1)*NB_SIMPLEX_GRAD; // Default for MW DP
  // Explicitely provided	  
	if ( _max_bb_eval > 0 ) {
		max_bb_eval = _max_bb_eval;
		if ( _times_n )
			max_bb_eval *= n;
		
		if ( max_bb_eval > MAX_BB_EVAL )
			max_bb_eval = MAX_BB_EVAL;
	}
	if ( max_bb_eval > 0 )
		p.set_MAX_BB_EVAL ( max_bb_eval );
	
	// directions type:
	p.set_DIRECTION_TYPE ( _dir_type );
	
	
	// mesh:
    p.set_ANISOTROPIC_MESH         ( _anisotropic_mesh         );
	p.set_INITIAL_MESH_INDEX       ( _initial_mesh_index       );
	p.set_MESH_UPDATE_BASIS        ( _mesh_update_basis        );
    p.set_POLL_UPDATE_BASIS			( _poll_update_basis        );
	p.set_MESH_COARSENING_EXPONENT ( _mesh_coarsening_exponent );
	p.set_MESH_REFINING_EXPONENT   ( _mesh_refining_exponent   );
	
	
	// initial mesh size:
	NOMAD::Point d0;
	compute_initial_mesh_size ( pb , d0 );
	p.set_INITIAL_MESH_SIZE ( d0 );
	
	// opportunistic evaluations:
    // C.Tribes may 14, 2015 --- modif to handle opportunistic_min_eval
	// p.set_OPPORTUNISTIC_EVAL ( _opp_eval );
    if ( _opp_eval==0 )
       p.set_OPPORTUNISTIC_EVAL ( false );
    else
    {
        p.set_OPPORTUNISTIC_EVAL ( true );
        if ( _opp_eval > 1 )
            p.set_OPPORTUNISTIC_MIN_EVAL ( _opp_eval );
    }
        
        
	
	// LH search:
	if ( _LH_search[0] > 0 || _LH_search[1] > 0 ) {
		p.set_LH_SEARCH ( _LH_search[0] , _LH_search[1] );
		p.set_OPPORTUNISTIC_LH ( _opp_LH );
	}
	
	// VNS search:
	if ( _VNS_search )
		p.set_VNS_SEARCH ( _VNS_trigger );
	
	// h norm:
	p.set_H_NORM ( _h_norm );
	
	// rho for the progressive barrier:
	p.set_RHO ( _rho );
	
	// snap to bounds:
	p.set_SNAP_TO_BOUNDS ( _snap_to_bounds );
	
	// speculative search:
	p.set_SPECULATIVE_SEARCH ( _speculative_search );
	
	// models:
	p.set_MODEL_SEARCH               ( 1 , _model_search         );
	p.set_MODEL_SEARCH               ( 2 , NOMAD::NO_MODEL       );
	
	p.set_MODEL_EVAL_SORT            ( _model_eval_sort          );
	p.set_MODEL_EVAL_SORT_CAUTIOUS   ( _model_eval_sort_cautious );
	p.set_MODEL_SEARCH_MAX_TRIAL_PTS ( _model_max_trial_pts      );
	p.set_MODEL_QUAD_MAX_Y_SIZE      ( _model_max_Y_size         );
	p.set_MODEL_SEARCH_PROJ_TO_MESH  ( _model_proj_to_mesh       );
	p.set_MODEL_SEARCH_OPTIMISTIC    ( _model_search_optimistic  );
	p.set_MODEL_QUAD_USE_WP          ( _model_use_WP             );
	p.set_MODEL_QUAD_RADIUS_FACTOR   ( _model_radius_factor      );
	
    // the eval_x is used for truth bb and sgte ---> ordering is omniscient
    p.set_HAS_SGTE ( _has_sgte );
    
#if !defined(SHORT_NOMAD_VERSION) || (SHORT_NOMAD_VERSION >= 380)
    if ( _use_granular_variables )
        p.set_MESH_TYPE( NOMAD::GMESH );
    else
        p.set_MESH_TYPE( NOMAD::XMESH );
#endif

  // C.Tribes oct 31, 2013 --- hardcoded function of user_param --> disable eval_sort
   if (_user_param==1)
//	  p.set_DISABLE_EVAL_SORT();
    
    // C.Tribes dec 17, 2013 --- hardcoded function of user_param --> sgte is free
    if (_user_param==2 || _user_param==3)
    {
//        p.set_HAS_SGTE(true);
    }
    
    if ( _user_param==4 )
    {
        p.set_DISABLE_EVAL_SORT();
    }
	
    // C.Tribes dec 17, 2013 --- hardcoded _user_param==5 for random sorting of the points (modified NOMAD compilation)
    if ( _user_param==5 )
    {

    }
    
    // C.Tribes mai 21, 2015 --- hardcoded _user_param==6 for set_OPPORTUNISTIC_MIN_NB_SUCCESS(2)
    if ( _user_param==6 )
    {
        p.set_OPPORTUNISTIC_MIN_NB_SUCCESS(2);
    }
    
    // C.Tribes dec 17, 2013 --- hardcoded _user_param==8 or _user_param==10 for Worst 2 Best sgte=truth sorting of the points (modified NOMAD compilation)
    if ( _user_param==8 || _user_param==10 )
    {
        
    }
    
    
    
    // C.Tribes hardcoded _user_param==11 for random sorting (modified nomad) + set_OPPORTUNISTIC_MIN_NB_SUCCESS(2)
    if ( _user_param==11 )
    {
        p.set_OPPORTUNISTIC_MIN_NB_SUCCESS(2);
    }
    
    
    // C.Tribes  --- hardcoded _user_param==12 for lexicographic sorting + set_OPPORTUNISTIC_MIN_NB_SUCCESS(2)
    if ( _user_param==12 )
    {
        p.set_DISABLE_EVAL_SORT();
        p.set_OPPORTUNISTIC_MIN_NB_SUCCESS(2);
    }
    
    // C.Tribes --- hardcoded _user_param==13 for sgte worst to best (modified nomad) + set_OPPORTUNISTIC_MIN_NB_SUCCESS(2)
    if ( _user_param==13 )
    {
        p.set_OPPORTUNISTIC_MIN_NB_SUCCESS(2);
    }
    
    // C.Tribes mai 21, 2015 --- hardcoded _user_param==6 for set_OPPORTUNISTIC_MIN_NB_SUCCESS(2)
    if ( _user_param==14 )
    {
        p.set_OPPORTUNISTIC_MIN_NB_SUCCESS(2);
    }
    
    // C.Tribes sept 30, 2016 --- hardcoded user_param==15 for GMesh 10 %, 20%, 40% (30% default)
    if ( _user_param==15 || _user_param==16 || _user_param==17)
    {
        
    }

  
    
       p.set_DIVERSIFICATION(_diversification) ;
    
    
	if ( _model_min_Y_size < 0 )
		p.set_MODEL_QUAD_MIN_Y_SIZE ( n + 1 );
	else
		p.set_MODEL_QUAD_MIN_Y_SIZE ( _model_min_Y_size );
}
