#include "Problem.hpp"

// static member initialization:
std::set<std::string> Problem::_all_keywords;

/*----------------------------------------------*/
/*                   constructor                */
/*----------------------------------------------*/
Problem::Problem ( const std::string & id           ,
                  const std::string & pb_dir       ,
                  const std::string & xe_file_name ,
                  int                 n            ,
                  int                 m              )
: _index           ( -1                          ) ,
_id              ( id                          ) ,
_pb_dir          ( PROBLEMS_DIR + pb_dir + "/" ) ,
_xe_file_name    ( xe_file_name                ) ,
_n               ( n                           ) ,
_m               ( m                           ) ,
_bbit            ( n                           ) ,
_bbot            ( m                           ) ,
_has_constraints ( false                       ) ,
_has_integers    ( false                       ) ,
_has_binaries    ( false                       ) ,
_lb              ( n                           ) ,
_ub              ( n                           ) ,
_x0              ( n                           ) ,
_xe              ( n                           )   {
    
    std::ifstream in ( (_pb_dir + xe_file_name).c_str() );
    
    
    if ( !in.fail() ) {
        try {
            in >> _xe >> _fxe;
        }
        catch ( ... ) { }
        
        if ( !_xe.is_complete() )
            _xe.clear();
        
        if ( !_fxe.is_defined() ) {
            _xe.clear();
            _fxe.clear();
        }
    }
    in.close();
}

/*----------------------------------------------*/
/*     add a keyword describing the problem     */
/*----------------------------------------------*/
void Problem::add_keyword ( std::string s ) {
    NOMAD::toupper                ( s );
    _keywords.insert              ( s );
    Problem::_all_keywords.insert ( s );
}

/*----------------------------------------------*/
/*        search for a particular keyword       */
/*----------------------------------------------*/
bool Problem::has_keyword ( const std::string & kw ) {
    std::set<std::string>::const_iterator
    it = _keywords.find ( kw );
    return ( it != _keywords.end() );
}

/*----------------------------------------------*/
/*           set blackbox input types           */
/*----------------------------------------------*/
bool Problem::set_bbit ( int i , NOMAD::bb_input_type t ) {
    
    if ( i < 0 || i >= _n )
        return false;
    
    _bbit[i] = t;
    
    
    // C.Tribes may 11, 2017 --- REMOVED TO TEST CATEGORICAL VAR WITHOUT DIM CHANGE
    //if ( t == NOMAD::CATEGORICAL ) {
    //  throw NOMAD::Exception ( "RUNNER: Problem.cpp" , __LINE__ ,
    //			     "categorical variables are not handled" );
    //    return false;
    //  }
    //  else if ( t == NOMAD::INTEGER ) {
    if ( t == NOMAD::INTEGER ) {
        _has_integers = true;
        add_keyword ( "integer_variables" );
    }
    
    else if ( t == NOMAD::BINARY ) {
        _has_binaries = true;
        add_keyword ( "binary_variables" );
    }
    
    return true;
}

/*----------------------------------------------*/
/*          set blackbox output types           */
/*----------------------------------------------*/
bool Problem::set_bbot ( int i , NOMAD::bb_output_type t ) {
    
    if ( i < 0 || i >= _m )
        return false;
    
    _bbot[i] = t;
    
    if ( NOMAD::bbot_is_constraint ( t ) ) {
        _has_constraints = true;
        add_keyword ( "constrained" );
    }
    
    return true;
}

/*----------------------------------------------*/
/*         set bb executable (batch mode)       */
/*----------------------------------------------*/
void Problem::set_bb_exe ( const std::string & s ) {
    _bb_exe = s;
    if ( is_batch() )
        add_keyword ( "batch" );
}

/*----------------------------------------------*/
/*                  set bounds                  */
/*----------------------------------------------*/
void Problem::set_bounds ( const NOMAD::Point & lb , const NOMAD::Point & ub )
{
    if ( has_bounds() )
        throw NOMAD::Exception ( "RUNNER: Problem.cpp" , __LINE__ ,
                                "Problem::set_bounds can only be called once" );
    _lb = lb;
    _ub = ub;
    
    if ( _lb.size() != _n )
        _lb = NOMAD::Point ( _n );
    
    if ( _ub.size() != _n )
        _ub = NOMAD::Point ( _n );
    
    if ( has_bounds() )
        add_keyword ( "bounded" );
}

/*------------------------------------------------------------*/
/*  get the problem name and replace '_' with '\_' for latex  */
/*------------------------------------------------------------*/
std::string Problem::get_id ( bool latex ) const {
    
    if ( !latex )
        return _id;
    
    std::string s;
    size_t n = _id.size() , k;
    for ( k = 0 ; k < n ; ++k ) {
        if ( _id[k] == '_' )
            s.push_back ( '\\' );
        s.push_back ( _id[k] );
    }
    return s;
}

/*----------------------------------------------*/
/*                    display                   */
/*----------------------------------------------*/
void Problem::display ( const NOMAD::Display & out     ,
                       int                    w_id    ,
                       int                    w_batch ,
                       int                    max_n   ,
                       int                    max_m     ) const {
    out << "["
    << std::setw(w_id) << _id << "] [n=";
    out.display_int_w ( _n , max_n );
    out << "] [m=";
    out.display_int_w ( _m , max_m );
    out << "] [bnds=" << has_bounds()
    << "] [cstr=" << _has_constraints
    << "] [int=" << _has_integers
    << "] [bin=" << _has_binaries << "] ["
    << std::setw(w_batch) << ( is_batch() ? "batch" : "lib" )
    << "] [f*=" << _fxe << "]";
}

/*----------------------------------------------*/
/*       display keywords (static, private)     */
/*----------------------------------------------*/
void Problem::display_keywords ( const std::set<std::string> & keywords ,
                                const NOMAD::Display        & out        ) {
    size_t n = keywords.size();
    
    if ( n == 0 ) {
        out << "no keywords" << std::endl;
        return;
    }
    
    std::ostringstream msg;
    msg << "keywords(" << n << ")";
    out.open_block ( msg.str() );
    std::set<std::string>::const_iterator it , end = keywords.end();
    int i = 1;
    for ( it = keywords.begin() ; it != end ; ++it , ++i ) {
        out.display_int_w ( i , n );
        out << ": " << *it << std::endl;
    }
    out.close_block();
}

/*----------------------------------------------*/
/*         update the best known solution       */
/*----------------------------------------------*/
bool Problem::update_xe ( const NOMAD::Point  & xe  ,
                         const NOMAD::Double & fxe   ) {
    
    if ( xe.size() != _n || !xe.is_complete() || !fxe.is_defined() )
        return false;
    
    if ( !_fxe.is_defined() || fxe < _fxe ) {
        _xe  = xe;
        _fxe = fxe;
        
        std::string   xe_file_name = _pb_dir + _xe_file_name;
        std::ofstream out ( xe_file_name.c_str() );
        
        out.setf      ( std::ios::fixed );
        out.precision ( NOMAD::DISPLAY_PRECISION_BB );
        
        xe.display ( out , " " , -1 , -1 );
        
        out << std::endl << std::endl << fxe << std::endl;
        
        out.close();
        
        if ( out.fail() )
            std::cerr << "Warning: cannot update best known solution "
            << "for problem " << get_id() << std::endl;
        
        return true;
    }
    return false;
}

/*--------------------------------------------------------*/
/*                     test an evaluation                 */
/*--------------------------------------------------------*/
/* example:                                               */
/*                                                        */
/*   NOMAD::Point fx;                                     */
/*                                                        */
/*   pb.test_eval_x ( pb.get_x0() , fx );                 */
/*   out << "f(x0)=[ " << fx << " ]" << std::endl;        */
/*                                                        */
/*   pb.test_eval_x ( pb.get_xe() , fx );                 */
/*   out << "f(xe)=[ " << fx << " ]" << std::endl;        */
/*                                                        */
/*   See more complete example at the end of Problem.hpp  */
/*                                                        */
/*--------------------------------------------------------*/
bool Problem::test_eval_x ( const NOMAD::Point & x  ,
                           NOMAD::Point       & fx   ) const {
    fx.clear();
    if ( x.size() != _n || !x.is_complete() || is_batch() )
        return false;
    
    NOMAD::Eval_Point y ( _n , _m );
    y.NOMAD::Point::operator = ( x );
    
    bool count_eval;
    bool eval_ok = eval_x ( y , count_eval );
    
    if ( eval_ok )
        fx = y.get_bb_outputs();
    
    return eval_ok;
}
