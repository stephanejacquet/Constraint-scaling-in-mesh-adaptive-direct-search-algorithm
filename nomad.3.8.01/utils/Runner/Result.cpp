#include "Result.hpp"

/*----------------------------------*/
/*               reset              */
/*----------------------------------*/
void Result::reset ( void )
{
    _bbe.clear();
    _obj.clear();
    _last_x.clear();
    clear_solution();
}

/*-----------------------------------*/
/*      clear solution (private)     */
/*-----------------------------------*/
void Result::clear_solution ( void )
{
    _sol_bbe    = -1;
    _sol_fx.clear();
    _sol_xe.clear();
    _sol_fxe.clear();
    _sol_surf.clear();
    _has_sol = false;
    _is_infeas = false;
}

/*----------------------------------*/
/*          read results            */
/*----------------------------------*/
bool Result::read ( std::ifstream & in , int max_bbe , bool nomad )
{
    
    reset();
    
    std::string   s,s2;
    int           bbe,bbe_plus;
    NOMAD::Double time , obj;
    bool valid=true;
    
    
    if ( nomad )
    {
        
        bool first_line = true;
        
        while ( !in.eof() )
        {
            
            valid=true;
            if ( first_line )
            {
                in >> s;
                
                // no feasible solution has been found in X evaluations:
                if ( s == "no" )
                {
                    in >> s >> s >> s >> s >> s >> s >> _sol_bbe;
                    if ( in.fail() )
                        _sol_bbe = -1;
                    else
                    {
                        if ( max_bbe > 0 && max_bbe < _sol_bbe )
                            _sol_bbe = max_bbe;
                    }
                    _bbe.push_back ( _sol_bbe );
                    _obj.push_back ( NOMAD::INF );
                    return true;
                }
                // C.Tribes feb 6, 2013 --- time is not always set correctly
                // if ( !time.atof ( s ) )
                //     return false;
                in >> bbe >> obj;
                first_line = false;
                
                getline ( in , s );
            }
            // C.Tribes oct 2, 2014 --- modif to handle VNS search ( BBE + BBE_VNS)
            else
                //before
                //in >> s >> bbe >> obj;
                //after
            {
                in >> s >> bbe >> s2;
                if ( s2.compare("+")==0 )
                {
                    in >> bbe_plus >> obj ;
                    bbe+=bbe_plus;
                }
                else
                    valid=obj.atof(s2);
            }
            
            
            // C.Tribes oct2, 2014 --- test valid conversion from above
            if ( in.fail() || !valid )
                return false;
            
            _bbe.push_back ( bbe );
            _obj.push_back ( obj );
            
            
            getline ( in , s );
            in >> std::ws;
        }
        
        _last_x = s;
    }
    
    else
    {
        
        while ( !in.eof() ) {
            
            in >> bbe;
            
            if ( in.eof() )
                break;
            
            in >> obj >> std::ws;
            
            if ( in.fail() )
                return false;
            
            _bbe.push_back ( bbe );
            _obj.push_back ( obj );
        }
    }
    
    return true;
}

/*-----------------------------------*/
/*       get the last bbe entry      */
/*-----------------------------------*/
int Result::get_last_bbe ( void ) const
{
    if ( _bbe.empty() )
        return -1;
    return _bbe[_bbe.size()-1];
}

/*-----------------------------------*/
/*       compute surf (private)      */
/*-----------------------------------*/
NOMAD::Double Result::compute_surf ( const NOMAD::Double & flb     ,
                                    int                   bbe_max   ) const
{
    
    size_t n = _bbe.size();
    if ( n != _obj.size() || n == 0 || _bbe[0] >= bbe_max )
        return NOMAD::Double();
    
    NOMAD::Double fcur   = flb - (flb-_obj[0])*bbe_max / (bbe_max - _bbe[0]) ,
    surf   = 0.0;
    int           bbecur = 0;
    
    for ( size_t i = 0 ; i < n ; ++i ) {
        
        if ( _bbe[i] > bbe_max ) {
            bbecur = _bbe[i-1];
            break;
        }
        
        surf += ( fcur - flb ) * ( _bbe[i] - bbecur );
        
        //     std::cout << "surf += ( " << fcur << " - " << flb << " ) * ( "
        // 	      << _bbe[i] << " - " << bbecur << ")" << std::endl;
        
        bbecur = _bbe[i];
        fcur   = _obj[i];
    }
    
    surf += ( fcur - flb ) * ( bbe_max - bbecur );
    
    //   std::cout << "surf += ( " << fcur << " - " << flb << " ) * ( "
    // 	    << bbe_max << " - " << bbecur << ")" << std::endl;
    
    return surf;
}

/*-----------------------------------*/
/*          compute solution         */
/*-----------------------------------*/
bool Result::compute_solution ( int                   n     ,
                               const NOMAD::Double & flb   ,
                               int                   bbe   ,
                               bool                  nomad   )
{
    
    if ( _bbe.empty() || _obj.empty() )
    {
        int sol_bbe = _sol_bbe;
        clear_solution();
        _sol_bbe = sol_bbe;
        return false;
    }
    
    clear_solution();
    
    _sol_bbe = bbe;
    
    size_t p = _bbe.size();
    
    // Test if no feasible point has been obtained
    if ( _last_x.empty() && ! _bbe.empty() && ! _obj.empty() && p==_obj.size() && _obj.back()==NOMAD::INF )
    {
        _is_infeas = true;
        return false;
    }
    
    if ( ( nomad && _last_x.empty() )          ||
        _bbe.empty()                           ||
        _obj.empty()                           ||
        p != _obj.size()                       ||
        ( _sol_bbe > 0 && _bbe[0] > _sol_bbe )    )
    {
        clear_solution();
        _sol_bbe = bbe;
        return false;
    }
    
    if ( _sol_bbe < 0 )
        _sol_bbe = _bbe[p-1];
    
    if ( nomad )
    {
        
        // get xe:
        std::list<std::string> coords;
        NOMAD::get_words ( _last_x , coords );
        
        if ( n != static_cast<int>(coords.size()) )
        {
            clear_solution();
            return false;
        }
        
        _sol_xe.resize ( n );
        n = 0;
        
        std::list<std::string>::const_iterator it , end = coords.end();
        for ( it = coords.begin() ; it != end ; ++it )
        {
            if ( !(_sol_xe[n++].atof ( *it )) )
            {
                clear_solution();
                return false;
            }
        }
        
        if ( !_sol_xe.is_complete() )
        {
            clear_solution();
            return false;
        }
    }
    
    _has_sol = true;
    _sol_fxe = _obj[p-1];
    
    // get fx and compute the 'surf' stat:
    
    _sol_fx   = _obj[0];
    _sol_surf = compute_surf ( flb , bbe );
    
    if ( p == 1 )
        return true;
    
    for ( size_t k = 1 ; k < p ; ++k )
    {
        if ( _bbe[k] > _sol_bbe )
            return true;
        _sol_fx = _obj[k];
    }
    
    _sol_bbe = _bbe[p-1];
    _sol_fx  = _sol_fxe;
    
    return true;
}

/*------------------------------------------------------*/
/*  get the solution for a given number of evaluations  */
/*------------------------------------------------------*/
NOMAD::Double Result::get_sol ( int bbe ) const
{
    NOMAD::Double cur = NOMAD::INF;
    int n = static_cast<int> ( _bbe.size() );
    for ( int k = 0 ; k < n ; ++k ) {
        if ( _bbe[k] > bbe )
            return cur;
        cur = _obj[k];
    }
    return cur;
}

/*----------------------------------*/
/*               display            */
/*----------------------------------*/
void Result::display ( const NOMAD::Display & out ) const
{
    int n = static_cast<int> ( _bbe.size() );
    for ( int k = 0 ; k < n ; ++k )
        out << _bbe[k] << " " << _obj[k] << std::endl;
}
