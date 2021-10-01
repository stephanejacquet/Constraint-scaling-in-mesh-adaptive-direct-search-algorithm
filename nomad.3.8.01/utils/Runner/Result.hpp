#ifndef __RESULT__
#define __RESULT__

#include "runner_defines.hpp"

class Result : public NOMAD::Uncopyable
{
    
private:
    
    std::vector<int>           _bbe;
    std::vector<NOMAD::Double> _obj;
    
    std::string                _last_x;
    
    bool                       _has_sol;
    bool                       _is_infeas;
    int                        _sol_bbe;
    NOMAD::Double              _sol_surf;
    NOMAD::Double              _sol_fx;
    NOMAD::Double              _sol_fxe;
    NOMAD::Point               _sol_xe;
    
    // clear solution:
    void clear_solution ( void );
    
    // compute surf:
    NOMAD::Double compute_surf ( const NOMAD::Double & flb     ,
                                int                   bbe_max   ) const;
    
public:
    
    // constructor:
    Result ( void )
    {
        clear_solution();
    }
    
    // destructor:
    virtual ~Result ( void ) {}
    
    // reset:
    void reset ( void );
    
    // compute solution:
    bool compute_solution ( int                   n     ,
                           const NOMAD::Double & flb   ,
                           int                   bbe   ,
                           bool                  nomad   );
    
    // read results:
    bool read ( std::ifstream & in , int max_bbe , bool nomad );
    
    // GET methods:
    int                   get_last_bbe   ( void    ) const;
    bool                  has_solution   ( void    ) const { return _has_sol;    }
    bool                  is_infeas      ( void    ) const { return _is_infeas;  }
    int                   get_sol_bbe    ( void    ) const { return _sol_bbe;    }
    const NOMAD::Double & get_sol_surf   ( void    ) const { return _sol_surf;   }
    const NOMAD::Double & get_sol_fx     ( void    ) const { return _sol_fx;     }
    const NOMAD::Double & get_first_fx   ( void    ) const { return _obj[0];     }
    const NOMAD::Double & get_sol_fxe    ( void    ) const { return _sol_fxe;    }
    const NOMAD::Point  & get_sol_xe     ( void    ) const { return _sol_xe;     }
    NOMAD::Double         get_sol        ( int bbe ) const;
    
    // display:
    void display ( const NOMAD::Display & out ) const;
    
    // comparison operator:
    bool operator == ( const Result & r )
    {
        return _bbe == r._bbe && _obj == r._obj;
    }
};

#endif
