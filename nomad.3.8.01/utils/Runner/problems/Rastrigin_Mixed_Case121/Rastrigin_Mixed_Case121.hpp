#ifndef __RASTRIGINMIXEDCASE121__
#define __RASTRIGINMIXEDCASE121__

#include "../../Problem.hpp"

class Rastrigin_Mixed_Case121: public Problem {

public:

  Rastrigin_Mixed_Case121 ( void );

  virtual ~Rastrigin_Mixed_Case121 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

/*--------------------------------------------------*/
/*  user class to define categorical neighborhoods  */
/*--------------------------------------------------*/
class EP_Rastrigin_Mixed_Case121 : public NOMAD::Extended_Poll
{
public:
    
    // constructor:
    EP_Rastrigin_Mixed_Case121 ( NOMAD::Parameters & p):
    NOMAD::Extended_Poll ( p    ) {}
    
    // destructor:
    virtual ~EP_Rastrigin_Mixed_Case121 ( void ) {}
    
    // construct the extended poll points:
    virtual void construct_extended_points ( const NOMAD::Eval_Point & );
    
};

#endif
