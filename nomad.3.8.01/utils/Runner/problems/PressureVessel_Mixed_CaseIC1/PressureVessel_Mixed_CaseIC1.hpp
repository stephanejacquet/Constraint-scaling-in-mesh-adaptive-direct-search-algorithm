#ifndef __PRESSUREVESSELMIXEDCASEIC1__
#define __PRESSUREVESSELMIXEDCASEIC1__

#include "../../Problem.hpp"

class PressureVessel_Mixed_CaseIC1: public Problem {

public:

  PressureVessel_Mixed_CaseIC1 ( void );

  virtual ~PressureVessel_Mixed_CaseIC1 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

/*--------------------------------------------------*/
/*  user class to define categorical neighborhoods  */
/*--------------------------------------------------*/
class EP_PressureVessel_Mixed_CaseIC1 : public NOMAD::Extended_Poll
{
public:
    
    // constructor:
    EP_PressureVessel_Mixed_CaseIC1 ( NOMAD::Parameters & p):
    NOMAD::Extended_Poll ( p    ) {}
    
    // destructor:
    virtual ~EP_PressureVessel_Mixed_CaseIC1 ( void ) {}
    
    // construct the extended poll points:
    virtual void construct_extended_points ( const NOMAD::Eval_Point & );
    
};

#endif
