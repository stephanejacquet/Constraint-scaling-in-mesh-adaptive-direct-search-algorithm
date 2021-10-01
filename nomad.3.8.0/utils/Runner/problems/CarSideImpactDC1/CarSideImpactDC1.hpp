#ifndef __CARSIDEIMPACTDC1__
#define __CARSIDEIMPACTDC1__

#include "../../Problem.hpp"

class CarSideImpactDC1: public Problem {

public:

  CarSideImpactDC1 ( void );

  virtual ~CarSideImpactDC1 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

/*--------------------------------------------------*/
/*  user class to define categorical neighborhoods  */
/*--------------------------------------------------*/
class EP_CarSideImpactDC1 : public NOMAD::Extended_Poll
{
public:
    
    // constructor:
    EP_CarSideImpactDC1 ( NOMAD::Parameters & p):
    Extended_Poll ( p    ) {}
    
    // destructor:
    virtual ~EP_CarSideImpactDC1 ( void ) {}
    
    // construct the extended poll points:
    virtual void construct_extended_points ( const NOMAD::Eval_Point & );
    
};

#endif
