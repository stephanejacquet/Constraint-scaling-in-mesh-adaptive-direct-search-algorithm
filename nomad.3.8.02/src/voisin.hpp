/* 
 * File:   voisin.hpp
 * Author: jacqstep
 *
 * Created on 21 février 2017, 14:26
 */

#ifndef VOISIN_HPP
#define	VOISIN_HPP
#include "Double.hpp"
class Voisin
{
  public:
    int			origin;
    
    NOMAD::Double 	distance; 	//right side of violated Inequality
		
    
    Voisin(){};
    
    //bool operator< (const Voisin&) const;
  
};



bool Voisin::operator<(const Voisin& c1) const{
  return (distance <= c1.distance);	
}

#endif	/* VOISIN_HPP */

