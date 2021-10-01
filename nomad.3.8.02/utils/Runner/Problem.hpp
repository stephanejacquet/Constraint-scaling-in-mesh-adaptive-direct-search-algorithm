#ifndef __PROBLEM__
#define __PROBLEM__

#include "runner_defines.hpp"

class Problem : public NOMAD::Uncopyable {

private:

  int                                _index;
  std::string                        _id;
  std::string                        _pb_dir;
  std::string                        _xe_file_name;
  int                                _n;
  int                                _m;
  std::string                        _bb_exe;
  std::vector<NOMAD::bb_input_type > _bbit;
  std::vector<NOMAD::bb_output_type> _bbot;
  bool                               _has_constraints;
  bool                               _has_integers;
  bool                               _has_binaries;
  NOMAD::Point                       _lb;
  NOMAD::Point                       _ub;
  NOMAD::Point                       _x0;
  NOMAD::Point                       _xe;
  NOMAD::Double                      _fxe;  // best known solution
  NOMAD::Double                      _f_lb; // lower bound for f (f_lb <= fxe)

  std::set<std::string>              _keywords;
  static std::set<std::string>       _all_keywords;

  // display keywords:
  static void display_keywords ( const std::set<std::string> & keywords ,
				 const NOMAD::Display        & out        );
public:

  // constructor:
  Problem ( const std::string & id           ,
	    const std::string & pb_dir       ,
	    const std::string & xe_file_name ,
	    int                 n            ,
	    int                 m              );

  // destructor:
  virtual ~Problem ( void ) {}

  // evaluation:
  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
			bool              & count_eval   ) const
  { return false; }

  // test an evaluation (example given at the end of this header file):
  bool test_eval_x ( const NOMAD::Point & x , NOMAD::Point & fx ) const;

  // display:
  void display ( const NOMAD::Display & out          ,
		 int                    w_id    = -1 ,
		 int                    w_batch = -1 ,
		 int                    max_n   = -1 ,
		 int                    max_m   = -1   ) const;
  
  void display_keywords ( const NOMAD::Display & out ) const {
    Problem::display_keywords ( _keywords , out );
  }

  static void display_all_keywords ( const NOMAD::Display & out ) {
    Problem::display_keywords ( Problem::_all_keywords , out );
  }

  // GET methods:
  std::string         get_id        ( bool latex = false ) const;
  int                 get_index     ( void ) const { return _index;            }
  const std::string & get_pb_dir    ( void ) const { return _pb_dir;           }
  std::string         get_tests_dir ( void ) const { return _pb_dir+TESTS_DIR; }
  int                 get_n         ( void ) const { return _n;                }
  int                 get_m         ( void ) const { return _m;                }
  const std::string   & get_bb_exe  ( void ) const { return _bb_exe;           }
  const NOMAD::Point  & get_x0      ( void ) const { return _x0;               }
  const NOMAD::Point  & get_xe      ( void ) const { return _xe;               }
  const NOMAD::Double & get_fxe     ( void ) const { return _fxe;              }
  const NOMAD::Point  & get_lb      ( void ) const { return _lb;               }
  const NOMAD::Point  & get_ub      ( void ) const { return _ub;               }
  const NOMAD::Double & get_f_lb    ( void ) const { return _f_lb;             }

  const std::vector<NOMAD::bb_input_type > & get_bbit ( void ) const
  { return _bbit; }

  const std::vector<NOMAD::bb_output_type> & get_bbot ( void ) const
  { return _bbot; }

  bool has_bounds      ( void ) const { return _lb.is_defined() ||
                                               _ub.is_defined();   }
  bool has_constraints ( void ) const { return _has_constraints;   }

  bool has_keyword ( const std::string & kw );

  bool is_batch ( void ) const { return !_bb_exe.empty(); }

  // SET methods:

  void set_index ( int i ) { _index = i; }

  bool update_xe ( const NOMAD::Point  & xe  ,
		   const NOMAD::Double & fxe   );
protected:

  void set_bb_exe ( const std::string & s );

  bool set_bbit ( int i , NOMAD::bb_input_type t );

  bool set_bbot ( int i , NOMAD::bb_output_type t );

  void set_f_lb ( const NOMAD::Double & f ) { _f_lb = f; }

  void set_x0 ( const NOMAD::Point & x ) { _x0 = x; }

  void set_bounds ( const NOMAD::Point & lb , const NOMAD::Point & ub );

  void add_keyword ( std::string s );
};

#endif


// code for testing a given problem:
// ---------------------------------
//   if ( rank == 0 ) {

//     Mdo pb;

//     NOMAD::Point fx;

//     out << std::endl << pb.get_id() << " TEST:" << std::endl << std::endl;

//     out << "x0=( " << pb.get_x0() << " )" << std::endl;
//     out << "xe=( " << pb.get_xe() << " )" << std::endl;

//     out << std::endl;

//     pb.test_eval_x ( pb.get_x0() , fx );
//     out << "f(x0)=[ " << fx << " ]" << std::endl;

//     pb.test_eval_x ( pb.get_xe() , fx );
//     out << "f(xe)=[ " << fx << " ]" << std::endl;

//     out << std::endl;
//   }
