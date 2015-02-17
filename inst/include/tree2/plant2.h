// -*-c++-*-
#ifndef TREE_PLANT2_H_
#define TREE_PLANT2_H_

#include <memory> // std::shared_ptr
#include <vector>
#include <tree2/strategy.h>
#include <tree2/plant_internals.h>
#include <tree2/ode_interface.h>

namespace tree2 {

struct Plant2_internals {
  Plant2_internals()
    :
    height(NA_REAL),
    height_rate(NA_REAL),
    mortality(0.0),
    mortality_rate(NA_REAL),
    fecundity(0.0),
    fecundity_rate(NA_REAL) {
  }
  double height;
  double leaf_area;
  double height_rate;
  double mortality;
  double mortality_rate;
  double fecundity;
  double fecundity_rate;
};

class Plant2 {
public:
  typedef Strategy                       strategy_type;
  typedef std::shared_ptr<strategy_type> strategy_ptr_type;
  Plant2(strategy_ptr_type s)
    : strategy(s) {
    set_height(strategy->height_0);
  }

  double height() const {return vars.height;}
  double height_rate() const {return vars.height_rate;}
  void set_height(double x) {
    vars.height    = x;
    vars.leaf_area = strategy->leaf_area(x);
  }

  double mortality() const {return vars.mortality;}
  double mortality_rate() const {return vars.mortality_rate;}
  void set_mortality(double x) {vars.mortality = x;}

  double fecundity() const {return vars.fecundity;}
  double fecundity_rate() const {return vars.fecundity_rate;}
  void set_fecundity(double x) {vars.fecundity = x;}

  double leaf_area_above(double z) const {
    return strategy->leaf_area_above(z, vars.height, vars.leaf_area);
  }

  void compute_vars_phys(const Environment& environment, bool
                           reuse_intervals=false);
  double germination_probability(const Environment& environment) {
    return strategy->germination_probability(environment);
  }

  // * ODE interface
  static size_t       ode_size() {return 3;}
  ode::const_iterator set_ode_state(ode::const_iterator it);
  ode::iterator       ode_state(ode::iterator it) const;
  ode::iterator       ode_rates(ode::iterator it) const;

  // * R interface
  strategy_type r_get_strategy() const {return *strategy.get();}
  Plant2_internals r_internals() const {return vars;}
  const Control& control() const {return strategy->control;}

private:
  strategy_ptr_type strategy;
  Plant2_internals vars;
};

Plant2 make_plant2(Plant2::strategy_type s);

}

#endif
