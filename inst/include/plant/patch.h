// -*-c++-*-
#ifndef PLANT_PLANT_PATCH_H_
#define PLANT_PLANT_PATCH_H_

#include <plant/parameters.h>
#include <plant/species.h>
#include <plant/ode_interface.h>

#include <plant/disturbance_regime.h>

using namespace Rcpp;

namespace plant {

template <typename T, typename E>
class Patch {
public:
  typedef T                 strategy_type;
  typedef E                 environment_type;
  typedef Individual<T,E>   individual_type;
  typedef Cohort<T,E>       cohort_type;
  typedef Species<T,E>      species_type;
  typedef Parameters<T,E>   parameters_type;


  Patch(parameters_type p, environment_type e, plant::Control c);

  void reset();
  size_t size() const {return species.size();}
  double time() const {return environment.time;}

  double height_max() const;

  // [eqn 11] Canopy openness at `height`
  double compute_competition(double height) const;

  void introduce_new_cohort(size_t species_index);
  void introduce_new_cohorts(const std::vector<size_t>& species_index);

  // Open to better ways to test whether cohorts have been introduced
  int cohort_ode_size() const {
    int cohort_ode_size = ode_size() - environment.ode_size();
    return(cohort_ode_size);
  }

  const species_type& at(size_t species_index) const {
    return species[species_index];
  }

  // Patch disturbance
  Disturbance_Regime* survival_weighting;

  // * ODE interface
  size_t ode_size() const;
  size_t aux_size() const;
  double ode_time() const;
  ode::const_iterator set_ode_state(ode::const_iterator it, double time);
  ode::iterator       ode_state(ode::iterator it) const;
  ode::iterator       ode_rates(ode::iterator it) const;
  ode::iterator       ode_aux(ode::iterator it) const;

  // * R interface
  // Data accessors:

  // this sucks - we couldn't get Rcpp to resolve disturbance pointers needed
  // to switch between No_Disturbance and Weibull_Disturbance safely
  std::vector<double> r_density(std::vector<double> time) const {return survival_weighting->r_density(time);}
  double r_pr_survival(double time) const {return survival_weighting->pr_survival(time);}
  double r_disturbance_mean_interval() const {return survival_weighting->r_mean_interval();}
  double r_survival_weighting_cdf(double time) const {return survival_weighting->cdf(time);}
  double r_survival_weighting_icdf(double prob) const {return survival_weighting->icdf(prob);}

  parameters_type r_parameters() const {return parameters;}
  environment_type r_environment() const {return environment;}
  std::vector<species_type> r_species() const {return species;}
  std::vector<double> r_competition_effect_error(size_t species_index) const;
  void r_set_time(double time);
  void r_set_state(double time,
                   const std::vector<double>& state,
                   const std::vector<size_t>& n,
                   const std::vector<double>& env);
  void r_introduce_new_cohort(util::index species_index) {
    introduce_new_cohort(species_index.check_bounds(size()));
  }
  species_type r_at(util::index species_index) const {
    at(species_index.check_bounds(size()));
  }
  // These are only here because they wrap private functions.
  void r_compute_environment() {compute_environment();}
  void r_compute_rates() {compute_rates();}

private:
  void compute_environment();
  void rescale_environment();
  void compute_rates();

  parameters_type parameters;
  std::vector<bool> is_resident;
  environment_type environment;
  std::vector<species_type> species;
  Control control;
};

template <typename T, typename E>
Patch<T,E>::Patch(parameters_type p, environment_type e, Control c)
  : parameters(p),
    is_resident(p.is_resident),
    environment(e),
    control(c) {
  parameters.validate();

  survival_weighting = p.disturbance;

  // Overwrite all strategy control objects so that they take the
  // patch control object.
  for (auto s : parameters.strategies) {
    s.control = control;
    species.push_back(Species<T,E>(s));
  }
  reset();
}

template <typename T, typename E>
void Patch<T,E>::reset() {
  for (auto& s : species) {
    s.clear();
  }
  environment.clear();
  compute_environment();
  compute_rates();
}

template <typename T, typename E>
double Patch<T,E>::height_max() const {
  double ret = 0.0;
  for (size_t i = 0; i < species.size(); ++i) {
    if (is_resident[i]) {
      ret = std::max(ret, species[i].height_max());
    }
  }
  return ret;
}

template <typename T, typename E>
double Patch<T,E>::compute_competition(double height) const {
  double tot = 0.0;
  for (size_t i = 0; i < species.size(); ++i) {
    if (is_resident[i]) {
      tot += species[i].compute_competition(height);
    }
  }
  return tot;
}

template <typename T, typename E>
std::vector<double> Patch<T,E>::r_competition_effect_error(size_t species_index) const {
  const double tot_competition_effect = compute_competition(0.0);
  return species[species_index].r_competition_effects_error(tot_competition_effect);
}

template <typename T, typename E>
void Patch<T,E>::compute_environment() {
  if (parameters.n_residents() > 0) {
    auto f = [&] (double x) -> double {return compute_competition(x);};
    environment.compute_environment(f, height_max());
  }
}

template <typename T, typename E>
void Patch<T,E>::rescale_environment() {
  if (parameters.n_residents() > 0) {
    auto f = [&] (double x) -> double {return compute_competition(x);};
    environment.rescale_environment(f, height_max());
  }
}

template <typename T, typename E>
void Patch<T,E>::compute_rates() {
  for (size_t i = 0; i < size(); ++i) {
    double pr_patch_survival = survival_weighting->pr_survival(time());
    double birth_rate = parameters.birth_rate[i];
    species[i].compute_rates(environment, pr_patch_survival, birth_rate);
    environment.compute_rates();
  }
}

// TODO: We should only be recomputing the light environment for the
// points that are below the height of the seedling -- not the entire
// light environment; probably worth just doing a rescale there?
template <typename T, typename E>
void Patch<T,E>::introduce_new_cohort(size_t species_index) {
  species[species_index].introduce_new_cohort();
  if (parameters.is_resident[species_index]) {
    compute_environment();
  }
}

template <typename T, typename E>
void Patch<T,E>::introduce_new_cohorts(const std::vector<size_t>& species_index) {
  bool recompute = false;
  for (size_t i : species_index) {
    species[i].introduce_new_cohort();
    recompute = recompute || parameters.is_resident[i];
  }
  if (recompute) {
    compute_environment();
  }
}


template <typename T, typename E>
void Patch<T,E>::r_set_time(double time) {
  environment.time = time;
}


// Arguments here are:
//   time: time
//   state: vector of ode state; we'll pass an iterator with that in
//   n: number of *individuals* of each species
template <typename T, typename E>
void Patch<T,E>::r_set_state(double time,
                           const std::vector<double>& state,
                           const std::vector<size_t>& n,
                           const std::vector<double>& env) {
  const size_t n_species = species.size();
  util::check_length(n.size(), n_species);
  reset();
  for (size_t i = 0; i < n_species; ++i) {
    for (size_t j = 0; j < n[i]; ++j) {
      species[i].introduce_new_cohort();
    }
  }
  util::check_length(state.size(), ode_size());
  set_ode_state(state.begin(), time);
  environment.r_init_interpolators(env);
}

// ODE interface
template <typename T, typename E>
size_t Patch<T,E>::ode_size() const {
  return ode::ode_size(species.begin(), species.end()) + environment.ode_size();
}

template <typename T, typename E>
size_t Patch<T,E>::aux_size() const {
  // no use for auxiliary environment variables (yet)
  return ode::aux_size(species.begin(), species.end());// + environment.ode_size();
}

template <typename T, typename E>
double Patch<T,E>::ode_time() const {
  return time();
}

template <typename T, typename E>
ode::const_iterator Patch<T,E>::set_ode_state(ode::const_iterator it,
                                              double time) {
  it = ode::set_ode_state(species.begin(), species.end(), it);
  it = environment.set_ode_state(it);

  environment.time = time;
  if (environment.canopy_rescale_usually) {
    rescale_environment();
  } else {
    compute_environment();
  }
  compute_rates();
  return it;
}

template <typename T, typename E>
ode::iterator Patch<T,E>::ode_state(ode::iterator it) const {
  it = ode::ode_state(species.begin(), species.end(), it);
  it = environment.ode_state(it);
  return it;
}

template <typename T, typename E>
ode::iterator Patch<T,E>::ode_rates(ode::iterator it) const {
  it = ode::ode_rates(species.begin(), species.end(), it);
  it = environment.ode_rates(it);
  return it;
}

template <typename T, typename E>
ode::iterator Patch<T,E>::ode_aux(ode::iterator it) const {
  it = ode::ode_aux(species.begin(), species.end(), it);
  //it = environment.ode_rates(it);
  return it;
}

}

#endif
