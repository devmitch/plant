// Generated by RcppR6 (0.2.1): do not edit by hand
#include <tree2.h>

// [[Rcpp::export]]
tree2::ode::test::Lorenz Lorenz__ctor(double sigma, double R, double b) {
  return tree2::ode::test::Lorenz(sigma, R, b);
}
// [[Rcpp::export]]
size_t Lorenz__ode_size__get(tree2::RcppR6::RcppR6<tree2::ode::test::Lorenz> obj_) {
  return obj_->ode_size();
}

// [[Rcpp::export]]
tree2::ode::state_type Lorenz__ode_state__get(tree2::RcppR6::RcppR6<tree2::ode::test::Lorenz> obj_) {
  return tree2::ode::r_ode_state(*obj_);
}
// [[Rcpp::export]]
void Lorenz__ode_state__set(tree2::RcppR6::RcppR6<tree2::ode::test::Lorenz> obj_, tree2::ode::state_type value) {
  tree2::ode::r_set_ode_state(*obj_, value);
}

// [[Rcpp::export]]
tree2::ode::state_type Lorenz__ode_rates__get(tree2::RcppR6::RcppR6<tree2::ode::test::Lorenz> obj_) {
  return tree2::ode::r_ode_rates(*obj_);
}

// [[Rcpp::export]]
std::vector<double> Lorenz__pars__get(tree2::RcppR6::RcppR6<tree2::ode::test::Lorenz> obj_) {
  return obj_->pars();
}


// [[Rcpp::export]]
tree2::ode::test::OdeR OdeR__ctor(Rcpp::Function derivs, Rcpp::Function state, double time) {
  return tree2::ode::test::OdeR(derivs, state, time);
}
// [[Rcpp::export]]
void OdeR__update_state(tree2::RcppR6::RcppR6<tree2::ode::test::OdeR> obj_) {
  obj_->update_state();
}

// [[Rcpp::export]]
tree2::ode::Runner<tree2::ode::test::Lorenz> OdeRunner___Lorenz__ctor(tree2::ode::test::Lorenz obj, tree2::ode::OdeControl control) {
  return tree2::ode::Runner<tree2::ode::test::Lorenz>(obj, control);
}
// [[Rcpp::export]]
void OdeRunner___Lorenz__advance(tree2::RcppR6::RcppR6<tree2::ode::Runner<tree2::ode::test::Lorenz> > obj_, double time) {
  obj_->advance(time);
}
// [[Rcpp::export]]
void OdeRunner___Lorenz__advance_fixed(tree2::RcppR6::RcppR6<tree2::ode::Runner<tree2::ode::test::Lorenz> > obj_, std::vector<double> time) {
  obj_->advance_fixed(time);
}
// [[Rcpp::export]]
void OdeRunner___Lorenz__step(tree2::RcppR6::RcppR6<tree2::ode::Runner<tree2::ode::test::Lorenz> > obj_) {
  obj_->step();
}
// [[Rcpp::export]]
void OdeRunner___Lorenz__step_to(tree2::RcppR6::RcppR6<tree2::ode::Runner<tree2::ode::test::Lorenz> > obj_, double time) {
  obj_->step_to(time);
}
// [[Rcpp::export]]
void OdeRunner___Lorenz__set_state(tree2::RcppR6::RcppR6<tree2::ode::Runner<tree2::ode::test::Lorenz> > obj_, tree2::ode::state_type y, double time) {
  obj_->set_state(y, time);
}
// [[Rcpp::export]]
void OdeRunner___Lorenz__set_state_from_system(tree2::RcppR6::RcppR6<tree2::ode::Runner<tree2::ode::test::Lorenz> > obj_) {
  obj_->set_state_from_system();
}
// [[Rcpp::export]]
double OdeRunner___Lorenz__time__get(tree2::RcppR6::RcppR6<tree2::ode::Runner<tree2::ode::test::Lorenz> > obj_) {
  return obj_->time();
}

// [[Rcpp::export]]
tree2::ode::state_type OdeRunner___Lorenz__state__get(tree2::RcppR6::RcppR6<tree2::ode::Runner<tree2::ode::test::Lorenz> > obj_) {
  return obj_->state();
}

// [[Rcpp::export]]
std::vector<double> OdeRunner___Lorenz__times__get(tree2::RcppR6::RcppR6<tree2::ode::Runner<tree2::ode::test::Lorenz> > obj_) {
  return obj_->times();
}

// [[Rcpp::export]]
tree2::ode::test::Lorenz OdeRunner___Lorenz__object__get(tree2::RcppR6::RcppR6<tree2::ode::Runner<tree2::ode::test::Lorenz> > obj_) {
  return obj_->object();
}


// [[Rcpp::export]]
tree2::ode::Runner<tree2::ode::test::OdeR> OdeRunner___OdeR__ctor(tree2::ode::test::OdeR obj, tree2::ode::OdeControl control) {
  return tree2::ode::Runner<tree2::ode::test::OdeR>(obj, control);
}
// [[Rcpp::export]]
void OdeRunner___OdeR__advance(tree2::RcppR6::RcppR6<tree2::ode::Runner<tree2::ode::test::OdeR> > obj_, double time) {
  obj_->advance(time);
}
// [[Rcpp::export]]
void OdeRunner___OdeR__advance_fixed(tree2::RcppR6::RcppR6<tree2::ode::Runner<tree2::ode::test::OdeR> > obj_, std::vector<double> time) {
  obj_->advance_fixed(time);
}
// [[Rcpp::export]]
void OdeRunner___OdeR__step(tree2::RcppR6::RcppR6<tree2::ode::Runner<tree2::ode::test::OdeR> > obj_) {
  obj_->step();
}
// [[Rcpp::export]]
void OdeRunner___OdeR__step_to(tree2::RcppR6::RcppR6<tree2::ode::Runner<tree2::ode::test::OdeR> > obj_, double time) {
  obj_->step_to(time);
}
// [[Rcpp::export]]
void OdeRunner___OdeR__set_state(tree2::RcppR6::RcppR6<tree2::ode::Runner<tree2::ode::test::OdeR> > obj_, tree2::ode::state_type y, double time) {
  obj_->set_state(y, time);
}
// [[Rcpp::export]]
void OdeRunner___OdeR__set_state_from_system(tree2::RcppR6::RcppR6<tree2::ode::Runner<tree2::ode::test::OdeR> > obj_) {
  obj_->set_state_from_system();
}
// [[Rcpp::export]]
double OdeRunner___OdeR__time__get(tree2::RcppR6::RcppR6<tree2::ode::Runner<tree2::ode::test::OdeR> > obj_) {
  return obj_->time();
}

// [[Rcpp::export]]
tree2::ode::state_type OdeRunner___OdeR__state__get(tree2::RcppR6::RcppR6<tree2::ode::Runner<tree2::ode::test::OdeR> > obj_) {
  return obj_->state();
}

// [[Rcpp::export]]
std::vector<double> OdeRunner___OdeR__times__get(tree2::RcppR6::RcppR6<tree2::ode::Runner<tree2::ode::test::OdeR> > obj_) {
  return obj_->times();
}

// [[Rcpp::export]]
tree2::ode::test::OdeR OdeRunner___OdeR__object__get(tree2::RcppR6::RcppR6<tree2::ode::Runner<tree2::ode::test::OdeR> > obj_) {
  return obj_->object();
}


// [[Rcpp::export]]
tree2::ode::Runner<tree2::tools::PlantRunner> OdeRunner___PlantRunner__ctor(tree2::tools::PlantRunner obj, tree2::ode::OdeControl control) {
  return tree2::ode::Runner<tree2::tools::PlantRunner>(obj, control);
}
// [[Rcpp::export]]
void OdeRunner___PlantRunner__advance(tree2::RcppR6::RcppR6<tree2::ode::Runner<tree2::tools::PlantRunner> > obj_, double time) {
  obj_->advance(time);
}
// [[Rcpp::export]]
void OdeRunner___PlantRunner__advance_fixed(tree2::RcppR6::RcppR6<tree2::ode::Runner<tree2::tools::PlantRunner> > obj_, std::vector<double> time) {
  obj_->advance_fixed(time);
}
// [[Rcpp::export]]
void OdeRunner___PlantRunner__step(tree2::RcppR6::RcppR6<tree2::ode::Runner<tree2::tools::PlantRunner> > obj_) {
  obj_->step();
}
// [[Rcpp::export]]
void OdeRunner___PlantRunner__step_to(tree2::RcppR6::RcppR6<tree2::ode::Runner<tree2::tools::PlantRunner> > obj_, double time) {
  obj_->step_to(time);
}
// [[Rcpp::export]]
void OdeRunner___PlantRunner__set_state(tree2::RcppR6::RcppR6<tree2::ode::Runner<tree2::tools::PlantRunner> > obj_, tree2::ode::state_type y, double time) {
  obj_->set_state(y, time);
}
// [[Rcpp::export]]
void OdeRunner___PlantRunner__set_state_from_system(tree2::RcppR6::RcppR6<tree2::ode::Runner<tree2::tools::PlantRunner> > obj_) {
  obj_->set_state_from_system();
}
// [[Rcpp::export]]
double OdeRunner___PlantRunner__time__get(tree2::RcppR6::RcppR6<tree2::ode::Runner<tree2::tools::PlantRunner> > obj_) {
  return obj_->time();
}

// [[Rcpp::export]]
tree2::ode::state_type OdeRunner___PlantRunner__state__get(tree2::RcppR6::RcppR6<tree2::ode::Runner<tree2::tools::PlantRunner> > obj_) {
  return obj_->state();
}

// [[Rcpp::export]]
std::vector<double> OdeRunner___PlantRunner__times__get(tree2::RcppR6::RcppR6<tree2::ode::Runner<tree2::tools::PlantRunner> > obj_) {
  return obj_->times();
}

// [[Rcpp::export]]
tree2::tools::PlantRunner OdeRunner___PlantRunner__object__get(tree2::RcppR6::RcppR6<tree2::ode::Runner<tree2::tools::PlantRunner> > obj_) {
  return obj_->object();
}


// [[Rcpp::export]]
tree2::CohortScheduleEvent CohortScheduleEvent__ctor(double introduction, tree2::util::index species_index) {
  return tree2::CohortScheduleEvent(introduction, species_index);
}
// [[Rcpp::export]]
tree2::util::index CohortScheduleEvent__species_index__get(tree2::RcppR6::RcppR6<tree2::CohortScheduleEvent> obj_) {
  return obj_->species_index;
}
// [[Rcpp::export]]
void CohortScheduleEvent__species_index__set(tree2::RcppR6::RcppR6<tree2::CohortScheduleEvent> obj_, tree2::util::index value) {
  obj_->species_index = value;
}

// [[Rcpp::export]]
std::vector<double> CohortScheduleEvent__times__get(tree2::RcppR6::RcppR6<tree2::CohortScheduleEvent> obj_) {
  return obj_->times;
}

// [[Rcpp::export]]
double CohortScheduleEvent__time_introduction__get(tree2::RcppR6::RcppR6<tree2::CohortScheduleEvent> obj_) {
  return obj_->time_introduction();
}

// [[Rcpp::export]]
double CohortScheduleEvent__time_end__get(tree2::RcppR6::RcppR6<tree2::CohortScheduleEvent> obj_) {
  return obj_->time_end();
}

// [[Rcpp::export]]
size_t CohortScheduleEvent__species_index_raw__get(tree2::RcppR6::RcppR6<tree2::CohortScheduleEvent> obj_) {
  return obj_->species_index_raw();
}


// [[Rcpp::export]]
tree2::CohortSchedule CohortSchedule__ctor(size_t n_species) {
  return tree2::CohortSchedule(n_species);
}
// [[Rcpp::export]]
tree2::CohortSchedule CohortSchedule__expand(tree2::RcppR6::RcppR6<tree2::CohortSchedule> obj_, size_t n_extra, std::vector<double> times) {
  return obj_->expand(n_extra, times);
}
// [[Rcpp::export]]
void CohortSchedule__clear_times(tree2::RcppR6::RcppR6<tree2::CohortSchedule> obj_, tree2::util::index species_index) {
  obj_->clear_times(species_index);
}
// [[Rcpp::export]]
void CohortSchedule__clear_ode_times(tree2::RcppR6::RcppR6<tree2::CohortSchedule> obj_) {
  obj_->r_clear_ode_times();
}
// [[Rcpp::export]]
void CohortSchedule__set_times(tree2::RcppR6::RcppR6<tree2::CohortSchedule> obj_, std::vector<double> times, tree2::util::index species_index) {
  obj_->r_set_times(times, species_index);
}
// [[Rcpp::export]]
std::vector<double> CohortSchedule__times(tree2::RcppR6::RcppR6<tree2::CohortSchedule> obj_, tree2::util::index species_index) {
  return obj_->r_times(species_index);
}
// [[Rcpp::export]]
void CohortSchedule__reset(tree2::RcppR6::RcppR6<tree2::CohortSchedule> obj_) {
  obj_->reset();
}
// [[Rcpp::export]]
void CohortSchedule__pop(tree2::RcppR6::RcppR6<tree2::CohortSchedule> obj_) {
  obj_->pop();
}
// [[Rcpp::export]]
tree2::CohortSchedule CohortSchedule__copy(tree2::RcppR6::RcppR6<tree2::CohortSchedule> obj_) {
  return obj_->r_copy();
}
// [[Rcpp::export]]
size_t CohortSchedule__size__get(tree2::RcppR6::RcppR6<tree2::CohortSchedule> obj_) {
  return obj_->size();
}

// [[Rcpp::export]]
size_t CohortSchedule__n_species__get(tree2::RcppR6::RcppR6<tree2::CohortSchedule> obj_) {
  return obj_->get_n_species();
}

// [[Rcpp::export]]
tree2::CohortScheduleEvent CohortSchedule__next_event__get(tree2::RcppR6::RcppR6<tree2::CohortSchedule> obj_) {
  return obj_->next_event();
}

// [[Rcpp::export]]
size_t CohortSchedule__remaining__get(tree2::RcppR6::RcppR6<tree2::CohortSchedule> obj_) {
  return obj_->remaining();
}

// [[Rcpp::export]]
double CohortSchedule__max_time__get(tree2::RcppR6::RcppR6<tree2::CohortSchedule> obj_) {
  return obj_->get_max_time();
}
// [[Rcpp::export]]
void CohortSchedule__max_time__set(tree2::RcppR6::RcppR6<tree2::CohortSchedule> obj_, double value) {
  obj_->r_set_max_time(value);
}

// [[Rcpp::export]]
std::vector<double> CohortSchedule__ode_times__get(tree2::RcppR6::RcppR6<tree2::CohortSchedule> obj_) {
  return obj_->r_ode_times();
}
// [[Rcpp::export]]
void CohortSchedule__ode_times__set(tree2::RcppR6::RcppR6<tree2::CohortSchedule> obj_, std::vector<double> value) {
  obj_->r_set_ode_times(value);
}

// [[Rcpp::export]]
bool CohortSchedule__use_ode_times__get(tree2::RcppR6::RcppR6<tree2::CohortSchedule> obj_) {
  return obj_->using_ode_times();
}
// [[Rcpp::export]]
void CohortSchedule__use_ode_times__set(tree2::RcppR6::RcppR6<tree2::CohortSchedule> obj_, bool value) {
  obj_->r_set_use_ode_times(value);
}

// [[Rcpp::export]]
SEXP CohortSchedule__all_times__get(tree2::RcppR6::RcppR6<tree2::CohortSchedule> obj_) {
  return obj_->r_all_times();
}
// [[Rcpp::export]]
void CohortSchedule__all_times__set(tree2::RcppR6::RcppR6<tree2::CohortSchedule> obj_, SEXP value) {
  obj_->r_set_all_times(value);
}


// [[Rcpp::export]]
tree2::Disturbance Disturbance__ctor(double mean_interval) {
  return tree2::Disturbance(mean_interval);
}
// [[Rcpp::export]]
double Disturbance__density(tree2::RcppR6::RcppR6<tree2::Disturbance> obj_, double time) {
  return obj_->density(time);
}
// [[Rcpp::export]]
double Disturbance__pr_survival(tree2::RcppR6::RcppR6<tree2::Disturbance> obj_, double time) {
  return obj_->pr_survival(time);
}
// [[Rcpp::export]]
double Disturbance__pr_survival_conditional(tree2::RcppR6::RcppR6<tree2::Disturbance> obj_, double time, double time_start) {
  return obj_->pr_survival_conditional(time, time_start);
}
// [[Rcpp::export]]
double Disturbance__cdf(tree2::RcppR6::RcppR6<tree2::Disturbance> obj_, double time) {
  return obj_->cdf(time);
}
// [[Rcpp::export]]
double Disturbance__mean_interval__get(tree2::RcppR6::RcppR6<tree2::Disturbance> obj_) {
  return obj_->r_mean_interval();
}


// [[Rcpp::export]]
SEXP Control__ctor() {
  return Rcpp::wrap(tree2::Control());
}


// [[Rcpp::export]]
SEXP OdeControl__ctor() {
  return Rcpp::wrap(tree2::ode::OdeControl());
}


// [[Rcpp::export]]
SEXP Strategy__ctor() {
  return Rcpp::wrap(tree2::Strategy());
}


// [[Rcpp::export]]
SEXP Parameters__ctor() {
  return Rcpp::wrap(tree2::Parameters());
}

// [[Rcpp::export]]
SEXP Parameters__vdor(SEXP obj) {
  return Rcpp::wrap(Rcpp::as<tree2::Parameters>(obj));
}


// [[Rcpp::export]]
tree2::quadrature::QK QK__ctor(size_t rule) {
  return tree2::quadrature::QK(rule);
}
// [[Rcpp::export]]
double QK__integrate(tree2::RcppR6::RcppR6<tree2::quadrature::QK> obj_, SEXP f, double a, double b) {
  return obj_->r_integrate(f, a, b);
}
// [[Rcpp::export]]
std::vector<double> QK__integrate_vector_x(tree2::RcppR6::RcppR6<tree2::quadrature::QK> obj_, double a, double b) {
  return obj_->integrate_vector_x(a, b);
}
// [[Rcpp::export]]
double QK__integrate_vector(tree2::RcppR6::RcppR6<tree2::quadrature::QK> obj_, std::vector<double> y, double a, double b) {
  return obj_->integrate_vector(y, a, b);
}
// [[Rcpp::export]]
double QK__last_area__get(tree2::RcppR6::RcppR6<tree2::quadrature::QK> obj_) {
  return obj_->get_last_area();
}

// [[Rcpp::export]]
double QK__last_error__get(tree2::RcppR6::RcppR6<tree2::quadrature::QK> obj_) {
  return obj_->get_last_error();
}

// [[Rcpp::export]]
double QK__last_area_abs__get(tree2::RcppR6::RcppR6<tree2::quadrature::QK> obj_) {
  return obj_->get_last_area_abs();
}

// [[Rcpp::export]]
double QK__last_area_asc__get(tree2::RcppR6::RcppR6<tree2::quadrature::QK> obj_) {
  return obj_->get_last_area_asc();
}


// [[Rcpp::export]]
tree2::quadrature::QAG QAG__ctor(size_t rule, size_t max_iterations, double atol, double rtol) {
  return tree2::quadrature::QAG(rule, max_iterations, atol, rtol);
}
// [[Rcpp::export]]
double QAG__integrate(tree2::RcppR6::RcppR6<tree2::quadrature::QAG> obj_, SEXP f, double a, double b) {
  return obj_->r_integrate(f, a, b);
}
// [[Rcpp::export]]
double QAG__integrate_with_intervals(tree2::RcppR6::RcppR6<tree2::quadrature::QAG> obj_, SEXP f, SEXP intervals) {
  return obj_->r_integrate_with_intervals(f, intervals);
}
// [[Rcpp::export]]
double QAG__integrate_with_last_intervals(tree2::RcppR6::RcppR6<tree2::quadrature::QAG> obj_, SEXP f, double a, double b) {
  return obj_->r_integrate_with_last_intervals(f, a, b);
}
// [[Rcpp::export]]
double QAG__last_area__get(tree2::RcppR6::RcppR6<tree2::quadrature::QAG> obj_) {
  return obj_->get_last_area();
}

// [[Rcpp::export]]
double QAG__last_error__get(tree2::RcppR6::RcppR6<tree2::quadrature::QAG> obj_) {
  return obj_->get_last_error();
}

// [[Rcpp::export]]
size_t QAG__last_iterations__get(tree2::RcppR6::RcppR6<tree2::quadrature::QAG> obj_) {
  return obj_->get_last_iterations();
}

// [[Rcpp::export]]
tree2::quadrature::intervals_type QAG__last_intervals__get(tree2::RcppR6::RcppR6<tree2::quadrature::QAG> obj_) {
  return obj_->get_last_intervals();
}

// [[Rcpp::export]]
bool QAG__is_adaptive__get(tree2::RcppR6::RcppR6<tree2::quadrature::QAG> obj_) {
  return obj_->is_adaptive();
}


// [[Rcpp::export]]
tree2::interpolator::Interpolator Interpolator__ctor() {
  return tree2::interpolator::Interpolator();
}
// [[Rcpp::export]]
void Interpolator__init(tree2::RcppR6::RcppR6<tree2::interpolator::Interpolator> obj_, const std::vector<double>& x, const std::vector<double>& y) {
  obj_->init(x, y);
}
// [[Rcpp::export]]
std::vector<double> Interpolator__eval(tree2::RcppR6::RcppR6<tree2::interpolator::Interpolator> obj_, std::vector<double> x) {
  return obj_->r_eval(x);
}
// [[Rcpp::export]]
std::vector<double> Interpolator__x__get(tree2::RcppR6::RcppR6<tree2::interpolator::Interpolator> obj_) {
  return obj_->get_x();
}

// [[Rcpp::export]]
std::vector<double> Interpolator__y__get(tree2::RcppR6::RcppR6<tree2::interpolator::Interpolator> obj_) {
  return obj_->get_y();
}

// [[Rcpp::export]]
SEXP Interpolator__xy__get(tree2::RcppR6::RcppR6<tree2::interpolator::Interpolator> obj_) {
  return obj_->r_get_xy();
}

// [[Rcpp::export]]
size_t Interpolator__size__get(tree2::RcppR6::RcppR6<tree2::interpolator::Interpolator> obj_) {
  return obj_->size();
}

// [[Rcpp::export]]
double Interpolator__min__get(tree2::RcppR6::RcppR6<tree2::interpolator::Interpolator> obj_) {
  return obj_->min();
}

// [[Rcpp::export]]
double Interpolator__max__get(tree2::RcppR6::RcppR6<tree2::interpolator::Interpolator> obj_) {
  return obj_->max();
}


// [[Rcpp::export]]
tree2::Environment Environment__ctor(tree2::Parameters p) {
  return tree2::Environment(p);
}
// [[Rcpp::export]]
double Environment__canopy_openness(tree2::RcppR6::RcppR6<tree2::Environment> obj_, double height) {
  return obj_->canopy_openness(height);
}
// [[Rcpp::export]]
double Environment__patch_survival_conditional(tree2::RcppR6::RcppR6<tree2::Environment> obj_, double time_at_birth) {
  return obj_->patch_survival_conditional(time_at_birth);
}
// [[Rcpp::export]]
void Environment__clear(tree2::RcppR6::RcppR6<tree2::Environment> obj_) {
  obj_->clear();
}
// [[Rcpp::export]]
void Environment__set_seed_rain_index(tree2::RcppR6::RcppR6<tree2::Environment> obj_, tree2::util::index x) {
  obj_->r_set_seed_rain_index(x);
}
// [[Rcpp::export]]
double Environment__patch_survival__get(tree2::RcppR6::RcppR6<tree2::Environment> obj_) {
  return obj_->patch_survival();
}

// [[Rcpp::export]]
double Environment__seed_rain_dt__get(tree2::RcppR6::RcppR6<tree2::Environment> obj_) {
  return obj_->seed_rain_dt();
}

// [[Rcpp::export]]
tree2::Disturbance Environment__disturbance_regime__get(tree2::RcppR6::RcppR6<tree2::Environment> obj_) {
  return obj_->disturbance_regime;
}
// [[Rcpp::export]]
void Environment__disturbance_regime__set(tree2::RcppR6::RcppR6<tree2::Environment> obj_, tree2::Disturbance value) {
  obj_->disturbance_regime = value;
}

// [[Rcpp::export]]
double Environment__time__get(tree2::RcppR6::RcppR6<tree2::Environment> obj_) {
  return obj_->time;
}
// [[Rcpp::export]]
void Environment__time__set(tree2::RcppR6::RcppR6<tree2::Environment> obj_, double value) {
  obj_->time = value;
}

// [[Rcpp::export]]
tree2::interpolator::Interpolator Environment__light_environment__get(tree2::RcppR6::RcppR6<tree2::Environment> obj_) {
  return obj_->light_environment;
}
// [[Rcpp::export]]
void Environment__light_environment__set(tree2::RcppR6::RcppR6<tree2::Environment> obj_, tree2::interpolator::Interpolator value) {
  obj_->light_environment = value;
}


// [[Rcpp::export]]
SEXP Plant_Internals__ctor() {
  return Rcpp::wrap(tree2::Plant::Internals());
}


// [[Rcpp::export]]
tree2::Plant Plant__ctor(tree2::Strategy strategy) {
  return tree2::make_plant(strategy);
}
// [[Rcpp::export]]
double Plant__area_leaf_above(tree2::RcppR6::RcppR6<tree2::Plant> obj_, double h) {
  return obj_->area_leaf_above(h);
}
// [[Rcpp::export]]
void Plant__compute_vars_phys(tree2::RcppR6::RcppR6<tree2::Plant> obj_, const tree2::Environment& environment) {
  obj_->compute_vars_phys(environment);
}
// [[Rcpp::export]]
void Plant__compute_vars_growth(tree2::RcppR6::RcppR6<tree2::Plant> obj_) {
  obj_->compute_vars_growth();
}
// [[Rcpp::export]]
double Plant__germination_probability(tree2::RcppR6::RcppR6<tree2::Plant> obj_, const tree2::Environment& environment) {
  return obj_->germination_probability(environment);
}
// [[Rcpp::export]]
double Plant__height__get(tree2::RcppR6::RcppR6<tree2::Plant> obj_) {
  return obj_->height();
}
// [[Rcpp::export]]
void Plant__height__set(tree2::RcppR6::RcppR6<tree2::Plant> obj_, double value) {
  obj_->set_height(value);
}

// [[Rcpp::export]]
double Plant__mortality__get(tree2::RcppR6::RcppR6<tree2::Plant> obj_) {
  return obj_->mortality();
}
// [[Rcpp::export]]
void Plant__mortality__set(tree2::RcppR6::RcppR6<tree2::Plant> obj_, double value) {
  obj_->set_mortality(value);
}

// [[Rcpp::export]]
double Plant__fecundity__get(tree2::RcppR6::RcppR6<tree2::Plant> obj_) {
  return obj_->fecundity();
}
// [[Rcpp::export]]
void Plant__fecundity__set(tree2::RcppR6::RcppR6<tree2::Plant> obj_, double value) {
  obj_->set_fecundity(value);
}

// [[Rcpp::export]]
double Plant__area_heartwood__get(tree2::RcppR6::RcppR6<tree2::Plant> obj_) {
  return obj_->area_heartwood();
}
// [[Rcpp::export]]
void Plant__area_heartwood__set(tree2::RcppR6::RcppR6<tree2::Plant> obj_, double value) {
  obj_->set_area_heartwood(value);
}

// [[Rcpp::export]]
double Plant__mass_heartwood__get(tree2::RcppR6::RcppR6<tree2::Plant> obj_) {
  return obj_->mass_heartwood();
}
// [[Rcpp::export]]
void Plant__mass_heartwood__set(tree2::RcppR6::RcppR6<tree2::Plant> obj_, double value) {
  obj_->set_mass_heartwood(value);
}

// [[Rcpp::export]]
double Plant__area_leaf__get(tree2::RcppR6::RcppR6<tree2::Plant> obj_) {
  return obj_->area_leaf();
}

// [[Rcpp::export]]
tree2::Strategy Plant__strategy__get(tree2::RcppR6::RcppR6<tree2::Plant> obj_) {
  return obj_->r_get_strategy();
}

// [[Rcpp::export]]
tree2::Plant::Internals Plant__internals__get(tree2::RcppR6::RcppR6<tree2::Plant> obj_) {
  return obj_->r_internals();
}

// [[Rcpp::export]]
size_t Plant__ode_size__get(tree2::RcppR6::RcppR6<tree2::Plant> obj_) {
  return obj_->ode_size();
}

// [[Rcpp::export]]
tree2::ode::state_type Plant__ode_state__get(tree2::RcppR6::RcppR6<tree2::Plant> obj_) {
  return tree2::ode::r_ode_state(*obj_);
}
// [[Rcpp::export]]
void Plant__ode_state__set(tree2::RcppR6::RcppR6<tree2::Plant> obj_, tree2::ode::state_type value) {
  tree2::ode::r_set_ode_state(*obj_, value);
}

// [[Rcpp::export]]
tree2::ode::state_type Plant__ode_rates__get(tree2::RcppR6::RcppR6<tree2::Plant> obj_) {
  return tree2::ode::r_ode_rates(*obj_);
}

// [[Rcpp::export]]
std::vector<std::string> Plant__ode_names__get(tree2::RcppR6::RcppR6<tree2::Plant> obj_) {
  return obj_->ode_names();
}


// [[Rcpp::export]]
SEXP Plant2_Internals__ctor() {
  return Rcpp::wrap(tree2::Plant2::Internals());
}


// [[Rcpp::export]]
tree2::Plant2 Plant2__ctor(tree2::Strategy strategy) {
  return tree2::make_plant2(strategy);
}
// [[Rcpp::export]]
double Plant2__area_leaf_above(tree2::RcppR6::RcppR6<tree2::Plant2> obj_, double h) {
  return obj_->area_leaf_above(h);
}
// [[Rcpp::export]]
void Plant2__compute_vars_phys(tree2::RcppR6::RcppR6<tree2::Plant2> obj_, const tree2::Environment& environment) {
  obj_->compute_vars_phys(environment);
}
// [[Rcpp::export]]
double Plant2__germination_probability(tree2::RcppR6::RcppR6<tree2::Plant2> obj_, const tree2::Environment& environment) {
  return obj_->germination_probability(environment);
}
// [[Rcpp::export]]
double Plant2__height__get(tree2::RcppR6::RcppR6<tree2::Plant2> obj_) {
  return obj_->height();
}
// [[Rcpp::export]]
void Plant2__height__set(tree2::RcppR6::RcppR6<tree2::Plant2> obj_, double value) {
  obj_->set_height(value);
}

// [[Rcpp::export]]
double Plant2__mortality__get(tree2::RcppR6::RcppR6<tree2::Plant2> obj_) {
  return obj_->mortality();
}
// [[Rcpp::export]]
void Plant2__mortality__set(tree2::RcppR6::RcppR6<tree2::Plant2> obj_, double value) {
  obj_->set_mortality(value);
}

// [[Rcpp::export]]
double Plant2__fecundity__get(tree2::RcppR6::RcppR6<tree2::Plant2> obj_) {
  return obj_->fecundity();
}
// [[Rcpp::export]]
void Plant2__fecundity__set(tree2::RcppR6::RcppR6<tree2::Plant2> obj_, double value) {
  obj_->set_fecundity(value);
}

// [[Rcpp::export]]
tree2::Strategy Plant2__strategy__get(tree2::RcppR6::RcppR6<tree2::Plant2> obj_) {
  return obj_->r_get_strategy();
}

// [[Rcpp::export]]
tree2::Plant2::Internals Plant2__internals__get(tree2::RcppR6::RcppR6<tree2::Plant2> obj_) {
  return obj_->r_internals();
}

// [[Rcpp::export]]
size_t Plant2__ode_size__get(tree2::RcppR6::RcppR6<tree2::Plant2> obj_) {
  return obj_->ode_size();
}

// [[Rcpp::export]]
tree2::ode::state_type Plant2__ode_state__get(tree2::RcppR6::RcppR6<tree2::Plant2> obj_) {
  return tree2::ode::r_ode_state(*obj_);
}
// [[Rcpp::export]]
void Plant2__ode_state__set(tree2::RcppR6::RcppR6<tree2::Plant2> obj_, tree2::ode::state_type value) {
  tree2::ode::r_set_ode_state(*obj_, value);
}

// [[Rcpp::export]]
tree2::ode::state_type Plant2__ode_rates__get(tree2::RcppR6::RcppR6<tree2::Plant2> obj_) {
  return tree2::ode::r_ode_rates(*obj_);
}

// [[Rcpp::export]]
std::vector<std::string> Plant2__ode_names__get(tree2::RcppR6::RcppR6<tree2::Plant2> obj_) {
  return obj_->ode_names();
}


// [[Rcpp::export]]
tree2::Cohort<tree2::Plant> Cohort__ctor(tree2::Strategy strategy) {
  return tree2::make_cohort<tree2::Plant>(strategy);
}
// [[Rcpp::export]]
double Cohort__area_leaf_above(tree2::RcppR6::RcppR6<tree2::Cohort<tree2::Plant> > obj_, double height) {
  return obj_->area_leaf_above(height);
}
// [[Rcpp::export]]
double Cohort__growth_rate_gradient(tree2::RcppR6::RcppR6<tree2::Cohort<tree2::Plant> > obj_, const tree2::Environment& environment) {
  return obj_->r_growth_rate_gradient(environment);
}
// [[Rcpp::export]]
void Cohort__compute_vars_phys(tree2::RcppR6::RcppR6<tree2::Cohort<tree2::Plant> > obj_, const tree2::Environment& environment) {
  obj_->compute_vars_phys(environment);
}
// [[Rcpp::export]]
void Cohort__compute_initial_conditions(tree2::RcppR6::RcppR6<tree2::Cohort<tree2::Plant> > obj_, const tree2::Environment& environment) {
  obj_->compute_initial_conditions(environment);
}
// [[Rcpp::export]]
tree2::Plant Cohort__plant__get(tree2::RcppR6::RcppR6<tree2::Cohort<tree2::Plant> > obj_) {
  return obj_->plant;
}

// [[Rcpp::export]]
double Cohort__height__get(tree2::RcppR6::RcppR6<tree2::Cohort<tree2::Plant> > obj_) {
  return obj_->height();
}

// [[Rcpp::export]]
double Cohort__area_leaf__get(tree2::RcppR6::RcppR6<tree2::Cohort<tree2::Plant> > obj_) {
  return obj_->area_leaf();
}

// [[Rcpp::export]]
double Cohort__fecundity__get(tree2::RcppR6::RcppR6<tree2::Cohort<tree2::Plant> > obj_) {
  return obj_->fecundity();
}

// [[Rcpp::export]]
size_t Cohort__ode_size__get(tree2::RcppR6::RcppR6<tree2::Cohort<tree2::Plant> > obj_) {
  return obj_->ode_size();
}

// [[Rcpp::export]]
tree2::ode::state_type Cohort__ode_state__get(tree2::RcppR6::RcppR6<tree2::Cohort<tree2::Plant> > obj_) {
  return tree2::ode::r_ode_state(*obj_);
}
// [[Rcpp::export]]
void Cohort__ode_state__set(tree2::RcppR6::RcppR6<tree2::Cohort<tree2::Plant> > obj_, tree2::ode::state_type value) {
  tree2::ode::r_set_ode_state(*obj_, value);
}

// [[Rcpp::export]]
tree2::ode::state_type Cohort__ode_rates__get(tree2::RcppR6::RcppR6<tree2::Cohort<tree2::Plant> > obj_) {
  return tree2::ode::r_ode_rates(*obj_);
}


// [[Rcpp::export]]
tree2::Species<tree2::Cohort<tree2::Plant> > Species__ctor(tree2::Strategy strategy) {
  return tree2::Species<tree2::Cohort<tree2::Plant> >(strategy);
}
// [[Rcpp::export]]
void Species__clear(tree2::RcppR6::RcppR6<tree2::Species<tree2::Cohort<tree2::Plant> > > obj_) {
  obj_->clear();
}
// [[Rcpp::export]]
void Species__compute_vars_phys(tree2::RcppR6::RcppR6<tree2::Species<tree2::Cohort<tree2::Plant> > > obj_, const tree2::Environment& environment) {
  obj_->compute_vars_phys(environment);
}
// [[Rcpp::export]]
double Species__area_leaf_above(tree2::RcppR6::RcppR6<tree2::Species<tree2::Cohort<tree2::Plant> > > obj_, double height) {
  return obj_->area_leaf_above(height);
}
// [[Rcpp::export]]
void Species__add_seed(tree2::RcppR6::RcppR6<tree2::Species<tree2::Cohort<tree2::Plant> > > obj_) {
  obj_->add_seed();
}
// [[Rcpp::export]]
tree2::Cohort<tree2::Plant> Species__plant_at(tree2::RcppR6::RcppR6<tree2::Species<tree2::Cohort<tree2::Plant> > > obj_, tree2::util::index index) {
  return obj_->r_plant_at(index);
}
// [[Rcpp::export]]
std::vector<double> Species__area_leafs_error(tree2::RcppR6::RcppR6<tree2::Species<tree2::Cohort<tree2::Plant> > > obj_, double scal) {
  return obj_->r_area_leafs_error(scal);
}
// [[Rcpp::export]]
size_t Species__size__get(tree2::RcppR6::RcppR6<tree2::Species<tree2::Cohort<tree2::Plant> > > obj_) {
  return obj_->size();
}

// [[Rcpp::export]]
tree2::Cohort<tree2::Plant> Species__seed__get(tree2::RcppR6::RcppR6<tree2::Species<tree2::Cohort<tree2::Plant> > > obj_) {
  return obj_->r_seed();
}

// [[Rcpp::export]]
double Species__height_max__get(tree2::RcppR6::RcppR6<tree2::Species<tree2::Cohort<tree2::Plant> > > obj_) {
  return obj_->height_max();
}

// [[Rcpp::export]]
std::vector<double> Species__heights__get(tree2::RcppR6::RcppR6<tree2::Species<tree2::Cohort<tree2::Plant> > > obj_) {
  return obj_->r_heights();
}
// [[Rcpp::export]]
void Species__heights__set(tree2::RcppR6::RcppR6<tree2::Species<tree2::Cohort<tree2::Plant> > > obj_, std::vector<double> value) {
  obj_->r_set_heights(value);
}

// [[Rcpp::export]]
std::vector<tree2::Cohort<tree2::Plant> > Species__plants__get(tree2::RcppR6::RcppR6<tree2::Species<tree2::Cohort<tree2::Plant> > > obj_) {
  return obj_->r_plants();
}

// [[Rcpp::export]]
std::vector<double> Species__seeds__get(tree2::RcppR6::RcppR6<tree2::Species<tree2::Cohort<tree2::Plant> > > obj_) {
  return obj_->seeds();
}

// [[Rcpp::export]]
std::vector<double> Species__area_leafs__get(tree2::RcppR6::RcppR6<tree2::Species<tree2::Cohort<tree2::Plant> > > obj_) {
  return obj_->r_area_leafs();
}

// [[Rcpp::export]]
size_t Species__ode_size__get(tree2::RcppR6::RcppR6<tree2::Species<tree2::Cohort<tree2::Plant> > > obj_) {
  return obj_->ode_size();
}

// [[Rcpp::export]]
tree2::ode::state_type Species__ode_state__get(tree2::RcppR6::RcppR6<tree2::Species<tree2::Cohort<tree2::Plant> > > obj_) {
  return tree2::ode::r_ode_state(*obj_);
}
// [[Rcpp::export]]
void Species__ode_state__set(tree2::RcppR6::RcppR6<tree2::Species<tree2::Cohort<tree2::Plant> > > obj_, tree2::ode::state_type value) {
  tree2::ode::r_set_ode_state(*obj_, value);
}

// [[Rcpp::export]]
tree2::ode::state_type Species__ode_rates__get(tree2::RcppR6::RcppR6<tree2::Species<tree2::Cohort<tree2::Plant> > > obj_) {
  return tree2::ode::r_ode_rates(*obj_);
}


// [[Rcpp::export]]
tree2::Patch<tree2::Cohort<tree2::Plant> > Patch__ctor(tree2::Parameters parameters) {
  return tree2::Patch<tree2::Cohort<tree2::Plant> >(parameters);
}
// [[Rcpp::export]]
double Patch__area_leaf_above(tree2::RcppR6::RcppR6<tree2::Patch<tree2::Cohort<tree2::Plant> > > obj_, double height) {
  return obj_->area_leaf_above(height);
}
// [[Rcpp::export]]
double Patch__canopy_openness(tree2::RcppR6::RcppR6<tree2::Patch<tree2::Cohort<tree2::Plant> > > obj_, double height) {
  return obj_->canopy_openness(height);
}
// [[Rcpp::export]]
void Patch__add_seed(tree2::RcppR6::RcppR6<tree2::Patch<tree2::Cohort<tree2::Plant> > > obj_, tree2::util::index species_index) {
  obj_->r_add_seed(species_index);
}
// [[Rcpp::export]]
void Patch__compute_light_environment(tree2::RcppR6::RcppR6<tree2::Patch<tree2::Cohort<tree2::Plant> > > obj_) {
  obj_->r_compute_light_environment();
}
// [[Rcpp::export]]
void Patch__compute_vars_phys(tree2::RcppR6::RcppR6<tree2::Patch<tree2::Cohort<tree2::Plant> > > obj_) {
  obj_->r_compute_vars_phys();
}
// [[Rcpp::export]]
void Patch__reset(tree2::RcppR6::RcppR6<tree2::Patch<tree2::Cohort<tree2::Plant> > > obj_) {
  obj_->reset();
}
// [[Rcpp::export]]
void Patch__set_ode_state(tree2::RcppR6::RcppR6<tree2::Patch<tree2::Cohort<tree2::Plant> > > obj_, tree2::ode::state_type values, double time) {
  tree2::ode::r_set_ode_state(*obj_, values, time);
}
// [[Rcpp::export]]
tree2::ode::state_type Patch__derivs(tree2::RcppR6::RcppR6<tree2::Patch<tree2::Cohort<tree2::Plant> > > obj_, const tree2::ode::state_type& y, double time) {
  return tree2::ode::r_derivs(*obj_, y, time);
}
// [[Rcpp::export]]
double Patch__time__get(tree2::RcppR6::RcppR6<tree2::Patch<tree2::Cohort<tree2::Plant> > > obj_) {
  return obj_->time();
}

// [[Rcpp::export]]
size_t Patch__size__get(tree2::RcppR6::RcppR6<tree2::Patch<tree2::Cohort<tree2::Plant> > > obj_) {
  return obj_->size();
}

// [[Rcpp::export]]
double Patch__height_max__get(tree2::RcppR6::RcppR6<tree2::Patch<tree2::Cohort<tree2::Plant> > > obj_) {
  return obj_->height_max();
}

// [[Rcpp::export]]
tree2::Parameters Patch__parameters__get(tree2::RcppR6::RcppR6<tree2::Patch<tree2::Cohort<tree2::Plant> > > obj_) {
  return obj_->r_parameters();
}

// [[Rcpp::export]]
tree2::Environment Patch__environment__get(tree2::RcppR6::RcppR6<tree2::Patch<tree2::Cohort<tree2::Plant> > > obj_) {
  return obj_->r_environment();
}

// [[Rcpp::export]]
std::vector<tree2::Species<tree2::Cohort<tree2::Plant> > > Patch__species__get(tree2::RcppR6::RcppR6<tree2::Patch<tree2::Cohort<tree2::Plant> > > obj_) {
  return obj_->r_species();
}

// [[Rcpp::export]]
size_t Patch__ode_size__get(tree2::RcppR6::RcppR6<tree2::Patch<tree2::Cohort<tree2::Plant> > > obj_) {
  return obj_->ode_size();
}

// [[Rcpp::export]]
double Patch__ode_time__get(tree2::RcppR6::RcppR6<tree2::Patch<tree2::Cohort<tree2::Plant> > > obj_) {
  return tree2::ode::r_ode_time(*obj_);
}

// [[Rcpp::export]]
tree2::ode::state_type Patch__ode_state__get(tree2::RcppR6::RcppR6<tree2::Patch<tree2::Cohort<tree2::Plant> > > obj_) {
  return tree2::ode::r_ode_state(*obj_);
}

// [[Rcpp::export]]
tree2::ode::state_type Patch__ode_rates__get(tree2::RcppR6::RcppR6<tree2::Patch<tree2::Cohort<tree2::Plant> > > obj_) {
  return tree2::ode::r_ode_rates(*obj_);
}


// [[Rcpp::export]]
tree2::EBT<tree2::Plant> EBT__ctor(tree2::Parameters parameters) {
  return tree2::EBT<tree2::Plant>(parameters);
}
// [[Rcpp::export]]
void EBT__run(tree2::RcppR6::RcppR6<tree2::EBT<tree2::Plant> > obj_) {
  obj_->run();
}
// [[Rcpp::export]]
std::vector<tree2::util::index> EBT__run_next(tree2::RcppR6::RcppR6<tree2::EBT<tree2::Plant> > obj_) {
  return obj_->r_run_next();
}
// [[Rcpp::export]]
void EBT__reset(tree2::RcppR6::RcppR6<tree2::EBT<tree2::Plant> > obj_) {
  obj_->reset();
}
// [[Rcpp::export]]
double EBT__seed_rain(tree2::RcppR6::RcppR6<tree2::EBT<tree2::Plant> > obj_, tree2::util::index species_index) {
  return obj_->r_seed_rain(species_index);
}
// [[Rcpp::export]]
std::vector<double> EBT__seed_rain_cohort(tree2::RcppR6::RcppR6<tree2::EBT<tree2::Plant> > obj_, tree2::util::index species_index) {
  return obj_->r_seed_rain_cohort(species_index);
}
// [[Rcpp::export]]
std::vector<double> EBT__area_leaf_error(tree2::RcppR6::RcppR6<tree2::EBT<tree2::Plant> > obj_, tree2::util::index species_index) {
  return obj_->r_area_leaf_error(species_index);
}
// [[Rcpp::export]]
void EBT__set_cohort_schedule_times(tree2::RcppR6::RcppR6<tree2::EBT<tree2::Plant> > obj_, std::vector<std::vector<double> > times) {
  obj_->r_set_cohort_schedule_times(times);
}
// [[Rcpp::export]]
bool EBT__complete__get(tree2::RcppR6::RcppR6<tree2::EBT<tree2::Plant> > obj_) {
  return obj_->complete();
}

// [[Rcpp::export]]
double EBT__time__get(tree2::RcppR6::RcppR6<tree2::EBT<tree2::Plant> > obj_) {
  return obj_->time();
}

// [[Rcpp::export]]
std::vector<double> EBT__seed_rains__get(tree2::RcppR6::RcppR6<tree2::EBT<tree2::Plant> > obj_) {
  return obj_->seed_rains();
}

// [[Rcpp::export]]
tree2::Parameters EBT__parameters__get(tree2::RcppR6::RcppR6<tree2::EBT<tree2::Plant> > obj_) {
  return obj_->r_parameters();
}

// [[Rcpp::export]]
tree2::Patch<tree2::Cohort<tree2::Plant> > EBT__patch__get(tree2::RcppR6::RcppR6<tree2::EBT<tree2::Plant> > obj_) {
  return obj_->r_patch();
}

// [[Rcpp::export]]
tree2::CohortSchedule EBT__cohort_schedule__get(tree2::RcppR6::RcppR6<tree2::EBT<tree2::Plant> > obj_) {
  return obj_->r_cohort_schedule();
}
// [[Rcpp::export]]
void EBT__cohort_schedule__set(tree2::RcppR6::RcppR6<tree2::EBT<tree2::Plant> > obj_, tree2::CohortSchedule value) {
  obj_->r_set_cohort_schedule(value);
}

// [[Rcpp::export]]
std::vector<double> EBT__ode_times__get(tree2::RcppR6::RcppR6<tree2::EBT<tree2::Plant> > obj_) {
  return obj_->r_ode_times();
}

// [[Rcpp::export]]
Rcpp::List EBT__state__get(tree2::RcppR6::RcppR6<tree2::EBT<tree2::Plant> > obj_) {
  return tree2::get_state(*obj_);
}

// [[Rcpp::export]]
bool EBT__use_ode_times__get(tree2::RcppR6::RcppR6<tree2::EBT<tree2::Plant> > obj_) {
  return obj_->r_use_ode_times();
}
// [[Rcpp::export]]
void EBT__use_ode_times__set(tree2::RcppR6::RcppR6<tree2::EBT<tree2::Plant> > obj_, bool value) {
  obj_->r_set_use_ode_times(value);
}

// [[Rcpp::export]]
std::vector<std::vector<double> > EBT__seed_rain_error__get(tree2::RcppR6::RcppR6<tree2::EBT<tree2::Plant> > obj_) {
  return obj_->r_seed_rain_error();
}


// [[Rcpp::export]]
tree2::tools::PlantRunner PlantRunner__ctor(tree2::Plant plant, tree2::Environment environment) {
  return tree2::tools::PlantRunner(plant, environment);
}
// [[Rcpp::export]]
tree2::Plant PlantRunner__plant__get(tree2::RcppR6::RcppR6<tree2::tools::PlantRunner> obj_) {
  return obj_->plant;
}
// [[Rcpp::export]]
void PlantRunner__plant__set(tree2::RcppR6::RcppR6<tree2::tools::PlantRunner> obj_, tree2::Plant value) {
  obj_->plant = value;
}

