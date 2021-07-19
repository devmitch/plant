// Generated by RcppR6 (0.2.4): do not edit by hand
#ifndef _PLANT_RCPPR6_PRE_HPP_
#define _PLANT_RCPPR6_PRE_HPP_

#include <RcppCommon.h>


namespace plant {
namespace RcppR6 {
template <typename T> class RcppR6;
}
}

namespace plant { namespace ode { namespace test { class OdeR; } } }

namespace Rcpp {
template <typename T> SEXP wrap(const plant::RcppR6::RcppR6<T>&);
namespace traits {
template <typename T> class Exporter<plant::RcppR6::RcppR6<T> >;
}

template <> SEXP wrap(const plant::ode::test::Lorenz&);
template <> plant::ode::test::Lorenz as(SEXP);
template <> SEXP wrap(const plant::ode::test::OdeR&);
template <> plant::ode::test::OdeR as(SEXP);
template <> SEXP wrap(const plant::ode::Runner<plant::ode::test::Lorenz>&);
template <> plant::ode::Runner<plant::ode::test::Lorenz> as(SEXP);

template <> SEXP wrap(const plant::ode::Runner<plant::ode::test::OdeR>&);
template <> plant::ode::Runner<plant::ode::test::OdeR> as(SEXP);

template <> SEXP wrap(const plant::ode::Runner<plant::tools::IndividualRunner<plant::FF16_Strategy, plant::FF16_Environment> >&);
template <> plant::ode::Runner<plant::tools::IndividualRunner<plant::FF16_Strategy, plant::FF16_Environment> > as(SEXP);

template <> SEXP wrap(const plant::ode::Runner<plant::tools::IndividualRunner<plant::Water_Strategy, plant::Water_Environment> >&);
template <> plant::ode::Runner<plant::tools::IndividualRunner<plant::Water_Strategy, plant::Water_Environment> > as(SEXP);

template <> SEXP wrap(const plant::ode::Runner<plant::tools::IndividualRunner<plant::FF16r_Strategy, plant::FF16_Environment> >&);
template <> plant::ode::Runner<plant::tools::IndividualRunner<plant::FF16r_Strategy, plant::FF16_Environment> > as(SEXP);

template <> SEXP wrap(const plant::ode::Runner<plant::tools::IndividualRunner<plant::K93_Strategy, plant::K93_Environment> >&);
template <> plant::ode::Runner<plant::tools::IndividualRunner<plant::K93_Strategy, plant::K93_Environment> > as(SEXP);
template <> SEXP wrap(const plant::CohortScheduleEvent&);
template <> plant::CohortScheduleEvent as(SEXP);
template <> SEXP wrap(const plant::CohortSchedule&);
template <> plant::CohortSchedule as(SEXP);
template <> SEXP wrap(const plant::Control&);
template <> plant::Control as(SEXP);
template <> SEXP wrap(const plant::ode::OdeControl&);
template <> plant::ode::OdeControl as(SEXP);
template <> SEXP wrap(const plant::quadrature::QK&);
template <> plant::quadrature::QK as(SEXP);
template <> SEXP wrap(const plant::quadrature::QAG&);
template <> plant::quadrature::QAG as(SEXP);
template <> SEXP wrap(const plant::interpolator::Interpolator&);
template <> plant::interpolator::Interpolator as(SEXP);
template <> SEXP wrap(const plant::Individual<plant::FF16_Strategy,plant::FF16_Environment>&);
template <> plant::Individual<plant::FF16_Strategy,plant::FF16_Environment> as(SEXP);

template <> SEXP wrap(const plant::Individual<plant::Water_Strategy,plant::Water_Environment>&);
template <> plant::Individual<plant::Water_Strategy,plant::Water_Environment> as(SEXP);

template <> SEXP wrap(const plant::Individual<plant::FF16r_Strategy,plant::FF16_Environment>&);
template <> plant::Individual<plant::FF16r_Strategy,plant::FF16_Environment> as(SEXP);

template <> SEXP wrap(const plant::Individual<plant::K93_Strategy,plant::K93_Environment>&);
template <> plant::Individual<plant::K93_Strategy,plant::K93_Environment> as(SEXP);
template <> SEXP wrap(const plant::tools::IndividualRunner<plant::FF16_Strategy,plant::FF16_Environment>&);
template <> plant::tools::IndividualRunner<plant::FF16_Strategy,plant::FF16_Environment> as(SEXP);

template <> SEXP wrap(const plant::tools::IndividualRunner<plant::Water_Strategy,plant::Water_Environment>&);
template <> plant::tools::IndividualRunner<plant::Water_Strategy,plant::Water_Environment> as(SEXP);

template <> SEXP wrap(const plant::tools::IndividualRunner<plant::FF16r_Strategy,plant::FF16_Environment>&);
template <> plant::tools::IndividualRunner<plant::FF16r_Strategy,plant::FF16_Environment> as(SEXP);

template <> SEXP wrap(const plant::tools::IndividualRunner<plant::K93_Strategy,plant::K93_Environment>&);
template <> plant::tools::IndividualRunner<plant::K93_Strategy,plant::K93_Environment> as(SEXP);
template <> SEXP wrap(const plant::Internals&);
template <> plant::Internals as(SEXP);
template <> SEXP wrap(const plant::Parameters<plant::FF16_Strategy,plant::FF16_Environment>&);
template <> plant::Parameters<plant::FF16_Strategy,plant::FF16_Environment> as(SEXP);

template <> SEXP wrap(const plant::Parameters<plant::Water_Strategy,plant::Water_Environment>&);
template <> plant::Parameters<plant::Water_Strategy,plant::Water_Environment> as(SEXP);

template <> SEXP wrap(const plant::Parameters<plant::FF16r_Strategy,plant::FF16_Environment>&);
template <> plant::Parameters<plant::FF16r_Strategy,plant::FF16_Environment> as(SEXP);

template <> SEXP wrap(const plant::Parameters<plant::K93_Strategy,plant::K93_Environment>&);
template <> plant::Parameters<plant::K93_Strategy,plant::K93_Environment> as(SEXP);
template <> SEXP wrap(const plant::Cohort<plant::FF16_Strategy,plant::FF16_Environment>&);
template <> plant::Cohort<plant::FF16_Strategy,plant::FF16_Environment> as(SEXP);

template <> SEXP wrap(const plant::Cohort<plant::Water_Strategy,plant::Water_Environment>&);
template <> plant::Cohort<plant::Water_Strategy,plant::Water_Environment> as(SEXP);

template <> SEXP wrap(const plant::Cohort<plant::FF16r_Strategy,plant::FF16_Environment>&);
template <> plant::Cohort<plant::FF16r_Strategy,plant::FF16_Environment> as(SEXP);

template <> SEXP wrap(const plant::Cohort<plant::K93_Strategy,plant::K93_Environment>&);
template <> plant::Cohort<plant::K93_Strategy,plant::K93_Environment> as(SEXP);
template <> SEXP wrap(const plant::Species<plant::FF16_Strategy,plant::FF16_Environment>&);
template <> plant::Species<plant::FF16_Strategy,plant::FF16_Environment> as(SEXP);

template <> SEXP wrap(const plant::Species<plant::Water_Strategy,plant::Water_Environment>&);
template <> plant::Species<plant::Water_Strategy,plant::Water_Environment> as(SEXP);

template <> SEXP wrap(const plant::Species<plant::FF16r_Strategy,plant::FF16_Environment>&);
template <> plant::Species<plant::FF16r_Strategy,plant::FF16_Environment> as(SEXP);

template <> SEXP wrap(const plant::Species<plant::K93_Strategy,plant::K93_Environment>&);
template <> plant::Species<plant::K93_Strategy,plant::K93_Environment> as(SEXP);
template <> SEXP wrap(const plant::Patch<plant::FF16_Strategy,plant::FF16_Environment>&);
template <> plant::Patch<plant::FF16_Strategy,plant::FF16_Environment> as(SEXP);

template <> SEXP wrap(const plant::Patch<plant::Water_Strategy,plant::Water_Environment>&);
template <> plant::Patch<plant::Water_Strategy,plant::Water_Environment> as(SEXP);

template <> SEXP wrap(const plant::Patch<plant::FF16r_Strategy,plant::FF16_Environment>&);
template <> plant::Patch<plant::FF16r_Strategy,plant::FF16_Environment> as(SEXP);

template <> SEXP wrap(const plant::Patch<plant::K93_Strategy,plant::K93_Environment>&);
template <> plant::Patch<plant::K93_Strategy,plant::K93_Environment> as(SEXP);
template <> SEXP wrap(const plant::SCM<plant::FF16_Strategy,plant::FF16_Environment>&);
template <> plant::SCM<plant::FF16_Strategy,plant::FF16_Environment> as(SEXP);

template <> SEXP wrap(const plant::SCM<plant::Water_Strategy,plant::Water_Environment>&);
template <> plant::SCM<plant::Water_Strategy,plant::Water_Environment> as(SEXP);

template <> SEXP wrap(const plant::SCM<plant::FF16r_Strategy,plant::FF16_Environment>&);
template <> plant::SCM<plant::FF16r_Strategy,plant::FF16_Environment> as(SEXP);

template <> SEXP wrap(const plant::SCM<plant::K93_Strategy,plant::K93_Environment>&);
template <> plant::SCM<plant::K93_Strategy,plant::K93_Environment> as(SEXP);
template <> SEXP wrap(const plant::StochasticSpecies<plant::FF16_Strategy,plant::FF16_Environment>&);
template <> plant::StochasticSpecies<plant::FF16_Strategy,plant::FF16_Environment> as(SEXP);

template <> SEXP wrap(const plant::StochasticSpecies<plant::Water_Strategy,plant::Water_Environment>&);
template <> plant::StochasticSpecies<plant::Water_Strategy,plant::Water_Environment> as(SEXP);

template <> SEXP wrap(const plant::StochasticSpecies<plant::FF16r_Strategy,plant::FF16_Environment>&);
template <> plant::StochasticSpecies<plant::FF16r_Strategy,plant::FF16_Environment> as(SEXP);

template <> SEXP wrap(const plant::StochasticSpecies<plant::K93_Strategy,plant::K93_Environment>&);
template <> plant::StochasticSpecies<plant::K93_Strategy,plant::K93_Environment> as(SEXP);
template <> SEXP wrap(const plant::StochasticPatch<plant::FF16_Strategy,plant::FF16_Environment>&);
template <> plant::StochasticPatch<plant::FF16_Strategy,plant::FF16_Environment> as(SEXP);

template <> SEXP wrap(const plant::StochasticPatch<plant::Water_Strategy,plant::Water_Environment>&);
template <> plant::StochasticPatch<plant::Water_Strategy,plant::Water_Environment> as(SEXP);

template <> SEXP wrap(const plant::StochasticPatch<plant::FF16r_Strategy,plant::FF16_Environment>&);
template <> plant::StochasticPatch<plant::FF16r_Strategy,plant::FF16_Environment> as(SEXP);

template <> SEXP wrap(const plant::StochasticPatch<plant::K93_Strategy,plant::K93_Environment>&);
template <> plant::StochasticPatch<plant::K93_Strategy,plant::K93_Environment> as(SEXP);
template <> SEXP wrap(const plant::StochasticPatchRunner<plant::FF16_Strategy,plant::FF16_Environment>&);
template <> plant::StochasticPatchRunner<plant::FF16_Strategy,plant::FF16_Environment> as(SEXP);

template <> SEXP wrap(const plant::StochasticPatchRunner<plant::Water_Strategy,plant::Water_Environment>&);
template <> plant::StochasticPatchRunner<plant::Water_Strategy,plant::Water_Environment> as(SEXP);

template <> SEXP wrap(const plant::StochasticPatchRunner<plant::FF16r_Strategy,plant::FF16_Environment>&);
template <> plant::StochasticPatchRunner<plant::FF16r_Strategy,plant::FF16_Environment> as(SEXP);

template <> SEXP wrap(const plant::StochasticPatchRunner<plant::K93_Strategy,plant::K93_Environment>&);
template <> plant::StochasticPatchRunner<plant::K93_Strategy,plant::K93_Environment> as(SEXP);
template <> SEXP wrap(const plant::Canopy&);
template <> plant::Canopy as(SEXP);
template <> SEXP wrap(const plant::Disturbance_Regime&);
template <> plant::Disturbance_Regime as(SEXP);
template <> SEXP wrap(const plant::No_Disturbance&);
template <> plant::No_Disturbance as(SEXP);
template <> SEXP wrap(const plant::Weibull_Disturbance_Regime&);
template <> plant::Weibull_Disturbance_Regime as(SEXP);
template <> SEXP wrap(const plant::FF16_Strategy&);
template <> plant::FF16_Strategy as(SEXP);
template <> SEXP wrap(const plant::FF16_Environment&);
template <> plant::FF16_Environment as(SEXP);
template <> SEXP wrap(const plant::FF16r_Strategy&);
template <> plant::FF16r_Strategy as(SEXP);
template <> SEXP wrap(const plant::K93_Strategy&);
template <> plant::K93_Strategy as(SEXP);
template <> SEXP wrap(const plant::K93_Environment&);
template <> plant::K93_Environment as(SEXP);
template <> SEXP wrap(const plant::Water_Strategy&);
template <> plant::Water_Strategy as(SEXP);
template <> SEXP wrap(const plant::Water_Environment&);
template <> plant::Water_Environment as(SEXP);
}

#endif
