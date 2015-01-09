// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/tree2.h"
#include <Rcpp.h>

using namespace Rcpp;

// Lorenz__ctor
ode::test::Lorenz Lorenz__ctor(double sigma, double R, double b);
RcppExport SEXP tree2_Lorenz__ctor(SEXP sigmaSEXP, SEXP RSEXP, SEXP bSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< double >::type sigma(sigmaSEXP );
        Rcpp::traits::input_parameter< double >::type R(RSEXP );
        Rcpp::traits::input_parameter< double >::type b(bSEXP );
        ode::test::Lorenz __result = Lorenz__ctor(sigma, R, b);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// Lorenz__size__get
size_t Lorenz__size__get(tree2::RcppR6::RcppR6<ode::test::Lorenz> obj_);
RcppExport SEXP tree2_Lorenz__size__get(SEXP obj_SEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< tree2::RcppR6::RcppR6<ode::test::Lorenz> >::type obj_(obj_SEXP );
        size_t __result = Lorenz__size__get(obj_);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// Lorenz__ode_values__get
std::vector<double> Lorenz__ode_values__get(tree2::RcppR6::RcppR6<ode::test::Lorenz> obj_);
RcppExport SEXP tree2_Lorenz__ode_values__get(SEXP obj_SEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< tree2::RcppR6::RcppR6<ode::test::Lorenz> >::type obj_(obj_SEXP );
        std::vector<double> __result = Lorenz__ode_values__get(obj_);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// Lorenz__ode_values__set
void Lorenz__ode_values__set(tree2::RcppR6::RcppR6<ode::test::Lorenz> obj_, std::vector<double> value);
RcppExport SEXP tree2_Lorenz__ode_values__set(SEXP obj_SEXP, SEXP valueSEXP) {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< tree2::RcppR6::RcppR6<ode::test::Lorenz> >::type obj_(obj_SEXP );
        Rcpp::traits::input_parameter< std::vector<double> >::type value(valueSEXP );
        Lorenz__ode_values__set(obj_, value);
    }
    return R_NilValue;
END_RCPP
}
// Lorenz__ode_rates__get
std::vector<double> Lorenz__ode_rates__get(tree2::RcppR6::RcppR6<ode::test::Lorenz> obj_);
RcppExport SEXP tree2_Lorenz__ode_rates__get(SEXP obj_SEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< tree2::RcppR6::RcppR6<ode::test::Lorenz> >::type obj_(obj_SEXP );
        std::vector<double> __result = Lorenz__ode_rates__get(obj_);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// Lorenz__pars__get
Rcpp::NumericVector Lorenz__pars__get(tree2::RcppR6::RcppR6<ode::test::Lorenz> obj_);
RcppExport SEXP tree2_Lorenz__pars__get(SEXP obj_SEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< tree2::RcppR6::RcppR6<ode::test::Lorenz> >::type obj_(obj_SEXP );
        Rcpp::NumericVector __result = Lorenz__pars__get(obj_);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// OdeSystem___Lorenz__ctor
ode::OdeSystem<ode::test::Lorenz> OdeSystem___Lorenz__ctor(ode::test::Lorenz obj, double abs_tol, double rel_tol);
RcppExport SEXP tree2_OdeSystem___Lorenz__ctor(SEXP objSEXP, SEXP abs_tolSEXP, SEXP rel_tolSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< ode::test::Lorenz >::type obj(objSEXP );
        Rcpp::traits::input_parameter< double >::type abs_tol(abs_tolSEXP );
        Rcpp::traits::input_parameter< double >::type rel_tol(rel_tolSEXP );
        ode::OdeSystem<ode::test::Lorenz> __result = OdeSystem___Lorenz__ctor(obj, abs_tol, rel_tol);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// OdeSystem___Lorenz__do_step
void OdeSystem___Lorenz__do_step(tree2::RcppR6::RcppR6<ode::OdeSystem<ode::test::Lorenz> > obj_, double dt);
RcppExport SEXP tree2_OdeSystem___Lorenz__do_step(SEXP obj_SEXP, SEXP dtSEXP) {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< tree2::RcppR6::RcppR6<ode::OdeSystem<ode::test::Lorenz> > >::type obj_(obj_SEXP );
        Rcpp::traits::input_parameter< double >::type dt(dtSEXP );
        OdeSystem___Lorenz__do_step(obj_, dt);
    }
    return R_NilValue;
END_RCPP
}
// OdeSystem___Lorenz__try_step
bool OdeSystem___Lorenz__try_step(tree2::RcppR6::RcppR6<ode::OdeSystem<ode::test::Lorenz> > obj_, double dt);
RcppExport SEXP tree2_OdeSystem___Lorenz__try_step(SEXP obj_SEXP, SEXP dtSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< tree2::RcppR6::RcppR6<ode::OdeSystem<ode::test::Lorenz> > >::type obj_(obj_SEXP );
        Rcpp::traits::input_parameter< double >::type dt(dtSEXP );
        bool __result = OdeSystem___Lorenz__try_step(obj_, dt);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// OdeSystem___Lorenz__advance
void OdeSystem___Lorenz__advance(tree2::RcppR6::RcppR6<ode::OdeSystem<ode::test::Lorenz> > obj_, double t, double dt);
RcppExport SEXP tree2_OdeSystem___Lorenz__advance(SEXP obj_SEXP, SEXP tSEXP, SEXP dtSEXP) {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< tree2::RcppR6::RcppR6<ode::OdeSystem<ode::test::Lorenz> > >::type obj_(obj_SEXP );
        Rcpp::traits::input_parameter< double >::type t(tSEXP );
        Rcpp::traits::input_parameter< double >::type dt(dtSEXP );
        OdeSystem___Lorenz__advance(obj_, t, dt);
    }
    return R_NilValue;
END_RCPP
}
// OdeSystem___Lorenz__advance_save
ode::state_saver<std::vector<double> > OdeSystem___Lorenz__advance_save(tree2::RcppR6::RcppR6<ode::OdeSystem<ode::test::Lorenz> > obj_, double t, double dt);
RcppExport SEXP tree2_OdeSystem___Lorenz__advance_save(SEXP obj_SEXP, SEXP tSEXP, SEXP dtSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< tree2::RcppR6::RcppR6<ode::OdeSystem<ode::test::Lorenz> > >::type obj_(obj_SEXP );
        Rcpp::traits::input_parameter< double >::type t(tSEXP );
        Rcpp::traits::input_parameter< double >::type dt(dtSEXP );
        ode::state_saver<std::vector<double> > __result = OdeSystem___Lorenz__advance_save(obj_, t, dt);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// OdeSystem___Lorenz__obj__get
ode::test::Lorenz OdeSystem___Lorenz__obj__get(tree2::RcppR6::RcppR6<ode::OdeSystem<ode::test::Lorenz> > obj_);
RcppExport SEXP tree2_OdeSystem___Lorenz__obj__get(SEXP obj_SEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< tree2::RcppR6::RcppR6<ode::OdeSystem<ode::test::Lorenz> > >::type obj_(obj_SEXP );
        ode::test::Lorenz __result = OdeSystem___Lorenz__obj__get(obj_);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// OdeSystem___Lorenz__t__get
double OdeSystem___Lorenz__t__get(tree2::RcppR6::RcppR6<ode::OdeSystem<ode::test::Lorenz> > obj_);
RcppExport SEXP tree2_OdeSystem___Lorenz__t__get(SEXP obj_SEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< tree2::RcppR6::RcppR6<ode::OdeSystem<ode::test::Lorenz> > >::type obj_(obj_SEXP );
        double __result = OdeSystem___Lorenz__t__get(obj_);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// OdeSystem___Lorenz__t__set
void OdeSystem___Lorenz__t__set(tree2::RcppR6::RcppR6<ode::OdeSystem<ode::test::Lorenz> > obj_, double value);
RcppExport SEXP tree2_OdeSystem___Lorenz__t__set(SEXP obj_SEXP, SEXP valueSEXP) {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< tree2::RcppR6::RcppR6<ode::OdeSystem<ode::test::Lorenz> > >::type obj_(obj_SEXP );
        Rcpp::traits::input_parameter< double >::type value(valueSEXP );
        OdeSystem___Lorenz__t__set(obj_, value);
    }
    return R_NilValue;
END_RCPP
}
// OdeSystem___Lorenz__y__get
std::vector<double> OdeSystem___Lorenz__y__get(tree2::RcppR6::RcppR6<ode::OdeSystem<ode::test::Lorenz> > obj_);
RcppExport SEXP tree2_OdeSystem___Lorenz__y__get(SEXP obj_SEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< tree2::RcppR6::RcppR6<ode::OdeSystem<ode::test::Lorenz> > >::type obj_(obj_SEXP );
        std::vector<double> __result = OdeSystem___Lorenz__y__get(obj_);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// OdeSystem___Lorenz__y__set
void OdeSystem___Lorenz__y__set(tree2::RcppR6::RcppR6<ode::OdeSystem<ode::test::Lorenz> > obj_, std::vector<double> value);
RcppExport SEXP tree2_OdeSystem___Lorenz__y__set(SEXP obj_SEXP, SEXP valueSEXP) {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< tree2::RcppR6::RcppR6<ode::OdeSystem<ode::test::Lorenz> > >::type obj_(obj_SEXP );
        Rcpp::traits::input_parameter< std::vector<double> >::type value(valueSEXP );
        OdeSystem___Lorenz__y__set(obj_, value);
    }
    return R_NilValue;
END_RCPP
}
// CohortScheduleEvent__ctor
tree2::CohortSchedule::Event CohortScheduleEvent__ctor(double introduction, size_t species_index);
RcppExport SEXP tree2_CohortScheduleEvent__ctor(SEXP introductionSEXP, SEXP species_indexSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< double >::type introduction(introductionSEXP );
        Rcpp::traits::input_parameter< size_t >::type species_index(species_indexSEXP );
        tree2::CohortSchedule::Event __result = CohortScheduleEvent__ctor(introduction, species_index);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// CohortScheduleEvent__species_index__get
size_t CohortScheduleEvent__species_index__get(tree2::RcppR6::RcppR6<tree2::CohortSchedule::Event> obj_);
RcppExport SEXP tree2_CohortScheduleEvent__species_index__get(SEXP obj_SEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< tree2::RcppR6::RcppR6<tree2::CohortSchedule::Event> >::type obj_(obj_SEXP );
        size_t __result = CohortScheduleEvent__species_index__get(obj_);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// CohortScheduleEvent__times__get
std::vector<double> CohortScheduleEvent__times__get(tree2::RcppR6::RcppR6<tree2::CohortSchedule::Event> obj_);
RcppExport SEXP tree2_CohortScheduleEvent__times__get(SEXP obj_SEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< tree2::RcppR6::RcppR6<tree2::CohortSchedule::Event> >::type obj_(obj_SEXP );
        std::vector<double> __result = CohortScheduleEvent__times__get(obj_);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// CohortScheduleEvent__time_introduction__get
double CohortScheduleEvent__time_introduction__get(tree2::RcppR6::RcppR6<tree2::CohortSchedule::Event> obj_);
RcppExport SEXP tree2_CohortScheduleEvent__time_introduction__get(SEXP obj_SEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< tree2::RcppR6::RcppR6<tree2::CohortSchedule::Event> >::type obj_(obj_SEXP );
        double __result = CohortScheduleEvent__time_introduction__get(obj_);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// CohortScheduleEvent__time_end__get
double CohortScheduleEvent__time_end__get(tree2::RcppR6::RcppR6<tree2::CohortSchedule::Event> obj_);
RcppExport SEXP tree2_CohortScheduleEvent__time_end__get(SEXP obj_SEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< tree2::RcppR6::RcppR6<tree2::CohortSchedule::Event> >::type obj_(obj_SEXP );
        double __result = CohortScheduleEvent__time_end__get(obj_);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// Control__ctor
SEXP Control__ctor();
RcppExport SEXP tree2_Control__ctor() {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        SEXP __result = Control__ctor();
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// Strategy__ctor
SEXP Strategy__ctor();
RcppExport SEXP tree2_Strategy__ctor() {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        SEXP __result = Strategy__ctor();
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
