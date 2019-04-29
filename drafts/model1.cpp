# include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix income(double gamma, NumericMatrix inflos) {
    return gamma * inflos;
}

// [[Rcpp::export]]
NumericMatrix exchange(double proba_migration, NumericMatrix inflos) {
    NumericMatrix alpha(inflos.nrow(), 9);
    alpha(, 0) = alpha(, 8) = 1 - proba_migration;
    alpha(, 2) = proba_migration * inflos(, 0) / (inflos(, 1) + inflos(, 0));
    alpha(, 3) = proba_migration * inflos(, 1) / (inflos(, 1) + inflos(, 2));
    alpha(, 5) = proba_migration * inflos(, 1) / (inflos(, 1) + inflos(, 0));
    alpha(, 6) = proba_migration * inflos(, 2) / (inflos(, 1) + inflos(, 2));
    return alpha;
}