//#include <Rcpp.h>
#include <RcppArmadillo.h>
#include <ilcplex/ilocplex.h>

class LossComputer {
private:
  arma::mat x; // {n x d} matrix
  arma::vec y; // {n x 1} vector
  arma::mat z; // {n x d} matrix

public:
  //LossComputer() {cout << "empty loss computer?\n";};
  //LossComputer(const LossComputer &tocopy) {cout << "copy constructor loss computer?\n";};

  LossComputer(const arma::mat &_x, const arma::vec &_y):
  x(_x), y(_y) {
    z = x.each_col() % y;
  }

  inline double loss(arma::vec lambda) {
    return arma::mean(arma::log(1 + arma::exp(-z * lambda)));
  }

  inline arma::vec loss_grad(arma::vec lambda) {
    arma::vec b = 1 + arma::exp(z * lambda); // {n x 1} vector
    arma::mat a = z.each_col() / b;
    arma::mat lg = arma::mean(-a, 0); // col means
    return arma::vectorise(lg);
  }
};

// my callback implements the Callback::Function interface (I think?)
class LossCutCallback: public IloCplex::Callback::Function {
private:
  // Empty constructor is forbidden
  LossCutCallback() {};

  // Copy constructor is forbidden
  LossCutCallback(const LossCutCallback &tocopy) {};

  // current solution
  IloIntVarArray lambda;
  IloNumVar L;

  // loss computer
  LossComputer* computer;

public:
  // Constructor with data
  LossCutCallback(const IloIntVarArray &_lambda, IloNumVar &_L, LossComputer* _computer):
  lambda(_lambda), L(_L), computer(_computer) {}

  inline void lazyCut(const IloCplex::Callback::Context &context) {
    IloEnv env = context.getEnv();

    //env.out() << "candidate lambda: [" ;
    arma::vec lambda_val(lambda.getSize());
    for (int j = 0; j < lambda.getSize(); j++) {
      lambda_val[j] = context.getCandidatePoint(lambda[j]);
    }

    // calculate actual loss at this lambda
    // compare that to the candidate objective
    // if actual loss is still greater than candidate loss,
    // add a new cut to improve the candidate
    // us
    //
    // Vmin = candidate? incumbent? value of objective function (uses cutting planes for loss function)
    // Vmax = best value using actual loss function?

    // add lazy constraint
    double loss_point = computer->loss(lambda_val);
    arma::vec loss_slope = computer->loss_grad(lambda_val);

    // check that loss_point is ?
    double L_val = context.getCandidatePoint(L);
    if (abs(loss_point - L_val) <= 1E-8) {
      //env.out() << "close enough, not adding a new constraint" << endl;
      return;
    }

    //  L >= loss_approx + sum_expr(colwise(loss_grad_approx[j]) * lambda[j], j = 1:d) - q
    IloNumExpr sum_expr = IloExpr(context.getEnv());
    for (int j = 0; j < lambda.getSize(); j++) {
      sum_expr += loss_slope[j]*(lambda[j] - lambda_val[j]);
    }

    context.rejectCandidate(L - loss_point - sum_expr >= 0.0);
    sum_expr.end();

  }

  // This is the function that we have to implement and that CPLEX will call
  // during the solution process at the places that we asked for.
  virtual void invoke(const IloCplex::Callback::Context &context) ILO_OVERRIDE;

  // Destructor
  ~LossCutCallback() {};
};


// Implementation of the invoke method.
//
// This is the method that we have to implement to fulfill the
// generic callback contract. CPLEX will call this method during
// the solution process at the places that we asked for.
void LossCutCallback::invoke(const IloCplex::Callback::Context &context) {
  if (context.inCandidate()) {
    lazyCut(context);
  }
}


// [[Rcpp::export]]
Rcpp::List lcpa_cpp(arma::mat x, arma::vec y, int R_max = 3, int time_limit = 60) {
  // create environment
  IloEnv env;

  // initialize the loss computer
  // add intercept to x beforehand
  LossComputer computer(x, y);

  // inputs
  int d = x.n_cols;
  double c0 = 1E-8;
  int R_min = 1;
  R_max = std::min(R_max, d);
  int intercept_min = -10;
  int intercept_max = 10;
  int coef_min = -5;
  int coef_max = 5;
  double L_min = 0;
  double L_max = IloInfinity;
  double V_min = 0;
  double V_max = IloInfinity;

  try {
    // create model
    IloModel model(env);

    // create variables
    IloIntVarArray alpha(env, d, 0, 1);
    IloIntVarArray lambda(env, d);
    IloIntVar R(env, R_min, R_max);
    IloNumVar L(env, L_min, L_max);
    IloNumVar V(env, V_min, V_max);

    // add constraints
    for (IloInt j = 0; j < d; j++) {

      if (j == 0) {
        lambda[j] = IloIntVar(env, intercept_min, intercept_max);
        model.add(lambda[j] - intercept_min*alpha[j] >= 0);
        model.add(lambda[j] - intercept_max*alpha[j] <= 0);
      } else {
        lambda[j] = IloIntVar(env, coef_min, coef_max);
        model.add(lambda[j] - coef_min*alpha[j] >= 0);
        model.add(lambda[j] - coef_max*alpha[j] <= 0);
      }
    }

    model.add(R == IloSum(alpha));
    model.add(V == L + c0*R);

    // add objective
    model.add(IloMinimize(env, V));

    // define optimizer
    IloCplex cplex(model);

    // add lazy callback
    // We instantiate a FacilityCallback and set the contextMask parameter.
    LossCutCallback cb(lambda, L, &computer);
    CPXLONG contextMask = 0;
    contextMask |= IloCplex::Callback::Context::Id::Candidate;
    cplex.use(&cb, contextMask);

    // cplex parameters
    cplex.setParam(IloCplex::Param::TimeLimit, time_limit); // 60 seconds

    // solve model
    if ( !cplex.solve() ) {
      Rcpp::stop("Failed to optimize LP.");
    }

    // write out model for inspection
    //cplex.exportModel("model2.lp");


    IloNumArray alpha_vals(env);
    cplex.getValues(alpha_vals, alpha);
    std::vector<int> alpha_vec;

    IloNumArray lambda_vals(env);
    cplex.getValues(lambda_vals, lambda);
    std::vector<int> lambda_vec;

    for (int j = 0; j < d; j++) {
      alpha_vec.push_back(alpha_vals[j]);
      lambda_vec.push_back(lambda_vals[j]);
    }

    Rcpp::List result = Rcpp::List::create(
      //Rcpp::Named("status") = cplex.getStatus(),
      Rcpp::Named("objective_value") = cplex.getObjValue(),
      Rcpp::Named("alpha") = alpha_vec,
      Rcpp::Named("lambda") = lambda_vec
    );

    env.end();
    return result;
    //env.out() << "optimality gap: " << 1 - cplex.getObjValue()/(computer.loss(arma::vec(lambda_vec)) + c0*cplex.getValue(R)) << endl;
    //env.out() << "titanic should have been lambda = [-3, 1, 2, 0]: " << computer.loss(arma::vec("-3;1;2;0")) + c0*3 << endl;

  }
  catch (IloException& e) {
    Rcpp::stop("Concert exception caught");
  }
  catch (...) {
    Rcpp::stop("Unknown exception caught");
  }

  env.end();
  return Rcpp::List::create();
}

