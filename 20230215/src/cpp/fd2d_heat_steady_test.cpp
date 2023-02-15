# include <cmath>
# include <cstdlib>
# include <ctime>
# include <fstream>
# include <iostream>
# include <iomanip>
# include <unistd.h>
# include "client.h"
#include <getopt.h>

using namespace std;

# include "fd2d_heat_steady.hpp"

int main ( int argc, char** argv);
void test01 (int nx, int ny);
double d ( double x, double y );
double f ( double x, double y );
void boundary ( int nx, int ny, double x[], double y[], int n, double a[], 
  double rhs[] );

//****************************************************************************80

int main (int argc, char** argv)

//****************************************************************************80
//
//  Purpose:
//
//    fd2d_heat_steady_test() tests fd2d_heat_steady().
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    29 August 2013
//
//  Author:
//
//    John Burkardt
//
{
  timestamp ( );
  cout << "\n";
  cout << "FD2D_HEAT_STEADY_TEST:\n";
  cout << "  C++ version\n";
  cout << "  Test the FD2D_HEAT_STEADY library.\n";

  int nx = 41, ny = 21;
  int c;
  while (1) {
    static struct option long_options[] = {
      {"nx", required_argument, 0, 'x'},
      {"ny", required_argument, 0, 'y'},
      {0, 0, 0, 0}
    };
    int option_index = 0;
    c = getopt_long(argc, argv, "x:y:", long_options, &option_index);
    if (c == -1) break;
    switch (c) {
      case 'x':
        nx = std::stoi(optarg);
        break;
      case 'y':
        ny = std::stoi(optarg);
        break;
      default:
        std::cerr << "Usage: " << argv[0] << " --nx=<value> --ny=<value>" << std::endl;
        return 1;
    }
  }

  test01 (nx, ny );
//
//  Terminate.
//
  cout << "\n";
  cout << "FD2D_HEAT_STEADY_TEST:\n";
  cout << "  Normal end of execution.\n";
  cout << "\n";
  timestamp ( );

  return 0;
}
//****************************************************************************80

void test01 (int nx, int ny )

//****************************************************************************80
//
//  Purpose:
//
//    TEST01 computes the solution for a steady state heat equation problem.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    29 August 2013
//
//  Author:
//
//    John Burkardt
//
{
  string command_filename = "test01_commands.txt";
  ofstream command_unit;
  string data_filename = "test01_data.txt";
  ofstream data_unit;
  int i;
  int j;
  double *umat;
  double u_mean;
  double *xmat;
  double *xvec;
  double *ymat;
  double *yvec;
//
//  Specify the spatial grid.
//

  xvec = r8vec_linspace_new ( nx, 0.0, 2.0 );

  yvec = r8vec_linspace_new ( ny, 0.0, 1.0 );

  xmat = new double[nx*ny];
  ymat = new double[nx*ny];
  r8vec_mesh_2d ( nx, ny, xvec, yvec, xmat, ymat );
//
//  Solve the finite difference approximation to the steady 2D heat equation.
//
  umat = fd2d_heat_steady ( nx, ny, xvec, yvec, d, f );

  std::vector<size_t> dims = {(size_t)ny, (size_t)nx};
  bool cluster_mode = false; // Set to false if not using a clustered database
  SmartRedis::Client client(cluster_mode);
  std::string key_u = "steady_state_u";
  std::string key_x = "steady_state_x";
  std::string key_y = "steady_state_y";

  client.put_tensor(key_u, umat, dims, SRTensorTypeDouble, SRMemLayoutContiguous);
  client.put_tensor(key_x, xmat, dims, SRTensorTypeDouble, SRMemLayoutContiguous);
  client.put_tensor(key_y, ymat, dims, SRTensorTypeDouble, SRMemLayoutContiguous);
//
//  Report the average value of U.
//
  u_mean = 0.0;
  for ( j = 0; j < ny; j++ )
  {
    for ( i = 0; i < nx; i++ )
    {
      u_mean = u_mean + umat[i+j*nx];
    }
  }
  u_mean = u_mean / ( double ) ( nx * ny );

  cout << "\n";
  cout << "  Mean value of U is " << u_mean << "\n";
//
//  Free memory.
//
  delete [] umat;
  delete [] xmat;
  delete [] xvec;
  delete [] ymat;
  delete [] yvec;

  return;
}
//****************************************************************************80

double d ( double x, double y )

//****************************************************************************80
//
//  Purpose:
//
//    D evaluates the heat conductivity coefficient.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    29 August 2013
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double X, Y, the evaluation point.
//
//    Output, double D, the value of the heat conductivity at (X,Y).
//
{
  double value;

  value = 1.0;

  return value;
}
//****************************************************************************80

double f ( double x, double y )

//****************************************************************************80
//
//  Purpose:
//
//    F evaluates the heat source term.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    29 August 2013
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double X, Y, the evaluation point.
//
//    Output, double F, the value of the heat source term at (X,Y).
//
{
  double value;

  value = 0.0;

  return value;
}
//****************************************************************************80

void boundary ( int nx, int ny, double x[], double y[], int n, double a[], 
  double rhs[] )

//****************************************************************************80
//
//  Purpose:
//
//    BOUNDARY sets up the matrix and right hand side at boundary nodes.
//
//  Discussion:
//
//    For this simple problem, the boundary conditions specify that the solution
//    is 100 on the left side, and insulated on the right, top and bottom.
//
//    Nodes are assigned a single index K, which increases as:
//
//    (NY-1)*NX+1  (NY-1)*NX+2  ...  NY * NX
//           ....         ....  ...    .....
//           NX+1         NX+2  ...   2 * NX
//              1            2  ...       NX
//
//    The index K of a node on the lower boundary satisfies:
//      1 <= K <= NX
//    The index K of a node on the upper boundary satisfies:
//      (NY-1)*NX+1 <= K <= NY * NX
//    The index K of a node on the left boundary satisfies:
//      mod ( K, NX ) = 1
//    The index K of a node on the right boundary satisfies:
//      mod ( K, NX ) = 0
//
//    If we number rows from bottom I = 1 to top I = NY
//    and columns from left J = 1 to right J = NX, then the relationship
//    between the single index K and the row and column indices I and J is:
//      K = ( I - 1 ) * NX + J
//    and
//      J = 1 + mod ( K - 1, NX )
//      I = 1 + ( K - J ) / NX
//      
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    29 August 2013
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int NX, NY, the number of grid points in X and Y.
//
//    Input, double X[NX], Y[NY], the coordinates of grid lines.
//
//    Input, int N, the number of nodes.
//
//    Input/output, double A[N*N].  On input, the system matrix, with the 
//    entries for the interior nodes filled in.  On output, the entries for
//    the boundary nodes have been set as well.
//
//    Input, double RHS[N], on input, the system right hand side, 
//    with the entries for the interior nodes filled in.  On output, the entries for
//    the boundary nodes have been set as well.
//
{
  int i;
  int j;
  int kc;
//
//  Left boundary.
//
  j = 0;
  for ( i = 1; i < ny - 1; i++ )
  {
    kc = i * nx + j;
    a[kc+kc*n] = a[kc+kc*n] + 1.0;
    rhs[kc] = 10.0;
  }
//
//  Right boundary.
//
  j = nx - 1;
  for ( i = 1; i < ny - 1; i++ )
  {
    kc = i * nx + j;
    a[kc+kc*n] = a[kc+kc*n] + 1.0;
    rhs[kc] = 100.0;
  }
//
//  Lower boundary.
//
  i = 0;
  for ( j = 0; j < nx; j++ )
  {
    kc = i * nx + j;
    a[kc+kc*n] = a[kc+kc*n] + 1.0;
    rhs[kc] = 0.0;
  }
//
//  Upper boundary.
//
  i = ny - 1;
  for ( j = 0; j < nx; j++ )
  {
    kc = i * nx + j;
    a[kc+kc*n] = a[kc+kc*n] + 1.0;
    rhs[kc] = 0.0;
  }

  return;
}

