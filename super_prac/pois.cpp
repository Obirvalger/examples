#include <iostream>
using std::fstream;
using std::string;
using std::cout;
using std::endl;
using std::swap;
#include <algorithm>
using std::max;
#include <stdexcept>
using std::runtime_error;
//#include <math.h> 
#include <cmath>
using std::abs;
#include <mpi.h>
#include <string>
#include <sstream>
using std::stringstream;
#include <fstream>
#include <cstdlib>
#include <ctime>
//#include <stdlib.h>
#ifdef _OPENMP
#include <omp.h>
#endif

// using namespace std;

string int_to_str(int number) {
	stringstream ss;
	ss << number;
	return ss.str();
}

double F(const double x, const double y) {
    return 4*(2 - 3*x*x - 3*y*y);
}

double phi(const double x, const double y) {
    return pow(1.0 - x*x,2) + pow(1.0 - y*y,2);
}

void compute_grid_processes_number(const int& nproc, int& x_proc_num, int& y_proc_num) {
    if (nproc >= 512) {
        x_proc_num = 16;
        y_proc_num = 32;
    } else if (nproc >= 256) {
        x_proc_num = 16;
        y_proc_num = 16;
    } else if (nproc >= 128) {
        x_proc_num = 8;
        y_proc_num = 16;
    } else if (nproc >= 64) {
        x_proc_num = 8;
        y_proc_num = 8;
    } else if (nproc >= 32) {
        x_proc_num = 4;
        y_proc_num = 8;
    } else if (nproc >= 16) {
        x_proc_num = 4;
        y_proc_num = 4;
    } else if (nproc >= 8){
        x_proc_num = 2;
        y_proc_num = 4;
    } else if (nproc >= 4) {
        x_proc_num = 2;
        y_proc_num = 2;
    } else if (nproc >= 2) {
        x_proc_num = 1;
        y_proc_num = 2;
    } else if (nproc >= 1) {
        x_proc_num = 1;
        y_proc_num = 1;
    } else {
        throw runtime_error("Incorrect processes number");
    }
}

struct GridParameters {
	int rank, N1, N2, p1, p2, x_index_from, x_index_to, y_index_from, y_index_to;
	double *x_grid, *y_grid;
	double eps;
    bool top, bottom, left, right;

    double *send_message_top, *send_message_bottom, *send_message_left, *send_message_right;
    double *recv_message_top, *recv_message_bottom, *recv_message_left, *recv_message_right;
    MPI_Request* send_requests;
    MPI_Request* recv_requests;
    MPI_Comm comm;

	GridParameters (int rank, MPI_Comm comm, double* x_grid, double* y_grid, int N1, int N2, int p1, int p2, double eps):
		rank (rank), comm (comm), x_grid (x_grid), y_grid (y_grid), 
		send_message_top (NULL), send_message_bottom (NULL), send_message_left (NULL), send_message_right (NULL),
		recv_message_top (NULL), recv_message_bottom (NULL), recv_message_left (NULL), recv_message_right (NULL),
		send_requests (NULL), recv_requests (NULL),
		N1 (N1), N2 (N2),p1 (p1), p2 (p2), eps (eps), 
		x_index_from (0), x_index_to (0), y_index_from (0), y_index_to (0),
		top (false), bottom (false), left (false), right (false) {
			int step1, step2;
			step1 = int(floor(1.0 * N1 / p1));
			step2 = int(floor(1.0 * N2 / p2));
			x_index_from = int(floor(1.0 * step1 * floor(1.0 * rank / p2)));
			y_index_from = int(floor((double(rank % p2)) * step2));

			if ((rank + 1) % p2 == 0)
				y_index_to = N2;
			else
				y_index_to = y_index_from + step2; 

			if (rank >= (p1-1)*p2)
				x_index_to = N1;
			else
				x_index_to = x_index_from + step1;

			if (x_index_from == 0)
				top = true;
			if (y_index_from == 0)
				left = true;
			if (y_index_to == N1)
				right = true;
			if (x_index_to == N1)
				bottom = true;
		}

	int get_num_x_points() {
		if (bottom) 
			return x_index_to - x_index_from + 1;
		else
			return x_index_to - x_index_from;
	}

	int get_num_y_points() {
		if (right) 
			return y_index_to - y_index_from + 1;
		else
			return y_index_to - y_index_from;
	}

	void get_real_grid_index(int i, int j, int& grid_i, int& grid_j) {
		grid_i = x_index_from + i;
		grid_j = y_index_from + j;
	}

	double get_x_grid_value(int grid_i) {
		return x_grid[grid_i];
	}

	double get_y_grid_value(int grid_j) {
		return y_grid[grid_j];
	}

	double get_x_h_step(int grid_i) {
		return x_grid[grid_i+1] - x_grid[grid_i];
	}

	double get_y_h_step(int grid_j) {
		return y_grid[grid_j+1] - y_grid[grid_j];
	}

	int get_top_rank() {
		return rank - p2;
	}

	int get_bottom_rank() {
		return rank + p2;
	}

	int get_left_rank() {
		return rank - 1;
	}

	int get_right_rank() {
		return rank + 1;
	}

	bool is_border_point(int grid_i, int grid_j) {
		if ((grid_i == 0) || (grid_j == 0) || (grid_i == N1) || (grid_j == N2))
			return true;
		else
			return false;
	}
};

double scalar_product(GridParameters gp, const double* f1, const double* f2) {
	double product = 0.0;

	#pragma omp parallel for reduction(+:product)
	for (int i=0; i<gp.get_num_x_points(); i++){
        for (int j=0; j<gp.get_num_y_points(); j++){
        	int grid_i, grid_j;
	    	gp.get_real_grid_index(i, j, grid_i, grid_j);
        	if (not gp.is_border_point(grid_i, grid_j)) {
	        	double average_hx = (gp.get_x_h_step(grid_i) + gp.get_x_h_step(grid_i-1)) / 2.0;
	        	double average_hy = (gp.get_y_h_step(grid_j) + gp.get_y_h_step(grid_j-1)) / 2.0;
	        	//printf("! average_hx=%f average_hy=%f i=%d j=%d f1[i,j]=%f f2[i,j]=%f\n", average_hx, average_hy, i, j, f1[i*gp.get_num_y_points()+j], f2[i*gp.get_num_y_points()+j]);
	            product += average_hx * average_hy * f1[i*gp.get_num_y_points()+j] * f2[i*gp.get_num_y_points()+j];
	        }
        }
    }

    double global_product = 0.0;
    int status = MPI_Allreduce(&product, &global_product, 1, MPI_DOUBLE, MPI_SUM, gp.comm);
    if (status != MPI_SUCCESS) throw runtime_error("Error in compute scalar_product!");
    //printf("rank %d: product=%f global_product=%f\n", gp.rank, product, global_product);
    return global_product;
}

void compute_delta(GridParameters gp, const double *func, double *delta_func, double f_top, double f_bottom, double f_left, double f_right, int i, int j, int grid_i, int grid_j) {
	double h_i_1 = gp.get_x_h_step(grid_i-1);
	double h_i = gp.get_x_h_step(grid_i);
	double h_j_1 = gp.get_y_h_step(grid_j-1);
	double h_j = gp.get_y_h_step(grid_j);
	double average_hx = (h_i + h_i_1) / 2.0;
	double average_hy = (h_j + h_j_1) / 2.0;
	double f_curr = func[i*gp.get_num_y_points()+j];
	delta_func[i*gp.get_num_y_points()+j] = 
		(1.0 / average_hx) * ((f_curr - f_top) / h_i_1 - (f_bottom - f_curr) / h_i) + 
		(1.0 / average_hy) * ((f_curr - f_left) / h_j_1 - (f_right - f_curr) / h_j);
	//printf("i=%d j=%d grid_i=%d grid_j=%d average_hx=%f average_hy=%f h_i_1=%f h_i=%f h_j_1=%f h_j=%f f_curr=%f f_top=%f f_bottom=%f f_left=%f f_right=%f delta_func[i][j] = %f\n", i, j, grid_i, grid_j, average_hx, average_hy, h_i_1, h_i, h_j_1, h_j, f_curr, f_top, f_bottom, f_left, f_right, delta_func[i*gp.get_num_y_points()+j]);
}

enum MPI_tags { SendToTop, SendToBottom, SendToLeft, SendToRight};

void compute_approx_delta(GridParameters gp, double* delta_func, const double* func) {
	// compute inner points
	int i, j;
	#pragma omp parallel for 
	for (i=1; i<gp.get_num_x_points()-1; i++) {
    	for (j=1; j<gp.get_num_y_points()-1; j++) {
    		int grid_i, grid_j;
    		gp.get_real_grid_index(i, j, grid_i, grid_j);
    		compute_delta(gp, func, delta_func, func[(i-1)*gp.get_num_y_points()+j], func[(i+1)*gp.get_num_y_points()+j], func[i*gp.get_num_y_points()+j-1], func[i*gp.get_num_y_points()+j+1], i, j, grid_i, grid_j);
    	}
	}

	if (gp.send_message_top == NULL)
		gp.send_message_top = new double [gp.get_num_y_points()];
	if (gp.send_message_bottom == NULL)
		gp.send_message_bottom = new double [gp.get_num_y_points()];
	if (gp.send_message_left == NULL)
		gp.send_message_left = new double [gp.get_num_x_points()];
	if (gp.send_message_right == NULL)
		gp.send_message_right = new double [gp.get_num_x_points()];

	if (gp.recv_message_top == NULL)
		gp.recv_message_top = new double [gp.get_num_y_points()];
	if (gp.recv_message_bottom == NULL)
		gp.recv_message_bottom = new double [gp.get_num_y_points()];
	if (gp.recv_message_left == NULL)
		gp.recv_message_left = new double [gp.get_num_x_points()];
	if (gp.recv_message_right == NULL)
		gp.recv_message_right = new double [gp.get_num_x_points()];

	if (gp.send_requests == NULL)
		gp.send_requests = new MPI_Request [4];
	if (gp.recv_requests == NULL)
		gp.recv_requests = new MPI_Request [4];

	for (int j=0; j<gp.get_num_y_points(); j++)
		gp.send_message_top[j] = func[0*gp.get_num_y_points()+j];
	for (int j=0; j<gp.get_num_y_points(); j++)
		gp.send_message_bottom[j] = func[(gp.get_num_x_points()-1)*gp.get_num_y_points()+j];
	for (int i=0; i<gp.get_num_x_points(); i++)
		gp.send_message_left[i] = func[i*gp.get_num_y_points()+0];
	for (int i=0; i<gp.get_num_x_points(); i++)
		gp.send_message_right[i] = func[i*gp.get_num_y_points()+gp.get_num_y_points()-1];

	int status;
	int send_count=0;
	if (not gp.top) {
		status = MPI_Isend(gp.send_message_top, gp.get_num_y_points(), MPI_DOUBLE, 
			gp.get_top_rank(), SendToTop, gp.comm, &(gp.send_requests[send_count]));
		if (status != MPI_SUCCESS) throw runtime_error("Error in send message!");
		send_count++;
	}
	if (not gp.bottom) {
		status = MPI_Isend(gp.send_message_bottom, gp.get_num_y_points(), MPI_DOUBLE, 
			gp.get_bottom_rank(), SendToBottom, gp.comm, &(gp.send_requests[send_count]));
		if (status != MPI_SUCCESS) throw runtime_error("Error in send message!");
		send_count++;
	}
	if (not gp.left) {
		status = MPI_Isend(gp.send_message_left, gp.get_num_x_points(), MPI_DOUBLE, 
			gp.get_left_rank(), SendToLeft, gp.comm, &(gp.send_requests[send_count]));
		if (status != MPI_SUCCESS) throw runtime_error("Error in send message!");
		send_count++;
	}
	if (not gp.right) {
		status = MPI_Isend(gp.send_message_right, gp.get_num_x_points(), MPI_DOUBLE, 
			gp.get_right_rank(), SendToRight, gp.comm, &(gp.send_requests[send_count]));
		if (status != MPI_SUCCESS) throw runtime_error("Error in send message!");
		send_count++;
	}

	int recv_count=0;
	if (not gp.top) {
		status = MPI_Irecv(gp.recv_message_top, gp.get_num_y_points(), MPI_DOUBLE, 
			gp.get_top_rank(), SendToBottom, gp.comm, &(gp.recv_requests[recv_count]));
		if (status != MPI_SUCCESS) throw runtime_error("Error in receive message!");
		recv_count++;
	}
	if (not gp.bottom) {
		status = MPI_Irecv(gp.recv_message_bottom, gp.get_num_y_points(), MPI_DOUBLE, 
			gp.get_bottom_rank(), SendToTop, gp.comm, &(gp.recv_requests[recv_count]));
		if (status != MPI_SUCCESS) throw runtime_error("Error in receive message!");
		recv_count++;
	}
	if (not gp.left) {
		status = MPI_Irecv(gp.recv_message_left, gp.get_num_x_points(), MPI_DOUBLE, 
			gp.get_left_rank(), SendToRight, gp.comm, &(gp.recv_requests[recv_count]));
		if (status != MPI_SUCCESS) throw runtime_error("Error in receive message!");
		recv_count++;
	}
	if (not gp.right) {
		status = MPI_Irecv(gp.recv_message_right, gp.get_num_x_points(), MPI_DOUBLE, 
			gp.get_right_rank(), SendToLeft, gp.comm, &(gp.recv_requests[recv_count]));
		if (status != MPI_SUCCESS) throw runtime_error("Error in receive message!");
		recv_count++;
	}

	status = MPI_Waitall(recv_count, gp.recv_requests, MPI_STATUS_IGNORE);
    if (status != MPI_SUCCESS) throw runtime_error("Error in waiting receive message!");

    status = MPI_Waitall(send_count, gp.send_requests, MPI_STATUS_IGNORE);
    if (status != MPI_SUCCESS) throw runtime_error("Error in waiting send message!");

    if (not gp.top) {
    	int i = 0;
    	for (int j=1; j<gp.get_num_y_points()-1; j++) {
    		int grid_i, grid_j;
    		gp.get_real_grid_index(i, j, grid_i, grid_j);
    		compute_delta(gp, func, delta_func, gp.recv_message_top[j], func[(i+1)*gp.get_num_y_points()+j], func[i*gp.get_num_y_points()+j-1], func[i*gp.get_num_y_points()+j+1], i, j, grid_i, grid_j);
    	}
    }

	if (not gp.bottom) {
    	int i = gp.get_num_x_points()-1;
    	for (int j=1; j<gp.get_num_y_points()-1; j++) {
    		int grid_i, grid_j;
    		gp.get_real_grid_index(i, j, grid_i, grid_j);
    		compute_delta(gp, func, delta_func, func[(i-1)*gp.get_num_y_points()+j], gp.recv_message_bottom[j], func[i*gp.get_num_y_points()+j-1], func[i*gp.get_num_y_points()+j+1], i, j, grid_i, grid_j);
    	}
    }

    if (not gp.left) {
    	int j = 0;
    	for (int i=1; i<gp.get_num_x_points()-1; i++) {
    		int grid_i, grid_j;
    		gp.get_real_grid_index(i, j, grid_i, grid_j);
    		compute_delta(gp, func, delta_func, func[(i-1)*gp.get_num_y_points()+j], func[(i+1)*gp.get_num_y_points()+j], gp.recv_message_left[i], func[i*gp.get_num_y_points()+j+1], i, j, grid_i, grid_j);
    	}
    }

    if (not gp.right) {
    	int j = gp.get_num_y_points()-1;
    	for (int i=1; i<gp.get_num_x_points()-1; i++) {
    		int grid_i, grid_j;
    		gp.get_real_grid_index(i, j, grid_i, grid_j);
    		compute_delta(gp, func, delta_func, func[(i-1)*gp.get_num_y_points()+j], func[(i+1)*gp.get_num_y_points()+j], func[i*gp.get_num_y_points()+j-1], gp.recv_message_right[i], i, j, grid_i, grid_j);
    	}
    }

    // compute corners
	i = 0; j = 0;
	if (not gp.top && not gp.left) {
		int grid_i, grid_j;
    	gp.get_real_grid_index(i, j, grid_i, grid_j);
    	compute_delta(gp, func, delta_func, gp.recv_message_top[j], func[(i+1)*gp.get_num_y_points()+j], gp.recv_message_left[i], func[i*gp.get_num_y_points()+j+1], i, j, grid_i, grid_j);
	}

	i = 0; j = gp.get_num_y_points()-1;
	if (not gp.top && not gp.right) {
		int grid_i, grid_j;
    	gp.get_real_grid_index(i, j, grid_i, grid_j);
    	compute_delta(gp, func, delta_func, gp.recv_message_top[j], func[(i+1)*gp.get_num_y_points()+j], func[i*gp.get_num_y_points()+j-1], gp.recv_message_right[i], i, j, grid_i, grid_j);
	}

	i = gp.get_num_x_points()-1; j = 0;
	if (not gp.bottom && not gp.left) {
		int grid_i, grid_j;
    	gp.get_real_grid_index(i, j, grid_i, grid_j);
    	compute_delta(gp, func, delta_func, func[(i-1)*gp.get_num_y_points()+j], gp.recv_message_bottom[j], gp.recv_message_left[i], func[i*gp.get_num_y_points()+j+1], i, j, grid_i, grid_j);
	}

	i = gp.get_num_x_points()-1; j = gp.get_num_y_points()-1;
	if (not gp.bottom && not gp.right) {
		int grid_i, grid_j;
    	gp.get_real_grid_index(i, j, grid_i, grid_j);
    	compute_delta(gp, func, delta_func, func[(i-1)*gp.get_num_y_points()+j], gp.recv_message_bottom[j], func[i*gp.get_num_y_points()+j-1], gp.recv_message_right[i], i, j, grid_i, grid_j);
	}
}

void compute_r(GridParameters gp, double *r, const double *delta_p) {
	int i, j;

	#pragma omp parallel for 
	for (i=0; i<gp.get_num_x_points(); i++) {
    	for (j=0; j<gp.get_num_y_points(); j++) {
    		int grid_i, grid_j;
    		gp.get_real_grid_index(i, j, grid_i, grid_j);
    		if (gp.is_border_point(grid_i, grid_j))
				r[i*gp.get_num_y_points()+j] = 0.0;
    		else 
    			r[i*gp.get_num_y_points()+j] = delta_p[i*gp.get_num_y_points()+j] - F(gp.get_x_grid_value(grid_i), gp.get_y_grid_value(grid_j));
    	}
	}
}

void compute_g(GridParameters gp, double *g, double *r, double alpha) {
	int i, j;

	#pragma omp parallel for 
	for (i=0; i<gp.get_num_x_points(); i++) {
    	for (j=0; j<gp.get_num_y_points(); j++) {
    		int grid_i, grid_j;
    		gp.get_real_grid_index(i, j, grid_i, grid_j);
    		g[i*gp.get_num_y_points()+j] = r[i*gp.get_num_y_points()+j] - alpha * g[i*gp.get_num_y_points()+j];
    	}
	}
}

void compute_p(GridParameters gp, double *p, double* p_prev, double *g, double tau) {
	int i, j;

	#pragma omp parallel for 
	for (i=0; i<gp.get_num_x_points(); i++) {
    	for (j=0; j<gp.get_num_y_points(); j++) {
    		int grid_i, grid_j;
    		gp.get_real_grid_index(i, j, grid_i, grid_j);
    		p[i*gp.get_num_y_points()+j] = p_prev[i*gp.get_num_y_points()+j] - tau * g[i*gp.get_num_y_points()+j];
    	}
	}
}

double compute_norm(GridParameters gp, double *p, double *p_prev) {
	double norm = 0.0;
	double norm_shared = 0.0;

	int i, j;
	#pragma omp parallel shared(norm_shared) firstprivate(norm) 
	{
		for (i=0; i<gp.get_num_x_points(); i++) {
	    	for (j=0; j<gp.get_num_y_points(); j++) {
	    		int grid_i, grid_j;
	    		gp.get_real_grid_index(i, j, grid_i, grid_j);
	    		norm = max(norm, abs(p[i*gp.get_num_y_points()+j] -       \
                            p_prev[i*gp.get_num_y_points()+j]));
	    	}
		}
		#pragma omp critical 
	  	{
	      if(norm > norm_shared) norm_shared = norm;
		}
	}

	double global_norm = 0.0;
	int status = MPI_Allreduce(&norm_shared, &global_norm, 1, MPI_DOUBLE, MPI_MAX, gp.comm);
    if (status != MPI_SUCCESS) throw runtime_error("Error in compute scalar_product!");
    //printf("rank %d: norm=%f global_norm=%f\n", gp.rank, norm, global_norm);
    return global_norm;
}

void init_vector(GridParameters gp, double* func) {
	int i, j;

	#pragma omp parallel for 
	for (i=0; i<gp.get_num_x_points(); i++) {
    	for (j=0; j<gp.get_num_y_points(); j++) {
    		func[i*gp.get_num_y_points()+j] = 0.0;
		}
	}
}

void init_p_prev(GridParameters gp, double* p_prev) {
	int i, j;

	#pragma omp parallel for 
	for (i=0; i<gp.get_num_x_points(); i++) {
    	for (j=0; j<gp.get_num_y_points(); j++) {
    		int grid_i, grid_j;
    		gp.get_real_grid_index(i, j, grid_i, grid_j);
    		if (not gp.is_border_point(grid_i, grid_j)) {
                p_prev[i*gp.get_num_y_points()+j] = 0.0;
            }
            else {
                p_prev[i*gp.get_num_y_points()+j] = phi(gp.get_x_grid_value(grid_i), gp.get_y_grid_value(grid_j));
            }
		}
	}
}

void write_func_to_file(GridParameters gp, double *func, string func_name) {
	string name= "output/"+func_name + "_" + int_to_str(gp.rank) + ".txt"; 
	fstream fout (name.c_str(), fstream::out);
	for (int i=0; i<gp.get_num_x_points(); i++) {
    	for (int j=0; j<gp.get_num_y_points(); j++) {
    		int grid_i, grid_j;
    		gp.get_real_grid_index(i, j, grid_i, grid_j);
    		fout << func_name << "[" << grid_i <<"][" << grid_j <<"]=" << func[i*gp.get_num_y_points()+j] << " x=" << gp.get_x_grid_value(grid_i) << " y=" << gp.get_y_grid_value(grid_j) << endl;
    	}
	}
}

void write_two_func_to_file(GridParameters gp, double *func1, string func1_name, double *func2, string func2_name) {
	string name = "output/" + func1_name + "_" + int_to_str(gp.rank) + ".txt"; 
    // cout << name << endl;
	fstream fout (name.c_str(), fstream::out);

	fout << "x,y," << func1_name << "," << func2_name << endl;
	for (int i=0; i<gp.get_num_x_points(); i++) {
    	for (int j=0; j<gp.get_num_y_points(); j++) {
    		int grid_i, grid_j;
    		gp.get_real_grid_index(i, j, grid_i, grid_j);
    		fout << gp.get_x_grid_value(grid_i) << "," << gp.get_y_grid_value(grid_j) << "," << func1[i*gp.get_num_y_points()+j] << "," << func2[i*gp.get_num_y_points()+j] << endl;
    	}
	}
}

int main (int argc, char** argv) {
	if (argc != 3)
		throw runtime_error("Incorrect number of arguments");
	clock_t start_time = clock();

	const double A1 = 0.0;
	const double A2 = 3.0;
	const double B1 = 0.0;
	const double B2 = 3.0;

	const int N1 = atoi(argv[1]);
	const int N2 = atoi(argv[2]);

	const double eps = 0.0001;

    double final_inaccuracy = 0.0;

	double* x_grid = new double [N1+1];
	double* y_grid = new double [N2+1];

	for (int i=0; i<=N1; i++) {
		x_grid[i] = A2 * 1.0*i/N1 + A1 * (1 - 1.0*i/N1);
	}
	for (int j=0; j<=N2; j++) {
		y_grid[j] = B2 * 1.0*j/N2 + B1 * (1 - 1.0*j/N2);
	}

	int rank, nproc;
	int p1, p2;
	int x_index_from, x_index_to, y_index_from, y_index_to;

	MPI_Init (&argc, &argv);	/* starts MPI */
	MPI_Comm_rank (MPI_COMM_WORLD, &rank);	/* get current process id */
	MPI_Comm_size (MPI_COMM_WORLD, &nproc);	/* get number of processes */

	compute_grid_processes_number(nproc, p1, p2);

	// filter extra processes
	if (rank < p1 * p2) {
		if (rank == 0) {
            printf("size = %d x %d\n", N1, N2);
	       	#ifdef _OPENMP
	        printf("Threads = %d\n", omp_get_max_threads());
	        #endif
			printf("p1 = %d p2 = %d nproc = %d\n", p1, p2, nproc);
	    }

	    GridParameters gp(rank, MPI_COMM_WORLD, x_grid, y_grid, N1, N2, p1, p2, eps);
	    
	    double* p = new double [gp.get_num_x_points() * gp.get_num_y_points()];
	    double* p_prev = new double [gp.get_num_x_points() * gp.get_num_y_points()];
	    double* g = new double [gp.get_num_x_points() * gp.get_num_y_points()];
	    double* r = new double [gp.get_num_x_points() * gp.get_num_y_points()];
	    double* delta_p = new double [gp.get_num_x_points() * gp.get_num_y_points()];
	    double* delta_r = new double [gp.get_num_x_points() * gp.get_num_y_points()];
	    double* delta_g = new double [gp.get_num_x_points() * gp.get_num_y_points()];
	    
	    init_p_prev(gp, p_prev);

	    init_vector(gp, r);
	    init_vector(gp, g);
	    init_vector(gp, delta_p);
	    init_vector(gp, delta_g);
	    init_vector(gp, delta_r);

	    double scalar_product_delta_g_and_g = 1.0;
	    double scalar_product_delta_r_and_g = 1.0;
	    double scalar_product_r_and_g = 1.0;
	    double alpha = 0.0;
	    double tau = 0.0;

	    double* phi_on_grid = new double [gp.get_num_x_points() * gp.get_num_y_points()];
	    for (int i=0; i<gp.get_num_x_points(); i++) {
	    	for (int j=0; j<gp.get_num_y_points(); j++) {
	    		int grid_i, grid_j;
	    		gp.get_real_grid_index(i, j, grid_i, grid_j);
	    		phi_on_grid[i*gp.get_num_y_points()+j] =                      \
                    phi(gp.get_x_grid_value(grid_i), gp.get_y_grid_value(grid_j));
	    	}
		}

	    int n_iter = 1;
	    for (;;) {
	    	compute_approx_delta(gp, delta_p, p_prev);
	    	compute_r(gp, r, delta_p);

	    	if (n_iter > 1) {
	    		compute_approx_delta(gp, delta_r, r);
	    		scalar_product_delta_r_and_g = scalar_product(gp, delta_r, g);
	    		alpha = 1.0 * scalar_product_delta_r_and_g / scalar_product_delta_g_and_g;
	    	}

	    	if (n_iter > 1) 
	    		compute_g(gp, g, r, alpha);
	    	else 
            	swap(g, r);

            compute_approx_delta(gp, delta_g, g);
            if (n_iter > 1) {
            	scalar_product_r_and_g = scalar_product(gp, r, g);
            }
            else {
            	scalar_product_r_and_g = scalar_product(gp, g, g);
            }

            scalar_product_delta_g_and_g = scalar_product(gp, delta_g, g);
	        tau = 1.0 * scalar_product_r_and_g / scalar_product_delta_g_and_g;

	       	compute_p(gp, p, p_prev, g, tau);
	       	double norm_p_prev = compute_norm(gp, p, p_prev);
	       	/*
            double norm_p_phi = compute_norm(gp, p, phi_on_grid);
	       	if (rank == 0)
	       		printf("# iteration %d: norm_p_prev=%f norm_p_phi=%f\n",      \
                    n_iter, norm_p_prev, norm_p_phi);
            */ 
	       	if (norm_p_prev < gp.eps) {
                final_inaccuracy = compute_norm(gp, p, phi_on_grid);
            	break;
            }

            swap(p, p_prev);
	    	n_iter += 1;
	    }
	    //write_two_func_to_file(gp, p, "p", phi_on_grid, "phi_on_grid");
    	//write_func_to_file(gp, p, "p");
    	//write_func_to_file(gp, phi_on_grid, "phi_on_grid");
	}
	MPI_Finalize();

	if (rank == 0) {
		clock_t end_time = clock();
	  	double elapsed_secs = double(end_time - start_time) / CLOCKS_PER_SEC;
	  	printf("Inaccuracy %f\n", final_inaccuracy);
	  	printf("Algorithm finished! Elapsed time: %f sec\n", elapsed_secs);
	}
	return 0;
}
