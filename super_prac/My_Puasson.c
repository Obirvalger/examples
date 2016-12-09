// This is the explicit conjugate gradient method for descrete Puasson problem 
// on uniform mesh. 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <mpi.h>

// Domain size.
const double A = 4.0;
const double B =  4.0;

int NX, NY, nx, ny;							        // the number of internal points on axes (ox) and (oy).
double hx, hy;                              // mesh steps on (0x) and (0y) axes

#define Test
#define Print
#define Step 10

#define Max(A,B) ((A)>(B)?(A):(B))
#define Min(A,B) ((A)>(B)?(B):(A))
#define R2(x,y) ((x)*(x)+(y)*(y))
#define Cube(x) ((x)*(x)*(x))
#define x(i) ((i)*hx)
#define y(j) ((j)*hy)

#define LeftPart(P,i,j)\
((-(P[NX*(j)+i+1]-P[NX*(j)+i])/hx+(P[NX*(j)+i]-P[NX*(j)+i-1])/hx)/hx+\
 (-(P[NX*(j+1)+i]-P[NX*(j)+i])/hy+(P[NX*(j)+i]-P[NX*(j-1)+i])/hy)/hy)

double LeftPartDep(double* ptr, double* shadows, int i, int j)
{
	double leftPoint;
	double rightPoint;
	double upPoint;
	double downPoint;
	if(j >= 1)
		upPoint = ptr[nx*(j-1)+i];
	else
		upPoint = shadows[i];

	if(j < ny - 1)
		downPoint = ptr[nx*(j+1)+i];
	else
		downPoint = shadows[nx + i];

	if(i >= 1)
		leftPoint = ptr[nx*(j)+i-1];
	else
		leftPoint = shadows[2*nx + j];

	if(i < nx - 1)
		rightPoint = ptr[nx*(j)+i+1];
	else
		rightPoint = shadows[2*nx + ny + j];

	return (-(rightPoint-ptr[nx*(j)+i])/hx+
			(ptr[nx*(j)+i]-leftPoint)/hx)/hx+
           (-(downPoint-ptr[nx*(j)+i])/hy+
           	 (ptr[nx*(j)+i]-upPoint)/hy)/hy;
}

double Solution(double x,double y)
{    
    return sqrt(4 + x * y);
}

double BoundaryValue(double x, double y)
{
    return Solution(x,y);
}

int RightPart(double * rhs, int nx, int ny, int shiftX, int shiftY)
{
    int i, j, k;
    /*for(j=0; j<ny; j++)
        for(i=0; i<nx; i++)*/
	#pragma omp parallel for private(i,j)
    for(k = 0; k < nx*ny; k++) {
    	i = k % nx;
    	j = k / nx;
        rhs[j*nx+i] = 0.25 * (x(i + shiftX) * x(i + shiftX) + y(j + shiftY) * y(j + shiftY)) /
    							     pow(4 + x(i + shiftX) * y(j+ shiftY), 1.5);
    }

    return 0;
}

double norm(double* vect)
{
	return 0.0;
}

int IsPower(int Number)
// the function returns log_{2}(Number) if it is integer. If not it returns (-1). 
{
    unsigned int M;
    int p;
    
    if(Number <= 0)
        return(-1);
        
    M = Number; p = 0;
    while(M % 2 == 0)
    {
        ++p;
        M = M >> 1;
    }
    if((M >> 1) != 0)
        return(-1);
    else
        return(p);
    
}

int SplitFunction(int N0, int N1, int p)
// This is the splitting procedure of proc. number p. The integer p0
// is calculated such that abs(N0/p0 - N1/(p-p0)) --> min.
{
    float n0, n1;
    int p0, i;
    
    n0 = (float) N0; n1 = (float) N1;
    p0 = 0;
    
    for(i = 0; i < p; i++)
        if(n0 > n1)
        {
            n0 = n0 / 2.0;
            ++p0;
        }
        else
            n1 = n1 / 2.0;
    
    return(p0);
}

int main(int argc, char * argv[])
{
	double * SolVect;					    // the solution array
    double * ResVect;					    // the residual array
	double * BasisVect;					    // the vector of A-orthogonal system in CGM
	double * RHS_Vect;					    // the right hand side of Puasson equation.
	double * ErrVect;
	/*double * ShadowLeft;
	double * ShadowRight;
	double * ShadowUp;
	double * ShadowDown;*/
	double * shadows;
	double sp, alpha, tau, NewValue, err, globalSp, globalTau, globalErr, globalAlpha;	// auxiliary values
	int SDINum, CGMNum;					    // the number of steep descent and CGM iterations.
	int counter;						    // the current iteration number.

	int i,j,k;
	char str[127];
	double *lbuf, *rbuf;
	FILE * fp;
	int ProcNum, rank;              // the number of processes and rank in communicator.
    int power, px, py;              // ProcNum = 2^(power), power splits into sum p0 + p1.
    int dims[2];                    // dims[0] = 2^p0, dims[1] = 2^p1 (--> M = dims[0]*dims[1]).
    int kx,ky;               // NX = nx*dims[1] + kx, NY = ny*dims[0] + ky.
    int Coords[2];                  // the process coordinates in the cartesian topology created for mesh.
    int domainPartXl = 0, domainPartXr = 0;//part of domen on the current proc
    int domainPartYl = 0, domainPartYr = 0;//part of domen on the current proc
    
    MPI_Comm Grid_Comm;             // this is a handler of a new communicator.
    MPI_Request reqSend[4];//requests for sending shadows
    MPI_Request reqRecv[4];//requests for receiving shadows
    MPI_Status stSend[4], stRecv[4];
    const int ndims = 2;            // the number of a process topology dimensions.
    int periods[2] = {0,0};         // it is used for creating processes topology.
    int left, right, up, down;      // the neighbours of the process.
    int xStart, yStart, xEnd, yEnd;
    int wait = 0;

    // MPI Library is being activated ...
    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD,&ProcNum);
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    // command line analyser
	switch (argc)
	{
	case 4:{
				SDINum = 1;
				CGMNum = atoi(argv[3]);
				break;
		   }
	case 5:{
				SDINum = Max(atoi(argv[3]),1);		// SDINum >= 1
				CGMNum = atoi(argv[4]);
				break;
		   }
	default:{
				printf("Wrong number of parameters in command line.\nUsage: <ProgName> "
                       "<Nodes number on (0x) axis> <Nodes number on (0y) axis> "
                       "[the number of steep descent iterations] "
                       "<the number of conjugate gragient iterations>\nFinishing...\n");
				return(-1);
			}
	}
    
    NX = atoi(argv[1]); NY = atoi(argv[2]);
    hx = A / (NX-1);    hy = B / (NY-1);
    
    if((NX <= 0)||(NY <= 0))
    {
        if(rank == 0)
            printf("The first and the second arguments (mesh numbers) should be positive.\n");
        
        MPI_Finalize();
        return(2);
    }
    
    if((power = IsPower(ProcNum)) < 0)
    {
        if(rank == 0)
            printf("The number of procs must be a power of 2.\n");
        MPI_Finalize();
        return(3);
    }
    
    py = SplitFunction(NY, NX, power);
    px = power - py;
    
    dims[0] = (unsigned int) 1 << py;   dims[1] = (unsigned int) 1 << px;
    ny = NY >> py;                      nx = NX >> px;
    ky = NY - dims[0]*ny;               kx = NX - dims[1]*nx;

    lbuf = (double*)malloc(ny*sizeof(double));
    rbuf = (double*)malloc(ny*sizeof(double));

    // the cartesian topology of processes is being created ...
    MPI_Cart_create(MPI_COMM_WORLD, ndims, dims, periods, 1, &Grid_Comm);
    MPI_Comm_rank(Grid_Comm, &rank);
    MPI_Cart_coords(Grid_Comm, rank, ndims, Coords);
    
    if(Coords[0] < ky)
    {
        ++ny;
        domainPartYl = ny * Coords[0];
        domainPartYr = domainPartYl + ny;
    } 
    else
    {
    	domainPartYl = (ny + 1) * ky + ny * (Coords[0] - ky);
        domainPartYr = domainPartYl + ny;
    }

    if(Coords[1] < kx)
    {
        ++nx;
        domainPartXl = nx * Coords[1];
        domainPartXr = domainPartXl + nx;
    }
    else
    {
		domainPartXl = (nx + 1) * kx + nx * (Coords[1] - kx);
        domainPartXr = domainPartXl + nx;
    }

     
    
    MPI_Cart_shift(Grid_Comm, 0, 1, &up, &down);
    MPI_Cart_shift(Grid_Comm, 1, 1, &left, &right);
    
/*#ifdef Print
    printf("My Rank in Grid_Comm is %d. My topological coords is (%d,%d). Domain size is %d x %d nodes.\n"
           "My neighbours: left = %d, right = %d, down = %d, up = %d.\n",
           rank, Coords[0], Coords[1], nx, ny, left,right, down,up);
#endif*/

	/*sprintf(str,"PuassonSerial_ECGM_%dx%d_proc_%d.log", NX, NY, rank);
	fp = fopen(str,"w");
	fprintf(fp,"My Rank in Grid_Comm is %d. My topological coords is (%d,%d). Domain size is %d x %d nodes.\n"
               "My neighbours: left = %d, right = %d, down = %d, up = %d.\n"
			   "The Domain Part: [%f,%f]x[%f,%f], number of points: N[0,A] = %d, N[0,B] = %d;\n"
			   "The steep descent iterations number: %d\n"
			   "The conjugate gradient iterations number: %d\n",
			   rank, Coords[0], Coords[1], nx, ny, left,right, down,up,
			   domainPartXl*hx,Min(domainPartXr*hx, A),domainPartYl*hy, Min(domainPartYr*hy, B), nx,ny, SDINum,CGMNum);
	*/
	SolVect   = (double *)malloc(nx*ny*sizeof(double));
	ErrVect   = (double *)malloc(nx*ny*sizeof(double));
	ResVect   = (double *)malloc(nx*ny*sizeof(double));
	RHS_Vect  = (double *)malloc(nx*ny*sizeof(double));
	/*ShadowLeft = (double *)malloc(ny*1*sizeof(double));
	ShadowRight = (double *)malloc(ny*1*sizeof(double));
	ShadowUp = (double *)malloc(1*nx*sizeof(double));
	ShadowDown = (double *)malloc(1*nx*sizeof(double));*/
	shadows = (double *)malloc((2*nx + 2*ny)*sizeof(double));
// Initialization of Arrays
	memset(ResVect,0,nx*ny*sizeof(double));
	memset(SolVect,0,nx*ny*sizeof(double));
	memset(ErrVect,0,nx*ny*sizeof(double));
	/*memset(ShadowLeft,0,ny*1*sizeof(double));
	memset(ShadowRight,0,ny*1*sizeof(double));
	memset(ShadowUp,0,1*nx*sizeof(double));
	memset(ShadowDown,0,1*nx*sizeof(double));*/
	memset(shadows,0,(2*nx + 2*ny)*sizeof(double));
	MPI_Barrier(MPI_COMM_WORLD);
	double time = MPI_Wtime();
	//fprintf(fp,"%d %d\n", domainPartXl, domainPartYl);
    RightPart(RHS_Vect, nx, ny, domainPartXl, domainPartYl);
	/*for (j=0; j < ny; j++)
	{
		for (i=0; i < nx; i++)
			fprintf(fp, "%f ", RHS_Vect[nx*j+i]);
		fprintf(fp, "\n");
	}*/
    if(Coords[0] == 0)
		#pragma omp parallel for
		for(i=0; i<nx; i++)
			SolVect[i] = BoundaryValue(x(i + domainPartXl),0.0);

	if(Coords[0] == dims[0] - 1)
		#pragma omp parallel for
		for(i=0; i<nx; i++)
			SolVect[nx*(ny-1)+i] = BoundaryValue(x(i + domainPartXl),B);

	if(Coords[1] == 0)
		#pragma omp parallel for
		for(j=0; j<ny; j++)
			SolVect[nx*j] = BoundaryValue(0.0,y(j + domainPartYl));

	if(Coords[1] == dims[1] - 1)
		#pragma omp parallel for
		for(j=0; j<ny; j++)
			SolVect[nx*j+(nx-1)] = BoundaryValue(A,y(j + domainPartYl));
	/*fprintf(fp, "solvect:\n");
	for (j=0; j < ny; j++)
	{
		for (i=0; i < nx; i++)
			fprintf(fp, "%f ", SolVect[nx*j+i]);
		fprintf(fp, "\n");
	}*/
// Iterations ...
	/*#ifdef Test
		err = 0.0;
		for(j=1; j < NY-1; j++)
			for(i=1; i < NX-1; i++)
				err = Max(err, fabs(Solution(x(i),y(j))-SolVect[NX*j+i]));
		fprintf(fp,"\nNo iterations have been performed. The residual error is %.12f\n", err);
	#endif*/
	

// Steep descent iterations begin ...
    
	/*#ifdef Print
		if(rank == 0)
			printf("\nSteep descent iterations begin ...\n");
	#endif*/

	for(counter=1; counter<=SDINum; counter++)
	{
		wait = 0;
		if(Coords[0] == 0) 
		{
			yStart = 1;
			memcpy(shadows, SolVect, nx*sizeof(double));
		}
		else
		{

			yStart = 0;
			MPI_Isend(SolVect, nx, MPI_DOUBLE, up, 0, Grid_Comm, &reqSend[wait]);//send to up neighbour
			MPI_Irecv(shadows, nx, MPI_DOUBLE, up, MPI_ANY_TAG, Grid_Comm, &reqRecv[wait]);//recv shadow from upper neighbour
			wait++;
		}

		if(Coords[0] == dims[0] - 1) 
		{
			yEnd = ny - 1;
			memcpy(shadows + nx, SolVect + nx*(ny-1), nx*sizeof(double));
		}
		else
		{

			yEnd = ny;
			MPI_Isend(SolVect + nx*(ny - 1), nx, MPI_DOUBLE, down, 0, Grid_Comm, &reqSend[wait]);//send to down neighbour
			MPI_Irecv(shadows + nx, nx, MPI_DOUBLE, down, 0, Grid_Comm, &reqRecv[wait]);//recv shadow from down neighbour
			wait++;
		}

		if(Coords[1] == dims[1] - 1) 
		{
			xEnd = nx - 1; 
			for(j=0; j < ny; j++)
			{
				shadows[2*nx + ny + j] = SolVect[nx*j+(nx-1)];
			}
		}
		else
		{

			xEnd = nx;
			for(j=0; j < ny; j++)
			{
				rbuf[j] = SolVect[nx*j+(nx-1)];
			}
			MPI_Isend(rbuf, ny, MPI_DOUBLE, right, 0, Grid_Comm, &reqSend[wait]);//send to right neighbour
			MPI_Irecv(shadows + 2*nx + ny, ny, MPI_DOUBLE, right, MPI_ANY_TAG, Grid_Comm, &reqRecv[wait]);//recv shadow from right neighbour
			wait++;
		}

		if(Coords[1] == 0) 
		{
			xStart = 1;
			for(j=0; j < ny; j++)
			{
				shadows[2*nx + j] = SolVect[nx*j];
			}
		}
		else
		{

			xStart = 0;
			for(j=0; j < ny; j++)
			{
				lbuf[j] = SolVect[nx*j];
			}
			MPI_Isend(lbuf, ny, MPI_DOUBLE, left, 0, Grid_Comm, &reqSend[wait]);//send to left neighbour
			MPI_Irecv(shadows + 2*nx, ny, MPI_DOUBLE, left, MPI_ANY_TAG, Grid_Comm, &reqRecv[wait]);//recv shadow from left neighbour
			wait++;
		}
		//printf("waiting recv to complete\n");
		//TODO:Wait fo recv to complete
		if(wait != 0)
			MPI_Waitall(wait, reqRecv, stRecv);
// The residual vector r(k) = Ax(k)-f is calculating ...
		/*printf("xstart: %d; xend: %d; ystart: %d; yend: %d\n", xStart, xEnd,yStart,yEnd);
		if(rank == 0) {
			printf("ShadowUp:\n");
			for (i=0; i < nx; i++)
				printf("%f ", ShadowUp[i]);
			printf("\nShadowDown:\n");
			for (i=0; i < nx; i++)
				printf("%f ", ShadowDown[i]);
			printf("\nShadowLeft:\n");
			for (i=0; i < ny; i++)
				printf("%f ", ShadowLeft[i]);
			printf("\nShadowRight:\n");
			for (i=0; i < ny; i++)
				printf("%f ", ShadowRight[i]);
		}*/
		#pragma omp parallel for private(i,j)
		for(k = 0; k < (yEnd - yStart) * (xEnd - xStart); ++k) {
			i = k % (xEnd - xStart) + xStart;
			j = k / (xEnd - xStart) + yStart;
			ResVect[nx*j+i] = LeftPartDep(SolVect, shadows, i, j)-RHS_Vect[nx*j+i];
		}



// The value of product (r(k),r(k)) is calculating ...
		sp = 0.0;
		#pragma omp parallel for private(i,j) reduction(+:sp)
		for(k = 0; k < (yEnd - yStart) * (xEnd - xStart); ++k) {
			i = k % (xEnd - xStart) + xStart;
			j = k / (xEnd - xStart) + yStart;
			sp += ResVect[nx*j+i]*ResVect[nx*j+i]*hx*hy;
		}
		MPI_Allreduce(&sp, &tau, 1, MPI_DOUBLE, MPI_SUM, Grid_Comm);
		//fprintf(fp, "tau:%f\n", tau);
		if(wait != 0)
			MPI_Waitall(wait, reqSend, stSend);//wait all sending to complete, to reuse requests
		wait = 0;
		//fprintf(fp, "resvect:\n");
		/*for (j=0; j < ny; j++)
		{
			for (i=0; i < nx; i++)
				fprintf(fp, "%f ", ResVect[nx*j+i]);
			fprintf(fp, "\n");
		}
		fprintf(fp, "Coords[0]: %d\n", Coords[0]);*/
		if(Coords[0] == 0) 
		{
			yStart = 1;
			memcpy(shadows, ResVect, nx*sizeof(double));
		}
		else
		{

			yStart = 0;
			MPI_Isend(ResVect, nx, MPI_DOUBLE, up, 0, Grid_Comm, &reqSend[wait]);//send to up neighbour
			MPI_Irecv(shadows, nx, MPI_DOUBLE, up, 0, Grid_Comm, &reqRecv[wait]);//recv shadow from upper neighbour
			wait++;
		}
		//fprintf(fp, "dims[0]: %d\n", dims[0]);
		if(Coords[0] == dims[0] - 1) 
		{
			yEnd = ny - 1;
			memcpy(shadows + nx, ResVect + nx*(ny-1), nx*sizeof(double));
		}
		else
		{

			yEnd = ny;
			MPI_Isend(ResVect + nx*(ny - 1), nx, MPI_DOUBLE, down, 0, Grid_Comm, &reqSend[wait]);//send to down neighbour
			MPI_Irecv(shadows + nx, nx, MPI_DOUBLE, down, 0, Grid_Comm, &reqRecv[wait]);//recv shadow from down neighbour
			wait++;
		}
		//fprintf(fp, "Coords[1]: %d; dims[1]: %d\n", Coords[1], dims[1]);
		if(Coords[1] == dims[1] - 1) 
		{
			xEnd = nx - 1; 
			for(j=0; j < ny; j++)
			{
				shadows[2*nx + ny + j] = ResVect[nx*j+(nx-1)];
			}
		}
		else
		{

			xEnd = nx;
			for(j=0; j < ny; j++)
			{
				rbuf[j] = ResVect[nx*j+(nx-1)];
			}
			
			MPI_Isend(rbuf, ny, MPI_DOUBLE, right, 0, Grid_Comm, &reqSend[wait]);//send to right neighbour
			MPI_Irecv(shadows + 2*nx + ny, ny, MPI_DOUBLE, right, MPI_ANY_TAG, Grid_Comm, &reqRecv[wait]);//recv shadow from right neighbour
			wait++;
		}

		if(Coords[1] == 0) 
		{
			xStart = 1;
			for(j=0; j < ny; j++)
			{
				shadows[2*nx + j] = ResVect[nx*j];
			}
		}
		else
		{

			xStart = 0;
			for(j=0; j < ny; j++)
			{
				lbuf[j] = ResVect[nx*j];
			}
			MPI_Isend(lbuf, ny, MPI_DOUBLE, left, 0, Grid_Comm, &reqSend[wait]);//send to left neighbour
			MPI_Irecv(shadows + 2*nx, ny, MPI_DOUBLE, left, MPI_ANY_TAG, Grid_Comm, &reqRecv[wait]);//recv shadow from left neighbour
			wait++;
		}
		if(wait != 0)
			MPI_Waitall(wait, reqRecv, stRecv);
// The value of product sp = (Ar(k),r(k)) is calculating ...
		//fprintf(fp, "sp\n");
		sp = 0.0;
		globalSp = 0.;
		#pragma omp parallel for private(i,j) reduction(+:sp)
		for(k = 0; k < (yEnd - yStart) * (xEnd - xStart); ++k) {
			i = k % (xEnd - xStart) + xStart;
			j = k / (xEnd - xStart) + yStart;
			sp += LeftPartDep(ResVect, shadows, i, j)*ResVect[nx*j+i]*hx*hy;
		}
		MPI_Allreduce(&sp, &globalSp, 1, MPI_DOUBLE, MPI_SUM, Grid_Comm);
		//fprintf(fp,"sp:%f\n", globalSp);
		tau = tau/globalSp;
		sp = globalSp;
// The x(k+1) is calculating ...
		memset(ErrVect,0,nx*ny*sizeof(double));
		#pragma omp parallel for private(i,j)
		for(k = 0; k < (yEnd - yStart) * (xEnd - xStart); ++k) {
			i = k % (xEnd - xStart) + xStart;
			j = k / (xEnd - xStart) + yStart;
			NewValue = SolVect[nx*j+i]-tau*ResVect[nx*j+i];
			//printf("new_val: %f\n", NewValue);
			ErrVect[nx*j+i] = NewValue-SolVect[nx*j+i];
			SolVect[nx*j+i] = NewValue;
		}
		#pragma omp parallel for private(i,j) reduction(+:err)
		for(k = 0; k < (yEnd - yStart) * (xEnd - xStart); ++k) {
			i = k % (xEnd - xStart) + xStart;
			j = k / (xEnd - xStart) + yStart;
			err += ErrVect[nx*j+i]*ErrVect[nx*j+i]*hx*hy;
		}
		globalErr = 0.;
		MPI_Allreduce(&err, &globalErr, 1, MPI_DOUBLE, MPI_SUM, Grid_Comm);//find max error

    	err = sqrt(globalErr);
    }
// the end of steep descent iteration.
    
	BasisVect = ResVect;    // g(0) = r(k-1).
    ResVect = (double *)malloc(nx*ny*sizeof(double));
    memset(ResVect,0,nx*ny*sizeof(double));
    /*fprintf(fp, "basisvect:\n");
	for (j=0; j < ny; j++)
	{
		for (i=0; i < nx; i++)
			fprintf(fp, "%f ", BasisVect[nx*j+i]);
		fprintf(fp, "\n");
	}*/
// CGM iterations begin ...
// sp == (Ar(k-1),r(k-1)) == (Ag(0),g(0)), k=1.
	/*if(rank == 0)
		printf("\nCGM iterations begin ...\n");*/

	//for(counter=1; counter<=CGMNum; counter++)
	counter = 1;
	err = 1.;
    while(err > 0.0001)
	{
		wait = 0;
		if(Coords[0] == 0) 
		{
			yStart = 1;
			memcpy(shadows, SolVect, nx*sizeof(double));
		}
		else
		{

			yStart = 0;
			MPI_Isend(SolVect, nx, MPI_DOUBLE, up, 0, Grid_Comm, &reqSend[wait]);//send to up neighbour
			MPI_Irecv(shadows, nx, MPI_DOUBLE, up, 0, Grid_Comm, &reqRecv[wait]);//recv shadow from upper neighbour
			wait++;
		}

		if(Coords[0] == dims[0] - 1) 
		{
			yEnd = ny - 1;
			memcpy(shadows + nx, SolVect + nx*(ny-1), nx*sizeof(double));
		}
		else
		{

			yEnd = ny;
			MPI_Isend(SolVect + nx*(ny - 1), nx, MPI_DOUBLE, down, 0, Grid_Comm, &reqSend[wait]);//send to down neighbour
			MPI_Irecv(shadows + nx, nx, MPI_DOUBLE, down, 0, Grid_Comm, &reqRecv[wait]);//recv shadow from down neighbour
			wait++;
		}

		if(Coords[1] == dims[1] - 1) 
		{
			xEnd = nx - 1; 
			for(j=0; j < ny; j++)
			{
				shadows[2*nx + ny + j] = SolVect[nx*j+(nx-1)];
			}
		}
		else
		{
			xEnd = nx;
			for(j=0; j < ny; j++)
			{
				rbuf[j] = SolVect[nx*j+(nx-1)];
			}
			MPI_Isend(rbuf, ny, MPI_DOUBLE, right, 0, Grid_Comm, &reqSend[wait]);//send to right neighbour
			MPI_Irecv(shadows + 2*nx + ny, ny, MPI_DOUBLE, right, MPI_ANY_TAG, Grid_Comm, &reqRecv[wait]);//recv shadow from right neighbour
			wait++;
		}

		if(Coords[1] == 0) 
		{
			xStart = 1;
			for(j=0; j < ny; j++)
			{
				shadows[2*nx + j] = SolVect[nx*j];
			}
		}
		else
		{

			xStart = 0;
			for(j=0; j < ny; j++)
			{
				lbuf[j] = SolVect[nx*j];
			}
			MPI_Isend(lbuf, ny, MPI_DOUBLE, left, 0, Grid_Comm, &reqSend[wait]);//send to left neighbour
			MPI_Irecv(shadows + 2*nx, ny, MPI_DOUBLE, left, MPI_ANY_TAG, Grid_Comm, &reqRecv[wait]);//recv shadow from left neighbour
			wait++;
		}
		if(wait != 0)
			MPI_Waitall(wait, reqRecv, stRecv);
	// The residual vector r(k) is calculating ...
		#pragma omp parallel for private(i,j)
		for(k = 0; k < (yEnd - yStart) * (xEnd - xStart); ++k) {
			i = k % (xEnd - xStart) + xStart;
			j = k / (xEnd - xStart) + yStart;
			ResVect[nx*j+i] = LeftPartDep(SolVect, shadows, i, j)-RHS_Vect[nx*j+i];
		//		fprintf(fp, "(%d;%d) %f %f\n", i + domainPartXl, j + domainPartYl, ResVect[nx*j+i], RHS_Vect[nx*j+i]);
		}
		if(wait != 0)
			MPI_Waitall(wait, reqSend, stSend);
		wait = 0;
		if(Coords[0] == 0) 
		{
			yStart = 1;
			memcpy(shadows, ResVect, nx*sizeof(double));
		}
		else
		{

			yStart = 0;
			MPI_Isend(ResVect, nx, MPI_DOUBLE, up, 0, Grid_Comm, &reqSend[wait]);//send to up neighbour
			MPI_Irecv(shadows, nx, MPI_DOUBLE, up, 0, Grid_Comm, &reqRecv[wait]);//recv shadow from upper neighbour
			wait++;
		}

		if(Coords[0] == dims[0] - 1) 
		{
			yEnd = ny - 1;
			memcpy(shadows + nx, ResVect + nx*(ny-1), nx*sizeof(double));
		}
		else
		{

			yEnd = ny;
			MPI_Isend(ResVect + nx*(ny - 1), nx, MPI_DOUBLE, down, 0, Grid_Comm, &reqSend[wait]);//send to down neighbour
			MPI_Irecv(shadows + nx, nx, MPI_DOUBLE, down, 0, Grid_Comm, &reqRecv[wait]);//recv shadow from down neighbour
			wait++;
		}

		if(Coords[1] == dims[1] - 1) 
		{
			xEnd = nx - 1; 
			for(j=0; j < ny; j++)
			{
				shadows[2*nx + ny + j] = ResVect[nx*j+(nx-1)];
			}
		}
		else
		{

			xEnd = nx;
			for(j=0; j < ny; j++)
			{
				rbuf[j] = ResVect[nx*j+(nx-1)];
			}
			MPI_Isend(rbuf, ny, MPI_DOUBLE, right, 0, Grid_Comm, &reqSend[wait]);//send to right neighbour
			MPI_Irecv(shadows + 2*nx + ny, ny, MPI_DOUBLE, right, MPI_ANY_TAG, Grid_Comm, &reqRecv[wait]);//recv shadow from right neighbour
			wait++;
		}

		if(Coords[1] == 0) 
		{
			xStart = 1;
			for(j=0; j < ny; j++)
			{
				shadows[2*nx + j] = ResVect[nx*j];
			}
		}
		else
		{

			xStart = 0;
			for(j=0; j < ny; j++)
			{
				lbuf[j] = ResVect[nx*j];
			}
			MPI_Isend(lbuf, ny, MPI_DOUBLE, left, 0, Grid_Comm, &reqSend[wait]);//send to left neighbour
			MPI_Irecv(shadows + 2*nx, ny, MPI_DOUBLE, left, MPI_ANY_TAG, Grid_Comm, &reqRecv[wait]);//recv shadow from left neighbour
			wait++;
		}
		if(wait != 0)
			MPI_Waitall(wait, reqRecv, stRecv);
	// The value of product (Ar(k),g(k-1)) is calculating ...
		alpha = 0.;
		globalAlpha = 0.;
		//fprintf(fp, "alpha\n");
		#pragma omp parallel for private(i,j) reduction(+:alpha)
		for(k = 0; k < (yEnd - yStart) * (xEnd - xStart); ++k) {
			i = k % (xEnd - xStart) + xStart;
			j = k / (xEnd - xStart) + yStart;
			alpha += LeftPartDep(ResVect, shadows, i, j)*BasisVect[nx*j+i]*hx*hy;
				//fprintf(fp, "(%d;%d) %f %f\n", i + domainPartXl, j + domainPartYl, alpha, BasisVect[nx*j+i]);
		}
		MPI_Allreduce(&alpha, &globalAlpha, 1, MPI_DOUBLE, MPI_SUM, Grid_Comm);
		//fprintf(fp,"local alpha: %f; alpha: %f, sp: %f\n", alpha, globalAlpha, sp);
		alpha = globalAlpha/sp;
	// The new basis vector g(k) is being calculated ...
		#pragma omp parallel for
		for(k = 0; k < (yEnd - yStart) * (xEnd - xStart); ++k) {
			i = k % (xEnd - xStart) + xStart;
			j = k / (xEnd - xStart) + yStart;
			BasisVect[nx*j+i] = ResVect[nx*j+i]-alpha*BasisVect[nx*j+i];
				//printf("%f\n", BasisVect[nx*j + i]);
		}

	// The value of product (r(k),g(k)) is being calculated ...
		tau = 0.0;
		globalTau = 0.;
		#pragma omp parallel for private(i,j) reduction(+:tau)
		for(k = 0; k < (yEnd - yStart) * (xEnd - xStart); ++k) {
			i = k % (xEnd - xStart) + xStart;
			j = k / (xEnd - xStart) + yStart;
			tau += ResVect[nx*j+i]*BasisVect[nx*j+i]*hx*hy;
		}
		MPI_Allreduce(&tau, &globalTau, 1, MPI_DOUBLE, MPI_SUM, Grid_Comm);
		tau = globalTau;
		//fprintf(fp,"tau1: %f\n", tau);
		if(wait != 0)
			MPI_Waitall(wait, reqSend, stSend);
	// The value of product sp = (Ag(k),g(k)) is being calculated ...
		wait = 0;
		if(Coords[0] == 0) 
		{
			yStart = 1;
			memcpy(shadows, BasisVect, nx*sizeof(double));
		}
		else
		{

			yStart = 0;
			MPI_Isend(BasisVect, nx, MPI_DOUBLE, up, 0, Grid_Comm, &reqSend[wait]);//send to up neighbour
			MPI_Irecv(shadows, nx, MPI_DOUBLE, up, 0, Grid_Comm, &reqRecv[wait]);//recv shadow from upper neighbour
			wait++;
		}

		if(Coords[0] == dims[0] - 1) 
		{
			yEnd = ny - 1;
			memcpy(shadows + nx, BasisVect + nx*(ny-1), nx*sizeof(double));
		}
		else
		{

			yEnd = ny;
			MPI_Isend(BasisVect + nx*(ny - 1), nx, MPI_DOUBLE, down, 0, Grid_Comm, &reqSend[wait]);//send to down neighbour
			MPI_Irecv(shadows + nx, nx, MPI_DOUBLE, down, 0, Grid_Comm, &reqRecv[wait]);//recv shadow from down neighbour
			wait++;
		}

		if(Coords[1] == dims[1] - 1) 
		{
			xEnd = nx - 1; 
			for(j=0; j < ny; j++)
			{
				shadows[2*nx + ny + j] = BasisVect[nx*j+(nx-1)];
			}
		}
		else
		{

			xEnd = nx;
			for(j=0; j < ny; j++)
			{
				rbuf[j] = BasisVect[nx*j+(nx-1)];
			}
			MPI_Isend(rbuf, ny, MPI_DOUBLE, right, 0, Grid_Comm, &reqSend[wait]);//send to right neighbour
			MPI_Irecv(shadows + 2*nx + ny, ny, MPI_DOUBLE, right, MPI_ANY_TAG, Grid_Comm, &reqRecv[wait]);//recv shadow from right neighbour
			wait++;
		}

		if(Coords[1] == 0) 
		{

			xStart = 1;
			for(j=0; j < ny; j++)
			{
				shadows[2*nx + j] = BasisVect[nx*j];
			}
		}
		else
		{

			xStart = 0;
			for(j=0; j < ny; j++)
			{
				lbuf[j] = BasisVect[nx*j];
			}
			MPI_Isend(lbuf, ny, MPI_DOUBLE, left, 0, Grid_Comm, &reqSend[wait]);//send to left neighbour
			MPI_Irecv(shadows + 2*nx, ny, MPI_DOUBLE, left, MPI_ANY_TAG, Grid_Comm, &reqRecv[wait]);//recv shadow from left neighbour
			wait++;
		}
		if(wait != 0)
			MPI_Waitall(wait, reqRecv, stRecv);
		sp = 0.0;
		globalSp = 0.;
		#pragma omp parallel for private(i,j) reduction(+:sp)
		for(k = 0; k < (yEnd - yStart) * (xEnd - xStart); ++k) {
			i = k % (xEnd - xStart) + xStart;
			j = k / (xEnd - xStart) + yStart;
			sp += LeftPartDep(BasisVect, shadows, i, j)*BasisVect[nx*j+i]*hx*hy;
			//printf("qsp: %f\n", sp);
		}
			
		MPI_Allreduce(&sp, &globalSp, 1, MPI_DOUBLE, MPI_SUM, Grid_Comm);
		//fprintf(fp,"sp1: %f %f\n", globalSp, sp);
		tau = tau/globalSp;
		//fprintf(fp,"tau2: %f\n", tau);
		sp = globalSp;
	// The x(k+1) is being calculated ...
		err = 0.0;
		globalErr = 0.;
		memset(ErrVect,0,nx*ny*sizeof(double));
		#pragma omp parallel for private(i,j) reduction(+:sp)
		for(k = 0; k < (yEnd - yStart) * (xEnd - xStart); ++k) {
			i = k % (xEnd - xStart) + xStart;
			j = k / (xEnd - xStart) + yStart;
			NewValue = SolVect[nx*j+i]-tau*BasisVect[nx*j+i];
			ErrVect[nx*j+i] = NewValue-SolVect[nx*j+i];
			SolVect[nx*j+i] = NewValue;
		}
		#pragma omp parallel for private(i,j) reduction(+:err)
		for(k = 0; k < (yEnd - yStart) * (xEnd - xStart); ++k) {
			i = k % (xEnd - xStart) + xStart;
			j = k / (xEnd - xStart) + yStart;
			err += ErrVect[nx*j+i]*ErrVect[nx*j+i]*hx*hy;
		}
		globalErr = 0.;
		MPI_Allreduce(&err, &globalErr, 1, MPI_DOUBLE, MPI_SUM, Grid_Comm);//find max error

		err = sqrt(globalErr);

        if(err < 0.0001)
            break;
		/*if(rank == 0 && counter%Step == 0)
		{
			printf("The %d iteration of CGM method has been carried out.\n", counter);
            
#ifdef Print            
            fprintf(fp,"\nThe iteration %d of conjugate gradient method has been finished.\n"
                       "The value of \\alpha(k) = %f, \\tau(k) = %f. The difference value is %f.\n",\
                        counter, alpha, tau, err);
#endif
		}*/
        counter++;
	}
// the end of CGM iterations.
    time = MPI_Wtime() - time;
	double max_time = 0.;
	MPI_Reduce(&time, &max_time, 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);
// printing some results ...
	if(rank == 0) 
	{
		/*fprintf(fp,"\nThe %d iterations are carried out. The error of iterations is estimated by %.12f.\n",
                SDINum+CGMNum, err);*/
		//fclose(fp);
		printf("Time: %f\n", max_time);
	}
	sprintf(str,"PuassonSerial_ECGM_%dx%d_%d.dat", NX, NY, ProcNum);


	MPI_Barrier(Grid_Comm);
	/*fprintf(fp,"# This is the conjugate gradient method for descrete Puasson equation.\n"
			"# A = %f, B = %f, N[0,A] = %d, N[0,B] = %d, SDINum = %d, CGMNum = %d.\n"
			"# One can draw it by gnuplot by the command: splot 'MyPath\\FileName.dat' with lines\n",\
			A, B, nx, ny, SDINum, CGMNum);*/

	for (j=0; j < ny; j++)
	{
		for(k = 0; k < ProcNum; ++k) {
			if(k == rank) {
				fp = fopen(str,"a");
				for (i=0; i < nx; i++)
					fprintf(fp,"\n%f %f %f", Min(x(i + domainPartXl), A), Min(y(j + domainPartYl), B), SolVect[nx*j+i]);
				fclose(fp);
			}
			MPI_Barrier(Grid_Comm);
		}
		if(rank == 0) {
			fp = fopen(str,"a");
			fprintf(fp,"\n");
			fclose(fp);
		}

	}

	

	free(SolVect); free(ResVect); free(BasisVect); free(RHS_Vect);
	MPI_Finalize();
	return(0);
}
