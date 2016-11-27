#! /usr/bin/perl

use strict;
use warnings;
use feature qw(say);
use Data::Dumper;
use List::Util qw(sum);

my %times;
my $repeats = 5;

warn "repeats $repeats\n";

#for my $mn (500) {
for my $mn (500, 1000, 1500, 2000) {
#    for my $k (100) {
    for my $k (100, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000) {
#        for my $thr (1,4) {
        for my $thr (1,2,4,6,8) {
            my $t = mk(n => $mn, m => $mn, thr => $thr,
                k => $k, repeats => $repeats);
            warn "mn $mn thr $thr k $k time $t \n";

            $times{$mn}{$thr}{$k} = $t;
        }
    }
}

print Dumper({%times});

sub mk {
    my %h = (
        n => 500,
        m => 500,
        thr => 4,
        k => 500,
        repeats => 1,
        @_
    );
    my $code = <<EOF_CODE;
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <numeric>
#include <omp.h>

using namespace std;

#define N $h{n}
#define M $h{m}
#define STEP 0.01

#define N_DIMS 2

vector<vector<double> > points_matrix(N_DIMS, vector<double>(N * M));
vector<vector<int> > graph_partition;

double find_span(vector<double> &x, vector<int> &idx)
{ 
    double max_value = x[idx[0]];
    double min_value = x[idx[0]];
    
    #pragma omp parallel for
    for (size_t i = 1; i < idx.size(); i++) {
        max_value = (x[idx[i]] > max_value) ? x[idx[i]] : max_value;
        min_value = (x[idx[i]] < min_value) ? x[idx[i]] : min_value; 
    }
    
    return max_value;
}

vector<int> bisect(vector<int> idx)
{
    double max_span = 0;
    size_t max_ind = 0;
    
    #pragma omp parallel for num_threads($h{thr})
    for (size_t i = 0; i < points_matrix.size(); i++) {
        //cout<<"Num threads"<<omp_get_num_threads()<<endl;
        double cur_span = find_span(points_matrix[i], idx);
        if (cur_span > max_span) {
            max_span = cur_span;
            max_ind = i;
        }
    }
    
    vector<int> sorted_idx = idx;

    sort(sorted_idx.begin(), sorted_idx.end(), 
            [&](const int& a, const int& b) {
                return points_matrix[max_ind][a] < points_matrix[max_ind][b];
            }
        );

    return sorted_idx;
}

void recursive_bisection(vector<int> idx, int k)
{
    if (k < 2) {
        graph_partition.push_back(idx);
        return;
    }
    
    int k1 = (k + 1) / 2;
    int k2 = k - k1;

    vector<int> sorted_idx1 = bisect(idx);
    vector<int> sorted_idx2(make_move_iterator(sorted_idx1.begin() + k1 * sorted_idx1.size() / k),
                    make_move_iterator(sorted_idx1.end()));
    sorted_idx1.erase(sorted_idx1.begin() + k1 * sorted_idx1.size() / k, sorted_idx1.end());
    
    #pragma omp task
	recursive_bisection(sorted_idx1, k1);
    #pragma omp task
    recursive_bisection(sorted_idx2, k2);
}

int main(int argc, char *argv[])
{
	for(size_t i = 0; i < N; ++i)
		for(size_t j = 0; j < M; ++j) {
			points_matrix[0][M * i + j] = i * STEP;
			points_matrix[1][M * i + j] = j * STEP;
		}

	vector<int> idx(M * N);
	iota(idx.begin(), idx.end(), 0);

    double start = omp_get_wtime();
    recursive_bisection(idx, atoi(argv[1]));
    cout<<omp_get_wtime() - start;
    
    /*for(size_t i = 0; i < graph_partition.size(); ++i) {
        for(size_t j = 0; j < graph_partition[i].size(); ++j)
            cout << graph_partition[i][j] << " ";
        cout << endl;
    }*/
}
EOF_CODE

    my $fname = "tmp.cpp";
    open(my $fh, '>', $fname);
    say $fh $code;

    qx(g++ -std=c++11 $fname -fopenmp -o task);

    my @a;
    for (1..$repeats) {
        my $t = qx(./task $h{k});
        push @a, $t;
    }
#    warn "@a\n";
    my $t = sum(@a) / $repeats;

    return $t;
}
