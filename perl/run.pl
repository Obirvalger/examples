#! /usr/bin/perl

use 5.014;
use warnings;
use File::Basename;

my $full = shift;
my ($filename, $dirs, $s) = fileparse($full, qw/.pl .py .c .cpp .hs/);

system("perl $full @ARGV") if ($s eq '.pl') ;
system("python $full @ARGV") if ($s eq '.py') ;
system("./$full @ARGV") if ($s eq '.c') ;
system("./$full @ARGV") if ($s eq '.cpp') ;
system("./$full @ARGV") if ($s eq '.hs') ;
