#! /usr/bin/perl

use strict;
use warnings;
use feature qw(say);
use Data::Dumper;

my $h = do 'data.pl';
my $hnew;

for my $thr (sort keys %$h) {
    for my $k (sort {$a <=> $b} keys %{$h->{$thr}}) {
        for my $mn (sort {$a <=> $b} keys %{$h->{$thr}{$k}}) {
            $hnew->{$mn}{$thr}{$k} = $h->{$thr}{$k}{$mn};
        }
    }
}

print Dumper($hnew);
