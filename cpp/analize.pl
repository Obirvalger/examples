#! /usr/bin/perl

use strict;
use warnings;
use feature qw(say);

my $h = do 'data.pl';
for my $mn (sort keys %$h) {
    open (my $fh, '>', "data_${mn}.dat"); 
    for my $thr (sort {$a <=> $b} keys %{$h->{$mn}}) {
        for my $k (sort {$a <=> $b} keys %{$h->{$mn}{$thr}}) {
            say $fh "$thr $k " . $h->{$mn}{$thr}{$k};
        }
    }
}
