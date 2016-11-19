#! /usr/bin/perl

use Memoize;
use bigint;
use feature 'say';

$m = $ARGV[0];
$m //= 35;

sub fib {
	  my $n = shift;
	  return $n if $n < 2;
	  fib($n-1) + fib($n-2);
}

memoize('fib');

say fib($m);
