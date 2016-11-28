use strict;
use warnings;
use Math::Prime::Util qw(znprimroot);
use Test::More tests => 2;

use_ok('Polynomial', qw());
can_ok('Polynomial', qw(new polarize mul clone len));

