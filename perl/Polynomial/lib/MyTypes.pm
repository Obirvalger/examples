package MyTypes;

use Moose::Util::TypeConstraints;
use Math::Prime::Util qw(is_prime);

subtype 'Uint',
    as 'Int',
    where { $_ >= 0 },
    message { "The number you provided, $_, was not a nonnegative number" };

subtype 'Prime',
    as 'Int',
    where { is_prime($_) },
    message { "The number you provided, $_, was not a prime number" };

subtype 'PolynomialClass',
    as 'ClassName',
    where { $_ eq 'SPolynomial' or $_ eq 'VPolynomial'},
    message { "You must provide SPolynomial or VPolynomial" };

1;
