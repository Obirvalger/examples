package AOP;

use Moose;
use Moose::Util::TypeConstraints;
use MyTypes;
use SPolynomial;
use VPolynomial;

use feature 'say';
use Data::Printer;
use Data::Dumper;
use Math::Prime::Util qw(binomial znprimroot is_primitive_root powmod);
use Storable 'dclone';
use Carp;

has 't' => (
    is  => 'ro',
    isa => 'PolynomialClass',
); 

has 'k' => (
    is      => 'rw',
    isa     => 'Prime',
#    default => 5,
    required => 1,
);

has 'polynomials' => (
    is       => 'ro',
    writer   => '_polynomials',
    isa      => 'ArrayRef[VPolynomial | SPolynomial]',
    init_arg => undef,
    lazy     => 1,
    builder  => '_build_polynomials',
);

sub _build_polynomials {
    ...
}

__PACKAGE__->meta->make_immutable;
no Moose;
