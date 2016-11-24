package SPolynomial;

use Moose;
use Moose::Util::TypeConstraints;
use MyTypes;

use feature 'say';
use Data::Printer;
use Data::Dumper;
use Math::Prime::Util qw(
    binomial 
    znprimroot 
    is_primitive_root 
    powmod 
    is_prime
);
use Storable 'dclone';
use Carp;

Moose::Exporter->setup_import_methods(
    as_is => [qw(show_polynomial generate)],
);

has 'k' => (
    is      => 'ro',
    isa     => 'Prime',
#    default => 5,
    required => 1,
);

has 'd' => (
    is      => 'ro',
    isa     => 'Uint',
    default => 0,
);

has 'str' => (
    is      => 'ro',
    isa     => 'Str',
#    required => 1,
);

has 'gen' => (
    is => 'rw',
    isa => 'Str',
);

has 'funcs' => (
    is  => 'ro',
    isa => 'ArrayRef[Str]',
    default => sub {['g', 'h']},
);

has 'polynomial' => (
    is       => 'rw',
#    init_arg => undef,
    isa      => 'ArrayRef[HashRef[Uint]]',
    lazy     => 1,
    builder  => '_build_polynomial',
); 


use overload '""' => \&_print;

sub _print {
    my $self = shift;
    show_polynomial(poly => $self, del => ' + ', noblank => 1);
}

sub to_csv {
    my $self = shift;
    show_polynomial(poly => $self, del => ';');
}

sub to_tex {
    my $self = shift;
    show_polynomial(poly => $self, del => '+', around => ['$','$']);
}

use overload '*=' => \&_overload_mul_eq, fallback => 1;

sub _overload_mul_eq {
#    say '*=';
    my ($self, $c) = @_;
#    croak "k must be equal in summands" unless $self->k == $other->k;
    my $polynomial = $self->polynomial;
    while (my ($i, $coeff)  = each @$polynomial) {
        for my $f (keys %$coeff) {
            ($polynomial->[$i]{$f} *= $c) %= $self->k;
        }
    }
    return $self;
}

use overload '*' => '_overload_mul';#, fallback => 1;
sub _overload_mul {
#    say '*';
    my ($c, $self) = @_;
    my $tmp = dclone $self;
    $tmp *= $c;
    return $tmp;
}

use overload '+=' => \&_overload_add_eq, fallback => 1;
sub _overload_add_eq {
#    say '+=';
    my ($self, $other) = @_;
    croak "k must be equal in summands" unless $self->k == $other->k;
    my $spolynomial = $self->polynomial;
    my $opolynomial = $other->polynomial;
    while (my ($i, $coeff)  = each @$opolynomial) {
        for my $f (keys %$coeff) {
            $spolynomial->[$i]{$f} += $coeff->{$f};
            $spolynomial->[$i]{$f} %= $self->k;
        }
    }
    return $self;
}

use overload '+' => '_overload_add';#, fallback => 1;
sub _overload_add {
#    say '+';
    my ($self, $other) = @_;
    my $tmp = dclone $self;
    $tmp += $other;
    return $tmp;
}

sub len {
    my $self = shift;
    my $sum = 0;

    for my $h (@{$self->polynomial}) {
        $sum += (grep {$h->{$_} > 0} keys %$h) > 0;
    }

    return $sum;
}

sub clone {
    my $self = shift;
    my $res = dclone $self;
    return $res;
}

sub mul {
    my ($self, $c) = @_;
    my $tmp = dclone $self;
    for my $coeff (@{$tmp->polynomial}) {
        for my $f (keys %$coeff) {
#            p $coeff;
#            say "$f $$coeff{$f}";
            $$coeff{$f} *= $c;
            $$coeff{$f} %= $tmp->k;
        }
    }
    
    return $tmp;
}

sub polarize {
    my ($self, $d) = @_;
    my $k = $self->k;
    $d %= $k;
    my $a = $self->polynomial;
    my $res;
    for (my $pow = 0; $pow < $k; ++$pow) {
        no warnings 'uninitialized';
        my $h = $a->[$pow];
        for my $f (@{$self->funcs}) {
            my $c = 0;
            for (my $i = $pow; $i < $k; ++$i) {
                $c += $a->[$i]->{$f} * binomial($i, $pow) *
                    powmod(-$d,$i-$pow,$k);
            }
            $c %= $k;
            $res->[$pow]->{$f} = $c;
        }
    }

    return SPolynomial->new(polynomial => $res, d => $d, k => $k);
}



sub _build_polynomial {
    my $self = shift;
    my $polynomial;

    if ($self->gen) {
        my @gen = split /;/, $self->gen;
        my $n = @gen;
        for my $x (@gen) {
            my ($c, $f) = split(/\./, $x);
            unshift @$polynomial, {$f => $c % $self->k};
        }

        for (my $i = $n; $i < $self->k; ++$i) {
#            p $polynomial;
            unshift @$polynomial, {%{$polynomial->[$n-1]}};
        }

    } else {
        my $s = $self->str;
        
        # powers
        $s =~ s/([a-z]+)$/$1*x^0/;
        $s =~ s/\)$/)*x^0/g;
        $s =~ s/x([^\^])/x^1$1/g;
        $s =~ s/x$/x^1/;

        # multiply
        $s =~ s/(\d+)\*([a-z]+)/$1.$2/g;
        $s =~ s/(\d+)([a-z]+)/$1.$2/g;
        $s =~ s/([^\*\.])([a-z]+)/${1}1.$2/g;
        $s =~ s/^([a-z]+)/1.$1/;
        
        # change inner + to Plus
        $s =~ s/\(([\w\d\.\*]+?)\s*\+\s*([\w\d\.\*]+?)\)/($1Plus$2)/g;
        
        # remove some elements to helping parsing   
        $s =~ s/\(|\)|\*x//g; 

        my @summands = map {s/Plus/+/r;} split(/\s*\+\s*/, $s);
        for my $s (@summands) {
            my ($coeff, $pow) = split(/\^/, $s);
            for my $cf (split(/\+/, $coeff)) {
                my ($c, $f) = split(/\./, $cf);
                $polynomial->[$pow]{$f} = $c;
            }
        }
    }

#    for my $coeff (@$polynomial) {
#        for my $f (@{$self->funcs}) {
#            $coeff->{$f} //= 0;
#        }
#    }

    return $polynomial;
}

__PACKAGE__->meta->make_immutable;
no Moose;

sub show_polynomial {
    my %args = (
        before  => '',
        after   => '',
        noblank => 0,
        default => '',
        around  => ['', ''],
        @_
    );

    for my $required (qw(poly del)) {
        croak "You must pass '$required' argument"
            unless defined $args{$required};
    }

    my ($k, $d) = ($args{poly}->k, $args{poly}->d);
    my @polynomial = @{$args{poly}->polynomial};
    my @res;
#    p @polynomial;

    while (my ($i,$s) = each @polynomial) {
        my @keys = sort grep {$s->{$_} > 0} keys %{$s};
        my $coeff = $args{default};
        my $blank = not @keys;

        unless ($blank) {
            $coeff = '';
            my @coeffs;
            for my $f (@keys) {
                if ($s->{$f} == 1) {
                    push @coeffs, $f;
                } else {
                    push @coeffs, "$s->{$f}*$f";
                }
            }
            
            $coeff = join(' + ', @coeffs);
            $coeff = "($coeff)" if @coeffs > 1;

            if ($i > 1) {
                if ($d > 0) {
                    $coeff .= "*(x+$d)^$i";
                } else {
                    $coeff .= "*x^$i";
                }
            } elsif ($i == 1) {
                if ($d > 0) {
                    $coeff .= "*(x+$d)";
                } else {
                    $coeff .= "*x";
                }
            }
        }
        
        if (not $blank or not $args{noblank}) {
          $coeff = $args{around}[0] . $coeff . $args{around}[1] unless $blank;
          unshift @res, $coeff; 
        }
    }

    $args{before} . join($args{del}, @res) . $args{after};
}

sub add_mul {
    my ($p1, $p2, $c) = @_;
    $c //= 1;

    return $p1 + $p2->mul($c);
}

sub generate {
    my %args = @_;
    unless (@_ == 6 and grep {defined $_} @args{qw(k gen type)} == 3) {
        croak "Need exactly 3 named arguments: k, gen, type";
    }

    my ($s, $t, $k) = @args{qw(gen type k)};
    croak "The value of type argument must be 1 or 2, you passed $args{type}"
        unless $t == 1 or $t == 2; 

    my $polynomial;
    my @gen = split /;/, $s;
    my @funcs;
    if ($t == 2) {
        my $n = @gen;
        for my $x (@gen) {
            my ($c, $f) = split(/\*/, $x);
            push @funcs, $f;
            unshift @$polynomial, {$f => $c % $k};
        }

        for (my $i = $n; $i < $k; ++$i) {
#            p $polynomial;
            unshift @$polynomial, {%{$polynomial->[$n-1]}};
        }
    } elsif ($t == 1) {
        my ($c1, $f1) = split(/\*/, $gen[0]);
        my ($c2, $f2) = split(/\*/, $gen[1]);
        push @funcs, $f1, $f2;
        unshift @$polynomial, {$f1 => $c1 % $k};

        for my $i (1..$k-1) {
            unshift @$polynomial, {$f2 => $c2 % $k};
        }
    }

#    for my $coeff (@$polynomial) {
#        for my $f (@funcs) {
#            $coeff->{$f} //= 0;
#        }
#    }

    return SPolynomial->new(k => $k, polynomial => $polynomial);
}

sub my_generate {
    my ($g, $h, $k) = @_;
    my @res;
    my $f0 = generate($g,1,$k);
    my $f1 = generate($h,1,$k);
    
    push @res, $f0, $f1;
#    say $f0;
#    say $f1;
    for my $c (1..$k-1) {
#        say add_mul($f0,$f1,$c);
        push @res, add_mul($f0,$f1,$c);
    }

    @res;
}

sub generate_all {
    my ($g, $h, $k) = @_;
    my @res;
    my $f0 = SPolynomial->new(k => $k, gen => $g);
    my $f1 = SPolynomial->new(k => $k, gen => $h);
    
    push @res, $f0, $f1;

    for my $c (1..$k-1) {
        push @res, add_mul($f0,$f1,$c);
    }

    @res;
}

