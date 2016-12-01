package Polynomial;

use feature 'say';
use List::Util qw(all);
use Data::Printer;
use Data::Dumper;
use Math::Prime::Util qw(
    binomial 
    znprimroot 
    is_primitive_root 
    powmod 
    is_prime
    invmod
);
use Storable 'dclone';
use Carp;

#use parent qw(Exporter);
#our @ISA = ("Exporter");
#our @EXPORT_OK = qw(binom_mod);

use Moose;
use Moose::Util::TypeConstraints;
use MyTypes;
use namespace::clean;

Moose::Exporter->setup_import_methods(
    as_is => [qw(binom_mod)],
);

has 'k' => (
    is      => 'ro',
    isa     => 'Prime',
    required => 1,
);

has 'd' => (
    is      => 'rw',
    isa     => 'Uint',
    default => 0,
);

has 'str' => (
    is      => 'bare',
    reader  => 'init_str',
    isa     => 'Str',
    required => 1,
);

has 'gen' => (
    is => 'rw',
    isa => 'Str',
);

has 'funcs' => (
    is      => 'ro',
    writer  => '_funcs',
    isa     => 'ArrayRef[Str]',
#    default => sub {['g', 'h']},
);

has 'polynomial' => (
    is       => 'rw',
#    init_arg => undef,
    isa      => 'ArrayRef[HashRef[Uint]]',
    lazy     => 1,
    builder  => '_build_polynomial',
); 

sub BUILD {
    my $self = shift;
    $self->polynomial; #build polynomial
}

use overload
    '""' => \&_print,
    '='  => 'clone',
    '*=' => '_overload_mul_eq',
    '*' => '_overload_mul',
    '+' => '_overload_add',
    '+=' => '_overload_add_eq', 
    fallback => 1;

sub _print {
    my $self = shift;
    $self->show(sep => ' + ', noblank => 1);
}

sub to_csv {
    my $self = shift;
    $self->show(sep => ';');
}

sub fprint {
    my $self = shift;
    my $k = $self->k;

    my %args = @_;

    my $c_sub = sub {
        $_ = $_[0];
        my $f = $_[1] // 'f';
        tr/()//d;
        my @a = split /\s*\+\s*/;
        if (@a > 2) {
            # Do nothing
            $_ = $_[0];
        } elsif (@a == 1) {
#            p $self->funcs;
#            p $self->polynomial;
            my $f0 = $self->funcs->[0];
            s/$f0/f0/g;
            my $f1 = $self->funcs->[1];
            s/$f1/f1/g;
        } else { # @a == 2
            $a[0] =~ s/^(\d)*//;
            my $c1 = $1 // 1;
            $a[1] =~ s/^(\d)*//;
            my $c2 = $1 // 1;
#            say "$c1 $c2";
            my $cf = "$f@{[invmod($c1, $k)*$c2 % $k + 1]}"; 
            $cf = "$c1*$cf" if $c1 > 0;
            $_ = $cf;
        }

        $_[0] = $_;
    };

    $self->show(c_sub => $c_sub, %args);
}

sub to_tex {
    my $self = shift;
    $self->show(sep => '+', around => '$');
}


sub _overload_mul_eq {
    my ($self, $c) = @_;
    my $polynomial = $self->polynomial;
    while (my ($i, $coeff)  = each @$polynomial) {
        for my $f (keys %$coeff) {
            ($polynomial->[$i]{$f} *= $c) %= $self->k;
        }
    }
    return $self;
}

sub _overload_mul {
    my ($self, $c) = @_;
    my $tmp = dclone $self;
    $tmp *= $c;
    return $tmp;
}

sub _overload_add_eq {
    my ($self, $other) = @_;
    croak "k must be equal in summands" unless $self->k == $other->k;
    my $spolynomial = $self->polynomial;
    my $opolynomial = $other->polynomial;
    $self->_funcs(_union($self->funcs, $other->funcs));
    while (my ($i, $coeff)  = each @$opolynomial) {
        for my $f (keys %$coeff) {
            $spolynomial->[$i]{$f} += $coeff->{$f};
            $spolynomial->[$i]{$f} %= $self->k;
        }
    }
    return $self;
}

sub _overload_add {
    my ($self, $other) = @_;
    my $tmp = dclone $self;
    $tmp += $other;
    return $tmp;
}

sub zeros {
    my $self = shift;
    my $res = [];

    while (my ($i, $x) = each @{$self->polynomial}) {
        push @$res, $i if all {$_ == 0} values %$x;
    }

    return $res;
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

sub polarize {
    my ($self, $d) = @_;
    my $k = $self->k;

    my $tmp = $self->clone;
    my $res = _polar($k, $self->polynomial, $d - $self->d, $self->funcs);
    $tmp->polynomial($res);
    $tmp->d($d);
    return  $tmp;
}



sub _build_polynomial {
    my $self = shift;
    my $summandst = resummand();
    my $function = $self->init_str;
    $function =~ tr/ //d;
    $function =~ s/- (?= [a-z] | \( )/-1/gx;
    die "Bad string" unless $function =~ /^ $summandst (\+ $summandst)*+ $/x;

    my ($k, $d) = ($self->k, $self->d);
    my $polar_polys = {};
    my %all_funcs;

    while ($function =~ /$summandst/g) {
        my %h = %+;
        $h{d} //= 0;
        $h{d} =~ s/\s//g;
        $h{d} += 0;
        $h{c} //= 1; 
        $h{c} =~ s/\s//g;
        $h{c} += 0;
        $h{pow} //= 1;
        $h{pow} = 0 unless defined $h{x};
        $h{funcs} //= 'Int';
        $h{funcs} =~ tr/*//d;
        delete $h{x};

        my $funcs = {};
        for (split /\+/, $h{funcs}) {
            my ($c, $f) = /(-?\d+)?(.+)/;
            $c //= 1;
            $funcs->{$f} = $c * $h{c} % $k;
            $all_funcs{$f} = 1;
        }

        for my $f (keys %$funcs) {
            no warnings 'uninitialized';
            ($polar_polys->{$h{d}}[$h{pow}]{$f} += $funcs->{$f}) %= $k;
        }
    }
    
    $self->_funcs([sort keys %all_funcs]);

#    p $self->funcs;
#    p %all_funcs;
#    p %$polar_polys;
    while (my ($i, $v) = each %$polar_polys) {
        $polar_polys->{$i} = _polar($k, $v, $d - $i, $self->funcs);
    }
#    p %$polar_polys;

    my $poly = _modulo_sum($k, values %$polar_polys);

    for my $coeff (@$poly) {
        $coeff->{Int} //= 0;
    }

    return $poly;
}

__PACKAGE__->meta->make_immutable;
#no Moose;
#1;

sub _union {
    my %union;
    for my $e (@{$_[0]}, @{$_[1]}) {$union{$e}++}

    return [sort keys %union];
}

sub _modulo_sum {
    my $k = shift;
    my $res = [];
    for my $v (@_) {
        while (my ($i, $coeff) = each @$v) {
            for my $f (keys %$coeff) {
                ($res->[$i]{$f} += $coeff->{$f}) %= $k;
            }
        }
    }

    return $res;
}

sub _polar {
    my ($k, $polynomial, $d, $funcs) = @_;
    $d %= $k;
    my $a = $polynomial;
    my $res;

    for (my $pow = 0; $pow < $k; ++$pow) {
        no warnings 'uninitialized';
        my $h = $a->[$pow];
        for my $f (@{$funcs}) {
            my $c = 0;
            for (my $i = $pow; $i < $k; ++$i) {
                my $binom = 0 + "@{[binomial($i, $pow) % $k]}";
#                say $binom;
#                my $binom = binomial($i, $pow);
                $c += $a->[$i]->{$f} * $binom * powmod(-$d, $i-$pow, $k);
            }
            $c %= $k;
#            say $c;
            $res->[$pow]->{$f} = $c;
        }
    }

    return $res;
}

sub resummand {
    my $number = qr/\-?\d++/;
    my $polar = qr/(?:-|\+)?\d++/;

    my $var = qr/(?<x>x)/;
    my $varst = qr{
            \($var(?<d>$polar)\)
        |
            \((?<d>$number)\+$var\)
        |
            $var
    }x;

    my $expst = qr/$varst\^(?<pow>$number)/;

    my $func = qr/[a-w]\d*+/x;
    my $c_func = qr/(?: $number \*? )? [a-w]\d*+/x;
    my $funcst = qr{
            (?<funcs> $c_func )
        |   
            \( (?<funcs> $c_func (?: \+ $c_func)*+ ) \) 
    }x;

    my $summand = qr/(?: $funcst \*? )? (?: $expst|$varst )/x;
    my $summandst = qr{
            (?: (?<c>$number) \*? )?? $summand 
        |
            (?<c>$number)?? $funcst
        |
            (?<c>$number)
    }x;

    return $summandst;
}

sub show {
    my $self = shift;
    my %args = (
        sep        => ' + ',
        tex        => 0,
        before     => '',
        mul        => '*',
        after      => '',
        noblank    => 0,
        c_sub      => sub {$_[0]},
        default    => '',
        only_funcs => 0,
        around     => ['', ''],
        @_
    );

    $args{around} = [$args{around}, $args{around}] unless ref($args{around});

    my ($k, $d) = ($self->k, $self->d);
    my @polynomial = @{$self->polynomial};
    my @res;
    my @funcs;

    while (my ($i,$s) = each @polynomial) {
        my @keys = sort grep {$s->{$_} > 0} keys %{$s};
        my $coeff = $args{default};
        my $blank = not @keys;

        unless ($blank) {
            $coeff = '';
            my @coeffs;
            for my $f (@keys) {
                if ($f eq 'Int') {
                    push @coeffs, $s->{$f};
                } elsif ($s->{$f} == 1) {
                    push @coeffs, $f;
                } else {
                    push @coeffs, "$s->{$f}$args{mul}$f";
                }
            }
            
            $coeff = join(' + ', @coeffs);
            $coeff = "($coeff)" if @coeffs > 1;

            $args{c_sub}->($coeff);
            if ($args{tex}) {
                $coeff =~ s/([a-w])(\d+)/$1_$2/g;
            }
            if ($args{only_funcs}) {
                $coeff =~ s/^\d+\*//;
                unshift @res, $coeff;
            }

            $coeff = '' if $coeff eq '1' and $i > 0;
            $coeff = $coeff . $args{mul} if $i > 0 and $coeff;

            if ($i > 1) {
                if ($d > 0) {
                    $coeff .= "(x+$d)^$i";
                } else {
                    $coeff .= "x^$i";
                }
            } elsif ($i == 1) {
                if ($d > 0) {
                    $coeff .= "(x+$d)";
                } else {
                    $coeff .= "x";
                }
            }
        }
        
        if ((not $blank or not $args{noblank}) and not $args{only_funcs}) {
          $coeff = $args{around}[0] . $coeff . $args{around}[1] unless $blank;
          unshift @res, $coeff; 
        }
    }

    $args{before} . join($args{sep}, @res) . $args{after};
}

sub prime_root {
    my $k = shift;
    my $res;
    ELEM: 
    for my $i (2..$k-1) {
        for my $e (2..$k-2) {
            next ELEM if $i ** $e  % $k == 1;
        }
        $res = $i;
        last ELEM;
    }

    return $res;
}

sub factmod {
    my ($n, $p) = @_;
	my $res = 1;
	while ($n > 1) {
		$res = ($res * (($n/$p) % 2 ? $p-1 : 1)) % $p;
		for (my $i=2; $i <= $n % $p; ++$i) {
			$res = ($res * $i) % $p;
        }
		$n /= $p;
	}

	return $res % $p;
}

sub binom_mod { 
    # C_n^r mod k
    my($n, $r, $k) = @_;
    croak "Bad arguments to binom_mod" unless
        defined $n && 
        $n =~ /^\d+$/ && 
        defined $r && 
        $r =~ /^\d+$/ && 
        defined $k && 
        $k =~ /^\d+$/;

        my $nf = factmod($n, $k);
        my $rf = factmod($r, $k);

        return $nf / ($rf * factmod($n-$r, $k));
}
