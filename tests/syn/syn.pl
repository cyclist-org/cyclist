#!/usr/bin/perl

use strict;
use warnings;
use Getopt::Long;
use Math::Random qw(random_binomial random_poisson 
                    random_uniform random_uniform_integer);
use YAML::Tiny;
use String::Urandom;

# The following are the default parameters of the distribution,
# these can also be set as command line arguments, e.g.
#
#    ./syn.pl -preds 10 -vars 20

my %param = (
  preds   => 3,     # number of predicates
  vars    => 50,    # total number of variables
  arity   => 3,     # arity of each predicate
  cases   => 3,     # number of rules per predicate
  eqs     => 2,     # average number of equality literals ~ Poisson(n)
  neqs    => 2,     # average number of disequality literals ~ Poisson(n)
  points  => 2,     # average number of points-to literals ~ Poisson(n)
  recurs  => 2,     # average number of recursive predicates ~ Poisson(n)
  pformal => undef, # pick a formal argument with at least this probability
  seed    => undef, # random seed
  rules   => undef, # shorthand for preds-cases
);

GetOptions(map { ("$_=s" => \$param{$_}) } keys %param) or die "Fatal Error.\n";

($param{preds}, $param{cases}) = split '-', $param{rules} if defined $param{rules};
delete $param{rules};

$param{seed} = String::Urandom->new()->rand_string()
  unless defined $param{seed};

$param{pformal} = sprintf("%0.2f", 1/($param{points}+$param{recurs})) 
  unless defined $param{pformal};

print "# ", join(" ", map { "-$_ $param{$_}" } keys %param), "\n\n";

Math::Random::random_set_seed_from_phrase($param{seed});

$param{props} = [proportions(map { $param{$_} } qw/eqs neqs points recurs/)];

for my $i (1..$param{preds}) {
  my $pred = fun_app("p$i", map { "x$_" } (1..$param{arity}));
  print "p$i {\n";
  for my $j (1..$param{cases}) {
    print "  ", rand_body(), " => $pred";
    print " |" if $j < $param{cases};
    print "\n";
  }
  print "}";
  print " ;" if $i < $param{preds};
  print "\n\n";
}

# random formula generators

sub rand_body {
#  my @n = rand_with_props($param{len}, $param{props});
  my @n = map { random_poisson(1, $param{$_}) } qw/eqs neqs points recurs/;
  my @lit = (
    rand_eqs($n[0]),
    rand_neqs($n[1]),
    rand_points($n[2]),
    rand_preds(maybe(1/($param{cases}*$param{preds})) ? 0 : $n[3])
  );
  push @lit, "x1=x1" unless @lit;
  return join(" * ", @lit);
}

sub rand_eqs {
  my $n = shift;
  my @lit = (undef) x $n;
  for my $lit (@lit) {
    $lit = join("=", rand_terms(2, nil => 1, distinct => 1));
  }
  return @lit;
}

sub rand_neqs {
  my $n = shift;
  my @lit = (undef) x $n;
  for my $lit (@lit) {
    $lit = join("!=", rand_terms(2, nil => 1, distinct => 1));
  }
  return @lit;
}

sub rand_points {
  my $n = shift;
  my @lit = rand_terms($n, distinct => 1);
  my @y = rand_terms($n, nil => 1);
  for my $lit (@lit) {
    $lit = join("->", $lit, shift @y);
  }
  return @lit;
}

sub rand_preds {
  my $n = shift;
  my @lit = random_uniform_integer($n, 1, $param{preds});
  for my $lit (@lit) {
    $lit = fun_app("p$lit", rand_terms($param{arity}, nil => 1));
  }
  return @lit;
}

sub fun_app {
  my ($f, @a) = @_;
  return @a ? sprintf("$f(%s)", join(",", @a)) : $f;
}

sub rand_terms {
  my $n = shift;
  my %opt = @_;
  my %seen;
  my $min = $opt{nil} ? 0 : 1;
  my @t;
  while (@t < $n) {
    my $max = maybe($param{pformal}) ? $param{arity} : $param{vars};
    my $t = random_uniform_integer(1, $min, $max);
    $t = $t ? "x$t" : "nil";
    next if $seen{$t} && $opt{distinct};
    push @t, $t;
    $seen{$t} = 1;
  }
  return @t;
}

# special probability distributions

sub rand_with_props {
  my ($n, $prop) = @_;
  my @x = @$prop;
  pop @x;
  for my $x (@x) {
    $x = random_binomial(1, $n, $x);
    $n -= $x;
  }
  push @x, $n;
  return @x;
}

sub proportions {
  my @props = reverse @_;
  my $sum = 0;
  for my $p (@props) {
    $sum += $p;
    $p /= $sum if $p;
  }
  return reverse @props;
}

sub rand_props {
  my ($n, $props) = @_;
  return map { idx($_, $props) } random_uniform($n, 0, $props->[-1]);
}

sub idx {
  my ($x, $props) = @_;
  my $i = 0;
  while ($x > $props->[$i]) { $i++; }
  return $i;
}

sub accumulate {
  my ($props, $len) = @_;
  $len = 1 unless defined $len;
  my $sum = 0;
  for my $x (@$props) {
    $sum += $x;
    $x = $sum;
  }
  $sum = $len/$sum;
  for my $x (@$props) {
    $x *= $sum;
  }
  return $props;
}

sub rand_small {
  my ($k, $n) = @_;
  my $m = int($n*($n+1)/2);
  return map { small_idx($_, $n) } random_uniform_integer($k, 0, $m - 1);
}

sub small_idx {
  my ($x, $n) = @_;
  my $i = $n;
  while ($x >= $i) { $x -= $i; $i--; }
  return $n - $i;
}

sub maybe {
  my $p = shift;
  return random_uniform(1) <= $p;
}

## general purpose ##

sub uniq {
  return keys %{{ map { $_ => 1 } @_ }};
}
