#!/usr/bin/perl

use strict;
use warnings;
use YAML::Tiny;
use Getopt::Long qw(:config posix_default);
use Math::Random qw(random_uniform_integer);
use Time::HiRes ();

Math::Random::random_set_seed_from_phrase(join(':', 'POPL-2014', @ARGV));
print join(' ', '#', $0, @ARGV), "\n";

my $cmd = "sl_satcheck";
my $file = "test.defs";
my $samples = 100;
my $timeout = 60;

GetOptions(
  "file=s"     => \$file,
  "samples=i"  => \$samples,
  "timeout=i"  => \$timeout,
) or die "Fatal Error.\n";

$| = 1;

for (1..$samples) {
  my $seed = rand_string(32);
  system sprintf("./syn.pl %s > $file", join(" ", "-seed", $seed, @ARGV)) and die;
  print join("\t", $seed, check($file)), "\n";
}

# The checker enforces the timeout itself, so no external time/ulimit wrapper is
# needed; elapsed time is taken here and includes dune's build revalidation.
sub check {
  my $file = shift;
  my $start = Time::HiRes::time();
  my $output = `dune exec $cmd -- -t $timeout -D $file 2>&1`;
  my $elapsed = sprintf("%.2f", Time::HiRes::time() - $start);
  die $output if $output =~ m/^Fatal error/m;
  my $result;
  if ($output =~ m/^UNSAT:/m) {
    $result = 'unsat';
  } elsif ($output =~ m/^SAT:/m) {
    $result = 'sat';
  } elsif ($output =~ m/^UNKNOWN:.*TIMEOUT/m) {
    $result = 'timeout';
  } else {
    die "Unexpected result:\n$output\n";
  }
  return ($result, $elapsed);
}

sub rand_string {
  my $n = shift;
  # 62 = 26 lower case + 26 upper case + 10 digits 
  return join("", map { ind_chr($_) } random_uniform_integer($n, 0, 61));
}

sub ind_chr {
  my $x = shift;
  return $x < 26 ? chr($x + ord('A'))
       : $x < 52 ? chr($x + ord('a') - 26)
       :           chr($x + ord('0') - 52);  
}


