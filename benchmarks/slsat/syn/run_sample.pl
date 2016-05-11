#!/usr/bin/perl

use strict;
use warnings;
use YAML::Tiny;
use Getopt::Long qw(:config posix_default);
use Math::Random qw(random_uniform_integer);

Math::Random::random_set_seed_from_phrase(join(':', 'POPL-2014', @ARGV));
print join(' ', '#', $0, @ARGV), "\n";

my $file = "test.defs";
my $clock = "clock.tmp";
my $time = "/usr/bin/time --quiet -f %e -o $clock";
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

sub check {
  my $file = shift;
  my $output = `ulimit -t $timeout; $time ../../slsat_check.native -D $file 2>&1`;
  die $output if $output =~ m/error/i;
  my @clock = `cat $clock`;
  chomp @clock;
  my $result;
  if (@clock > 1) {
    $result = shift @clock;
    $result = 'timeout' if $result =~ m/signal 9$/;
  } elsif ($output =~ m/NOT proved:.+empty base/) {
    $result = 'unsat';
  } elsif ($output =~ m/NOT proved:.+TIMEOUT/) {
    $result = 'timeout';
  } elsif ($output =~ m/Proved:.+non-empty bases/) {
    $result = 'sat';
  } else {
    die "Unexpected result:\n$output\n";
  }
  return ($result, $clock[-1]);
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


