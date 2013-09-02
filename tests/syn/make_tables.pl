#!/usr/bin/perl

use strict;
use warnings;
use YAML::Tiny;
use Getopt::Long;

my $latex = 0;

GetOptions(
  "latex" => \$latex,
);


my @header = qw(value easy solved sat);
make_table("a", "vars", qw(20 30 40 50 60 70 80));
make_table("b", "rules", qw(2-2 3-2 2-3 3-3 4-3 3-4 4-4));
make_table("c", "arity", qw(1 2 3 4 5));
make_table("d", "recs", qw(0 1 2 3 4));

sub make_table {
  my ($label, $param, @value) = @_;
  $header[0] = $param;
  print $latex ? '\begin{tabular}{@{}rrrr@{}} \toprule'
               : "## $param ##", "\n";
  print $latex ? "\\tableheader{$label}{\u$param}"
               : join("\t", @header), "\n";
  my @files = get_files($param, @value);
  for my $i (0..$#files) {
    my ($value, $file) = @{ $files[$i] };
    my $stats = grab_stats($file);
    $value =~ s/-/\$\\times\$/ if $latex;
    $stats->{$param} = $value;
    my @cell = map { $stats->{$_} } @header;
    @cell = map { "\\bf $_" } @cell if $latex && $i == $#files / 2;
    print join($latex ? " & " : "\t", @cell);
    print " \\\\" if $latex && $i < $#files;
    print "\n";
  }
  print "\\end{tabular}\n" if $latex;
  print "\n";
}

sub get_files {
  my ($param, @value) = @_;
  my @file = map { [$_, "sample-$param-$_.dat"] } @value;
  $file[$#file/2]->[1] = "sample-central.dat";
  return @file;
}

sub grab_stats {
  my $file = shift;
  my @t;
  my %stats = (map { $_ => 0 } qw/sat unsat timeout/);
  open FILE, "<", $file or die "Can\'t open: $file\n";
  while (<FILE>) {
    next if m/^#/;
    chomp;
    my @x = split ' ';
    $stats{$x[1]}++;
    push @t, $x[1] eq 'timeout' ? '100' : $x[2];
    $stats{easy}++ if $x[2] <= 1;
  }
  close FILE;
  @t = sort { $a <=> $b } @t;
  $stats{solved} = $stats{sat} + $stats{unsat};
  $stats{"90th"} = $t[-10];
  $stats{max} = $t[-1];
  return \%stats;
}
