#!/bin/perl

# replaces "file://" with just "//" in <a href= tags

# $1: 'y' to add '.html' toall anchors, 'n' otherwise

# $2->: input files, edited in place. if '-' is given
# it will read from stdin and output to stdout

use strict;
use warnings;
use HTML::TokeParser::Simple;

my ($do_add_html, @file_list) = @ARGV;
if (not defined $do_add_html) {
  $do_add_html = "no";
}
@file_list = map { if ($_ eq '-') { *STDIN; } else { $_; }} @file_list;

sub remove_file_links {
  my ($link, $add_html) = @_;
  my $bare_link = $link =~ s/^file://r;
  if ($add_html =~ /^[yY]/ and not $link =~ /^#/) {
    $bare_link . ".html";
  } else {
    $bare_link;
  }
}

for my $file (@file_list) {
  my ($outfile, $parser);
  if ($file ne *STDIN) {
    open my $fh, "<", $file or die "could not open $file: $!";
    my $file_contents = "";
    while (<$fh>) {
      $file_contents = $file_contents . $_;
    }
    close $fh;
    open $outfile, ">", $file or die "could not open $file: $!";
    $parser = HTML::TokeParser::Simple->new(string => $file_contents);
  } else {
    $outfile = *STDOUT;
    $parser = HTML::TokeParser::Simple->new(*STDIN);
  }

  while ( my $token = $parser->get_token ) {
    if ($token->is_start_tag('a')) {
      print $outfile "<a";
      my $attrs = $token->get_attr();
      while (my ($k, $v) = each %{$attrs}) {
        print $outfile " $k=\"";
        if ($k eq 'href') {
          print $outfile remove_file_links($v, $do_add_html);
        } else {
          print $outfile "$v";
        }
        print $outfile "\"";
      }
      print $outfile ">";
    } else {
      print $outfile $token->as_is;
    }
  }

  close $outfile;
}
