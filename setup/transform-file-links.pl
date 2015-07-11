#!/bin/perl

# replace "file://" with just "//" in <a href="" tags
# $1: 'y' to add '.html' toall anchors, 'n' otherwise
# $2->: input filename. used to avoid putting .html in links to the file's self

use strict;
use warnings;
use HTML::TokeParser::Simple;
use File::Basename;
use File::Spec::Functions;

my ($do_add_html, $file_name) = @ARGV;
my $file_name_exists = (scalar @ARGV eq 2);
my $dir_name;
if ($file_name_exists) {
  $dir_name = dirname($file_name);
  $file_name = basename($file_name);
}

sub remove_file_links {
  my ($link, $add_html) = @_;
  my $bare_link = $link;
  if ($link =~ /^file:/) {
    $bare_link = $link =~ s/^file://r;
  } elsif ($link !~ /^#/ and $link !~ /^([a-zA-Z0-9_]+:)?\/\//
           and $link !~ /\.[a-zA-Z0-9_]+$/) {
    # remove links to other parts of the org file in htmlized versions
    # extremely hacky, but works 99% of the time
    if (not -e catfile($dir_name, $link)) {
      return '#';
    }
  }
  # if this is a link to the file itself, don't append .html
  my $fileregex = quotemeta($file_name);
  if ($add_html =~ /^[yY]/ and $bare_link !~ /^#/ and $bare_link !~ /\.html$/
      and ((not $file_name_exists) or ($bare_link !~ /$fileregex$/)) and
      # don't modify http://, etc links, or plain old // links
      $bare_link !~ /^([a-zA-Z0-9]+:)?\/\//){
    return $bare_link . ".html";
  }
  return $bare_link;
}

my $parser = HTML::TokeParser::Simple->new(*STDIN);

while ( my $token = $parser->get_token ) {
  if ($token->is_start_tag('a')) {
    print "<a";
    my $attrs = $token->get_attr();
    while (my ($k, $v) = each %{$attrs}) {
      print " $k=\"";
      if ($k eq 'href') {
        print remove_file_links($v, $do_add_html);
      } else {
        print "$v";
      }
      print "\"";
    }
    print ">";
  } elsif ($token->is_start_tag('pre')) {
    print "<pre";
    my $attrs = $token->get_attr();
    my $code_classes;
    while (my ($k, $v) = each %{$attrs}) {
      print " $k=\"";
      if ($k eq 'class') {
        $code_classes = $v =~ s/src\-([^\s]+)/src\-$1 $1/rg;
        print $code_classes;
      } else { print "$v"; }
      print "\"";
    }
    print "><code";
    if ($code_classes) {
      print " class=\"$code_classes\"";
    }
    print ">"
  } elsif ($token->is_end_tag('pre')) {
    print "</code>";
    print $token->as_is;
  } else {
    print $token->as_is;
  }
}
