#!/usr/bin/perl
#
# 6502 Assembler in Perl
# 
# Based upon "Assembler for the PET" by Mark Zimmermann, Personal Computing, Dec 1978, pp42-45
#
# NOTES
#
# - Relative jumps take an address, rather than a displacemnt - similar to using labels.
#   - Maybe use an `@` prefix for addresses, and without for offset? IS there precedence?
#

#
# TODO
#
# Add decimal output (as well as hex)
# Subs
# Testfile - DONE!
# Print mnemonic on same line as hex - DONE!
# Convert all numbers to decimal - if two operand bytes are possible. 
# ... We can leave the mess for just one operand byte (prior to line 251)
# ... strip out the now redundant hex stuff
#
# Labels: END; ORG; DB; DC; DW; - DONE!
# Variables: search for `=` and store in hash, to reuse - DONE!
# Comments: ignore anything after `;`
# Labels: memory locations - if /^\w+\s/ and !exists($categories{$1}), then it is a label (or an error, i.e. bad mnemonic) - DONE!
#
# See for example:
# - https://atariwiki.org/wiki/Wiki.jsp?page=Advanced%206502%20Assembly%20Code%20Examples
#
# Multiple files? `foreach my $file (@ARGV)`, from https://www.perlmonks.org/?node_id=65453
#
# Print timings - DONE!
#
# Print branch alternative timings as well
#
# decimal
#
# add option for (non-label input) labels, so that stdin can be used.
#

#
# Use
#

use strict;
use warnings;
use Getopt::Std;

#
# Flags
#

my $flg_hex = 0;
my $flg_dec = 0;
my $flg_quiet = 0;
my $flg_upper = 0;
my $flg_debug = 0;
my $flg_no_labels = 0;

#
# Prefixes/Suffixes
#

my $prefix_warning = "[warn!:] ";
my $suffix_warning = $prefix_warning." Line:   ";
my $prefix_debug = "[debug:] ";
my $suffix_debug = "\n";


#
# Hashes
#

# Use hash(es) instead of DATA to look up mnemonic

my %categories = ( acd => 1,
	           and => 1, 
    	           asl => 3, 
    	           bcc => 8, 
    	           bcs => 8, 
    	           beq => 8, 
    	           bit => 7, 
    	           bmi => 8, 
    	           bne => 8, 
    	           bpl => 8, 
    	           brk => 0, 
    	           bvc => 8, 
    	           bvs => 8, 
    	           clc => 0, 
    	           cld => 0, 
    	           cli => 0, 
    	           clv => 0, 
    	           cmp => 1, 
    	           cpx => 4, 
    	           cpy => 4, 
    	           dec => 2, 
    	           dex => 0, 
    	           dey => 0, 
    	           eor => 1, 
    	           inc => 2, 
    	           inx => 0, 
    	           iny => 0, 
    	           jmp => 6, 
    	           jsr => 9, 
    	           lda => 1, 
    	           ldx => 5, 
    	           ldy => 5, 
    	           lsr => 3, 
    	           nop => 0, 
    	           ora => 1, 
    	           pha => 0, 
    	           php => 0, 
    	           pla => 0, 
    	           plp => 0, 
    	           rol => 3, 
    	           ror => 3, 
    	           rti => 0, 
    	           rts => 0, 
    	           sbc => 1, 
    	           sec => 0, 
    	           sed => 0, 
    	           sei => 0, 
    	           sta => 1, 
    	           stx => 2, 
    	           sty => 2, 
    	           tax => 0, 
    	           tay => 0, 
    	           tsx => 0, 
    	           txa => 0, 
    	           txs => 0, 
	           tya => 0);

# Note that the base op-codes for category 3 have been reduced by 4 (for "logic" reasons)
my %baseOpCode = ( acd => 97,     # +8,+4,+20,+12,+28,+24,0,+16
	           and => 33, 
    	           asl => 2,      # 4,0,16,8,24
    	           bcc => 144, 
    	           bcs => 176, 
    	           beq => 240, 
    	           bit => 36,     # 0,8
    	           bmi => 48, 
    	           bne => 208, 
    	           bpl => 16, 
    	           brk => 0, 
    	           bvc => 80, 
    	           bvs => 112, 
    	           clc => 24, 
    	           cld => 216, 
    	           cli => 88, 
    	           clv => 184, 
    	           cmp => 193, 
    	           cpx => 224,     # 0,4,12
    	           cpy => 192, 
    	           dec => 198, 
    	           dex => 202, 
    	           dey => 136, 
    	           eor => 65, 
    	           inc => 230, 
    	           inx => 232, 
    	           iny => 200, 
    	           jmp => 76,      # 0,32
    	           jsr => 32, 
    	           lda => 161, 
    	           ldx => 162,     # 0,4,20,12,28
    	           ldy => 160, 
    	           lsr => 66,      # 4,0,16,8,24
    	           nop => 234, 
    	           ora => 1, 
    	           pha => 72, 
    	           php => 8, 
    	           pla => 104, 
    	           plp => 40, 
    	           rol => 34, 
    	           ror => 98, 
    	           rti => 64, 
    	           rts => 96, 
    	           sbc => 225, 
    	           sec => 56, 
    	           sed => 248, 
    	           sei => 120, 
    	           sta => 129, 
    	           stx => 134,     # 0,16,8
    	           sty => 132, 
    	           tax => 170, 
    	           tay => 168, 
    	           tsx => 186, 
    	           txa => 138, 
    	           txs => 154, 
	           tya => 152);

#
# Not the lowest #cycles, but the #cycles of base opcode (indirect,X) or zeropage, or ...
# ... depending on category
#
# Offsets can be negative, order as follows:
#
# Category 1:
# immediate,zeropage,zeropage,X,absolute, absolute,X,absolute,Y,(indirect,X),(indirect,Y) 
#
# Category 3:
# accumulator, zeropage, zeropage,X, absolute, absolute,X
#
# Category 7:
# zeropage, absolute
#
# Category 4:
# immediate, zeropage, absolute
#
# Category 2:
# zeropage, zeropage,X, absolute, absolute,X
#
# Category 6:
# absolute, indirect
#
# Category 5:
# immediate, zeropage, zeropage,X, absolute, absolute,X
#
# Category 1a:
# zeropage,zeropage,X,absolute, absolute,X, absolute,Y, (indirect,X), (indirect,Y)
#
# Category 2:
# zeropage, zeropage,Y, absolute
#
# Categories 0, 8 and 9 have no offsets
#
# Notes:
# *  add 1 to cycles if page boundary is crossed
# ** add 1 to cycles if branch occurs on same page
#    add 2 to cycles if branch occurs to different page
#

my %cycles = (     acd => 6,     # -4,-3,-2,-2,-2*,-2*,0,-1*
	           and => 6,     # -4,-3,-2,-2,-2*,-2*,0,-1*
    	           asl => 5,     # -3,0,+1,+1,+2
    	           bcc => 2,     # **
    	           bcs => 2,     # ** 
    	           beq => 2,     # ** 
    	           bit => 3,     # 0,+1
    	           bmi => 2,     # ** 
    	           bne => 2,     # ** 
    	           bpl => 2,     # ** 
    	           brk => 7, 
    	           bvc => 2,     # ** 
    	           bvs => 2,     # ** 
    	           clc => 2, 
    	           cld => 2, 
    	           cli => 2, 
    	           clv => 2, 
    	           cmp => 6,     # -4,-3,-2,-2,-2*,-2*,0,-1*
    	           cpx => 2,     # 0,+1,+2 
    	           cpy => 2,     # 0,+1,+2 
    	           dec => 5,     # 0,+1,+1,+2 
    	           dex => 2, 
    	           dey => 2, 
    	           eor => 6,     # -4,-3,-2,-2,-2*,-2*,0,-1* 
    	           inc => 5,     # 0,+1,+1,+2 
    	           inx => 2, 
    	           iny => 2, 
    	           jmp => 3,     # 0,+2 
    	           jsr => 6, 
    	           lda => 6,     # -4,-3,-2,-2,-2*,-2*,0,-1* 
    	           ldx => 2,     # 0,+1,+2,+2,+2* 
    	           ldy => 2,     # 0,+1,+2,+2,+2* 
    	           lsr => 5,     # -3,0,+1,+1,+2 
    	           nop => 2, 
    	           ora => 6,     # -4,-3,-2,-2,-2*,-2*,0,-1* 
    	           pha => 3, 
    	           php => 3, 
    	           pla => 4, 
    	           plp => 4, 
    	           rol => 5,     # -3,0,+1,+1,+2 
    	           ror => 5,     # -3,0,+1,+1,+2 
    	           rti => 6, 
    	           rts => 6, 
    	           sbc => 6,     # -4,-3,-2,-2,-2*,-2*,0,-1* 
    	           sec => 2, 
    	           sed => 2, 
    	           sei => 2, 
    	           sta => 6,     # -3,-2,-2,-1,-1,0,0 
    	           stx => 3,     # 0,+1,+1
    	           sty => 3,     # 0,+1,+1 
    	           tax => 2, 
    	           tay => 2, 
    	           tsx => 2, 
    	           txa => 2, 
    	           txs => 2, 
	           tya => 2);

#
# Group the offsets to centralise any possible errors?
#

my @opcode_offsets = (
                       [0],                               # 0
                       [+8,+4,+20,+12,+28,+24,0,+16],     # 1
                       [0,16,8],                          # 2
                       [4,0,16,8,24],                     # 3
                       [8,4,20,12,28],                    # 3
                       [0,4,12],                          # 4
                       [0,4,20,12,28],                    # 5
                       [0,32],                            # 6
                       [0,8],                             # 7
                       [0],                               # 8
                       [0],                               # 9
                     );

my @timing_offsets = ( 
                       [0],                               # 0
                       #[-4,-3,-2,-2,-2*,-2*,0,-1*],      # 1
                       [-4,-3,-2,-2,-2,-2,0,-1],          # 1
                       [0,+1,+1],                         # 2
                       [-3,0,+1,+1,+2],                   # 3
                       [0,+1,+2],                         # 4
                       #[0,+1,+2,+2,+2*],                 # 5
                       [0,+1,+2,+2,+2],                   # 5
                       [0,+2],                            # 6
                       [0,+1],                            # 7
                       [0],                               # 8
                       [0],                               # 9
                     );

my %variables;
my %labels;

#
# Globals
#

my $line;
my $newline;

my $num_bytes=0;
my $opcode;
my $operand_byte;
my $operand_byte_two;
my $category;
my $timing;
my $label = "";
my $address=826;    # 826 - 1017 for the Commodore PET

our ($opt_a, $opt_d, $opt_h, $opt_l, $opt_n, $opt_q, $opt_u, $opt_w, $opt_x);
getopts('adhlnquwx');

if ($opt_h){
  print "\nHelp:\n";
  print "        prog.pl [adhlnquwx] [<filename>]\n\n";
  print "             a       - address              (Default: 826/\$033A)\n";
#  print "             d       - decimal output       (0-255)\n";
  print "             d       - debug\n";
  print "             h       - help\n";
  print "             l       - lowercase hex output (00-ff)\n";
  print "             n       - no labels used";
  print "             q       - quiet\n";
  print "             u       - uppercase hex output (00-FF)\n";
  print "             w       - suppress warnings\n";
  print "             x       - hex output           (00-FF)\n\n";
  exit;
}

$flg_debug=$opt_d                if $opt_d;
#$flg_no_warn=$opt_w         if $opt_w;
$flg_no_labels=$opt_n        if $opt_n;
$flg_hex=$opt_x              if $opt_x;
#$flg_dec=$opt_d             if $opt_d;
$flg_quiet=$opt_q            if $opt_q;
$flg_upper=$opt_u            if $opt_u;
$flg_upper=!$opt_l           if $opt_l;
$address=$opt_a              if $opt_a;
if ($opt_u && $opt_l){die "Can't both upper and lower!";}


#
# Subroutines
#

# Check if number is actually a pre-defined variable
# Use: $number = check4variable($number);
# Returns: variable value, or the original string
sub check4variable{
    die "Too many arguments for subroutine" unless @_ <= 1;
    die "Too few arguments for subroutine" unless @_ >= 1;
    my $variable = $_[0];
    my $variable_value = $variable;       # pass-thru number if variable not exist
    if (exists $variables{$variable}){
      $variable_value = $variables{$variable};
    }
    return $variable_value;
}

# Find labels using rewind

sub findlabelswithrewind{
  while (<>){
    my $line=$_;
    if ($line =~ /^(\w+)\s/){
      my $posslabel = $1;
      addlabel($posslabel);
    }
  }
  seek ARGV, 0, 0;
}

sub findlabelswithslurp{

  my @lines = <>;

  for ( @lines ){
    # operation one
    my $line=$_;
    # we need to first set the address and then
    # detect the number of bytes AND increase the address.
    # A total nightmare as it requires the mnemonic decoding
    # It MUST be two passes in the main loop - a pre-read (like this) is not sufficient
    # Note it can not be done afterwards, using '???' for unknown labels, until they are come across, and then finish off the 'forward referenced' labels at ther end of parsing, as the mnemonics and operands have already been coded by the end. This is why the labels must be found before strting the Zimmermann engine.
    # A cut down Zimmerman engine would be required to parse the code to find the number of bytes is used for each line, in order to increment the address. 
    if ($line =~ /^(\w+)\s/){
      my $posslabel = $1;
      addlabel($posslabel);
    }
  }

  for ( @lines ){
    # operation two
  }
}

# From https://stackoverflow.com/a/30086865
sub findlabelswithlocal{
  local @ARGV = @ARGV;
  while (<>){
    my $line = $_;
    my $original_line_length = length($line);
    chomp $line;
    print "Pass 1: $line";

    # No assembly or coding, just the mnenonic detection, instruction type and num_bytes
    # Start

    if ($line =~ /end/i) {                                    # END
      $num_bytes = 0;
      last;
    } elsif ($line =~ /(\w*) = (\$?\w+)/i) {                  # Variables
      $num_bytes=0;   # not needed?
      $variables{$1} = $2;
      #$num_bytes=0;   # not needed?
    } elsif ($line =~ /d[bc]\s*(\$?\w+)/i) {                  # DC/DB
      $num_bytes = 1;
    } elsif ($line =~ /dw\s*(\$?\w+)/i) {                     # DW
      $num_bytes = 2;
    } elsif ($line =~ /org\s*(\$?\w+)/i) {                    # ORG
      $num_bytes=0;   # not needed?
      $address=$1;
      if ($address =~ s/^\$(\w+)/$1/){
        $address = hex $address;
      }
    } elsif($line =~ /(.+)\s+(.+)/ || $line =~ /(.+)/){       # A real line of assembly code
      my $mnemonic = lc $1;
      my $operand = $2;
      if (exists $categories{$mnemonic}) {
        $category = $categories{$mnemonic};
      } else {
        if ($line!~/^(\w+)\s(.*)/){
          die "Unknown mnemonic!";
        } else {                                              # check for LABEL here
          addlabel($1);
          print $prefix_debug."Label found: $1\n" if $flg_debug;
          $line=~s/^(\w+)\s(.*)/$2/;                          # Strip out label 
          if($line =~ /(.+)\s+(.+)/ || $line =~ /(.+)/){      # and run it again
            $mnemonic = lc $1;
            $operand = $2;
            if (exists $categories{$mnemonic}) {
              $category = $categories{$mnemonic};
            } else {
              die "Unknown mnemonic!";
            }
          }
        }
      }
      print $prefix_debug."category $category$suffix_debug" if $flg_debug;

      $address += $num_bytes;
      #my $tmp_space = (" " x (20 - length($line)));
      my $tmp_space = (" " x (20 - $original_line_length));
      print $tmp_space." @ ";
      print_address($address);

      #
      # Zimmermann engine (truncated) - num_bytes only (start)
      #

      if ($category == 0) {
        $num_bytes = 1;
        # (code removed)
      } elsif ($operand =~ /^[Aa]$/) {
        if ($category == 3) {
          die "No! Can not be CATEGORY 3 FOR ACCUMULATOR OPERAND!";
        } else {
          $num_bytes = 1;
          # (code removed)
        }
        # (code removed)
      } elsif ( $operand =~ /^#(.+)/) {
        # 221 REM HANDLE "IMMEDIATE" INSTRUCTIONS HERE
        $num_bytes = 2;
        # (code removed)
      } elsif ( $operand =~ /^\((.+)/) {
        # 231 REM CHECK FOR VARIOUS INDIRECT INSTRUCTIONS
        if ( $operand =~ /(.+)\),Y$/ ) {
        # 233 REM IT IS AN "(INDIRECT),Y"
          $num_bytes = 2;
          # (code removed)
        } elsif ( $operand =~ /(.+),X\)$/ ) {
          $num_bytes = 2;
          # (code removed)
        } elsif ( $operand =~ /(.+)\)$/ ) {
          # 251 REM IT BETTER BE A JMP (INDIRECT), ELSE ERROR
          if ($category != 6){
            die "No! Not a JMP (INDIRECT)";
          } else {
            # 254 N=VAL(MID$(C$,2,L-2)):HI=INT(N/256):BY=3
            $num_bytes = 3;
            # (code removed)
          }
        } else {
            die "No! Not ')'";
        }
      } elsif ( $operand =~ /(.+),X$/ ) {
        # 260 IF RIGHT$(C$,2)<>",X" GOTO 280
        # TODO
        my $number = $1;
        $number = check4variable($number);
        #if ($number =~ /^\$(.+)/){
        if ($number =~ s/^\$(.+)/$1/){
          $number = hex $number;  # convert to decimal
                                  # this makes the hex stuff below redundant
        }
        if ($number < 256){
          # 265 REM HANDLE "ZERO PAGE,X" HERE
          $num_bytes = 2;
          # (code removed)
        } else {
          $num_bytes = 3;
          # (code removed)
        }
      } elsif ( $operand =~ /(.+),Y$/ ) {
        # 280 IF RIGHT$(C$,2)<>",Y" GOTO 300 
        my $number = $1;
        $number = check4variable($number);
        #if ($number =~ /^\$(.+)/){
        if ($number =~ s/^\$(.+)/$1/){
          $number = hex $number;  # convert to decimal
                                  # this makes the hex stuff below redundant
        }
        if ($number < 256){
          # 285 REM HANDLE ZERO PAGE,Y HERE
          $num_bytes = 2;
          # (code removed)
        } else {
          $num_bytes = 3;
          # (code removed)
        }
      } else {
        # 300 N=VAL(C$):REM NOW, FOR NUMERICAL OPERANDS
        my $number = $operand;  # Not really needed??? I think it is.
        $number = check4variable($number);
        #if ($number =~ /^\$(.+)/){
        if ($number =~ s/^\$(.+)/$1/){
          $number = hex $number;  # convert to decimal
                                  # this makes the hex stuff below redundant
        }
        if ($category != 8) {
          if ($number < 256){
            $num_bytes = 2;
            # (code removed)
          } else {
            # 330 HI=INT(N/256):POKE 999,HI:POKE998,N-256*HI:BY=3
            $num_bytes = 3;
            # (code removed)
          }
        } else {
          # 340 N=N-AD-2:IF N<-128 OR N>127 THEN PRINT "CAN'T BRANCH";N:GOTO 100
          # Do branches
          $number = $number - $address - 2;
          print $prefix_debug."number: $number address: $address$suffix_debug" if $flg_debug;

          if ($number < -128 || $number > 127) {
            die "No! CAN'T BRANCH $number!";                   # 3 - 826 - 2
          } elsif ($number < 0){
            $number += 256;
          }
          $num_bytes = 2;
          # (code removed)
        }
      }
    }
    print "\n";
      
    #
    # Zimmermann engine (truncated) - num_bytes only (end)
    #

    # End
    # No assembly or coding, just the mnenonic detection, instruction type and num_bytes
  }
  #
  # Print labels here
  #

  print "\n\nLabels found:\n";
  # https://www.perlmonks.org/?node_id=560981
  foreach (sort keys %labels) {
    #print "$_ : $labels{$_}\n";
    print "$_ : ";
    print_address($labels{$_});
    print "\n";
  }

  print "\n\n";
}

# Check if mnemonic is actually a label
# Use: $number = islabel($mnemonic);
# Will require two passes, for forward-declared, or used (?), labels
# See https://stackoverflow.com/questions/30085726/in-perl-how-can-i-make-two-passes-over-all-the-files-specified-on-the-command-l
# Returns: label value (an integer/address), or the original string
sub addlabel{
    die "Too many arguments for subroutine" unless @_ <= 1;
    die "Too few arguments for subroutine" unless @_ >= 1;
    my $label = $_[0];
    my $label_value = $label;       # pass-thru number if variable not exist
    if (!exists $categories{$label}){
      # It is either a bad mnemonic, or a label
      if (!exists $variables{$label}){
        $variables{$label} = $address+$num_bytes;  # add the label as a variable
        $labels{$label} = $address+$num_bytes;     # add the label to the extra list of labels
        $label_value = $address;
      }
    }
    return $label_value;
}


sub opcode2casehex{
  if ($flg_upper){
    $opcode = uc sprintf '%02x', $opcode if $flg_hex;
  } else {
    $opcode = lc sprintf '%02x', $opcode if $flg_hex;
  }
}

# Takes $address and $num_bytes as arguments
# Better to increment "manually" before calling
sub inc_and_print_address {
    die "Too many arguments for subroutine" unless @_ <= 2;
    die "Too few arguments for subroutine" unless @_ >= 2;
    my $address = $_[0];
    my $num_bytes = $_[1];
    $address += $num_bytes;
    if ($flg_hex) {
      if ($flg_upper) {
        print uc sprintf '%04x   ', $address;
      } else {
        print lc sprintf '%04x   ', $address;
      }
    } else {
      print $address."   ";
    }
    return $address;
}

# Takes $address as the argument
# Does not increment address
sub print_address {
    die "Too many arguments for subroutine" unless @_ <= 1;
    die "Too few arguments for subroutine" unless @_ >= 1;
    my $address = $_[0];
    if ($flg_hex) {
      if ($flg_upper) {
        print uc sprintf '%04x   ', $address;
      } else {
        print lc sprintf '%04x   ', $address;
      }
    } else {
      print $address."   ";
    }
    return;
}

sub print_line {
  #
  # Print the input line in the right hand side
  #

    # Print label in separate column
    my $tmp_space = ("   " x (5 - $num_bytes));
    print $tmp_space;
    if ($label ne ""){
      $tmp_space = (" " x (10 - length($label)));
      print $label;
      
      $label = "";
    } else {
      $tmp_space = (" " x 10);
    }
    print $tmp_space;

  #print $line."\n" if !$flg_quiet;
  #print "    ".$line if !$flg_quiet;
  # print spacing w.r.t $num_bytes
  if (!$flg_quiet){
    my $tmp_space = ("   " x (5 - $num_bytes));
    #print " ".$tmp_space;
  }
  if ($flg_upper){
    #print "    ".(uc $line) if !$flg_quiet;
    print uc $line if !$flg_quiet;
  } else {
    #print "    ".(lc $line) if !$flg_quiet;
    print lc $line if !$flg_quiet;
  }
}


sub print_timing {
  #
  # Print the timing in the right hand side
  #

  #print $line."\n" if !$flg_quiet;
  #print "    ".$line if !$flg_quiet;
  # print spacing w.r.t $num_bytes
  if (!$flg_quiet){
    my $tmp_space = (" " x (20 - length($line)));
    print " ".$tmp_space.$timing;
  }
}


#
# Code proper
#

#findlabelswithrewind();  # will not work as you cn't rewind stdin, obviously!
#findlabelswithslurp();   # Might work
findlabelswithlocal() if !$flg_no_labels;   # Seems to work, for a file at least.

while(<>){

  $line=$_;

  if ($line =~ /end/i) {                                       # END
    $num_bytes = 0;       # needed, or needed here?
    print $line;
    last;
  } elsif ($line =~ /(\w*) = (\$?\w+)/i) {                     # Variables

    $variables{$1} = $2 if $flg_no_labels;
    #$num_bytes=0;   # not needed?

    if ($flg_upper){
      print (uc $line) if !$flg_quiet;
    } else {
      print (lc $line) if !$flg_quiet;
    }
  } elsif ($line =~ /d[bc]\s*(\$?\w+)/i) {                     # DC/DB
    $operand_byte = $1;
    if ($operand_byte =~ s/^\$(\w+)/$1/){
      $operand_byte = hex $operand_byte;
    }

    $address += $num_bytes;
    print_address($address);

    # Set $num_bytes *after* printing the address!
    $num_bytes = 1;

    if ($flg_upper){
      $operand_byte = uc sprintf '%02x', $operand_byte if $flg_hex;
    } else {
      $operand_byte = lc sprintf '%02x', $operand_byte if $flg_hex;
    }
    print "$operand_byte";
    if ($flg_quiet) { print "\n";}

    print_line();
  } elsif ($line =~ /dw\s*(\$?\w+)/i) {                        # DW
    $operand_byte = $1;
    if ($operand_byte =~ s/^\$(\w+)/$1/){
      $operand_byte = hex $operand_byte;
    }

    $address += $num_bytes;
    print_address($address);

    # Set $num_bytes *after* printing the address!
    $num_bytes = 2;

    # We have dec input
    # ... and we need to split the bytes
    $operand_byte_two = int $operand_byte/256;
    $operand_byte = $operand_byte - ($operand_byte_two * 256);
    if ($flg_upper){
      $operand_byte = uc sprintf '%02x', $operand_byte if $flg_hex;
      $operand_byte_two = uc sprintf '%02x', $operand_byte_two if $flg_hex;
    } else {
      $operand_byte = lc sprintf '%02x', $operand_byte if $flg_hex;
      $operand_byte_two = lc sprintf '%02x', $operand_byte_two if $flg_hex;
    }
    print "$operand_byte $operand_byte_two";
    if ($flg_quiet) { print "\n";}

    print_line();
  } elsif ($line =~ /org\s*(\$?\w+)/i) {                     # ORG
    $address=$1;
    if ($address =~ s/^\$(\w+)/$1/){
      $address = hex $address;
    }
    if ($flg_upper){
      print (uc $line) if !$flg_quiet;
    } else {
      print (lc $line) if !$flg_quiet;
    }
  } elsif($line =~ /(.+)\s+(.+)/ || $line =~ /(.+)/){       # A real line of assembly code
    my $mnemonic = lc $1;
    my $operand = $2;
    if (exists $categories{$mnemonic}) {
      $category = $categories{$mnemonic};
    } else {
      #die "Unknown mnemonic!";
      # or check for LABEL here?
      if ($line!~/^(\w+)\s(.*)/){
        die "Unknown mnemonic!";
      } else {                                               # check for LABEL here
        $line =~s/^(\w+)\s(.*)/$2/;                          # Strip out label...
        $label = $1;
        if($line =~ /(.+)\s+(.+)/ || $line =~ /(.+)/){       # ... and run it again
          $mnemonic = lc $1;
          $operand = $2;
          if (exists $categories{$mnemonic}) {
            $category = $categories{$mnemonic};
          } else {
            die "Unknown mnemonic!";
          }
        }
      }
    }
    print $prefix_debug."category $category$suffix_debug" if $flg_debug;

    $address += $num_bytes;
    print_address($address);

    #
    # Zimmermann engine (start)
    #

    if ($category == 0) {
      $num_bytes = 1;
      $opcode = $baseOpCode{$mnemonic};
      $timing = $cycles{$mnemonic};
      opcode2casehex();
      #print "$opcode\n";
      print "$opcode";
      if ($flg_quiet) { print "\n";}
      print $prefix_debug."opcode $opcode$suffix_debug" if $flg_debug;

    } elsif ($operand =~ /^[Aa]$/) {
      if ($category == 3) {
        die "No! Can not be CATEGORY 3 FOR ACCUMULATOR OPERAND!";
      } else {
        $num_bytes = 1;
        $opcode = $baseOpCode{$mnemonic} + 8;
        $timing = $cycles{$mnemonic} - 4;
      }
      opcode2casehex();
      #print "$opcode\n";
      print "$opcode";
      if ($flg_quiet) { print "\n";}
    } elsif ( $operand =~ /^#(.+)/) {
      # 221 REM HANDLE "IMMEDIATE" INSTRUCTIONS HERE
      $num_bytes = 2;
      $operand_byte = $1;
      $operand_byte = check4variable($operand_byte);

      if ($category == 1){
        $opcode = $baseOpCode{$mnemonic} + 8;
        $timing = $cycles{$mnemonic} - 4;
      } elsif ($category == 4 || $category == 5){
        $opcode = $baseOpCode{$mnemonic};
        $timing = $cycles{$mnemonic};
      } else {
        die "No! Can not be $category,\n MUST BE CATEGORY 1, 4 or 5 FOR IMMEDIATE OPERAND!";
      }
      opcode2casehex();
      #$operand_byte =~ s/^\$(.+)/$1/;      # Strip $ from hex input
      if ($operand_byte !~ s/^\$(.+)/$1/) {      # Strip $ from hex input
        if ($flg_upper){
          $operand_byte = uc sprintf '%02x', $operand_byte if $flg_hex;
        } else {
          $operand_byte = lc sprintf '%02x', $operand_byte if $flg_hex;
        }
      }
      #print "$opcode $operand_byte\n";
      print "$opcode $operand_byte";
      if ($flg_quiet) { print "\n";}
    } elsif ( $operand =~ /^\((.+)/) {
      # 231 REM CHECK FOR VARIOUS INDIRECT INSTRUCTIONS
      if ( $operand =~ /(.+)\),Y$/ ) {
      # 233 REM IT IS AN "(INDIRECT),Y"
        $num_bytes = 2;
        $operand_byte = $1;
        $operand_byte = check4variable($operand_byte);
        
        if ($category == 1){
          $opcode = $baseOpCode{$mnemonic} + 16;
          $timing = $cycles{$mnemonic} - 1;
          opcode2casehex();
          #$operand_byte =~ s/^\$(.+)/$1/;      # Strip $ from hex input
          if ($operand_byte !~ s/^\$(.+)/$1/) {      # Strip $ from hex input
            if ($flg_upper){
              $operand_byte = uc sprintf '%02x', $operand_byte if $flg_hex;
            } else {
              $operand_byte = lc sprintf '%02x', $operand_byte if $flg_hex;
            }
          }
          #print "$opcode $operand_byte\n";
          print "$opcode $operand_byte";
          if ($flg_quiet) { print "\n";}
        } else {
          die "No! Can not be $category,\n MUST BE CATEGORY 1";
        }
      } elsif ( $operand =~ /(.+),X\)$/ ) {
        $num_bytes = 2;
        $operand_byte = $1;
        $operand_byte = check4variable($operand_byte);

        if ($category == 1){
          $opcode = $baseOpCode{$mnemonic};
          $timing = $cycles{$mnemonic};
          opcode2casehex();
          #$operand_byte =~ s/^\$(.+)/$1/;      # Strip $ from hex input
          if ($operand_byte !~ s/^\$(.+)/$1/) {      # Strip $ from hex input
          # NOT redundant in this particular case
            #$operand_byte = sprintf '%02x', $operand_byte if $flg_hex;
            if ($flg_upper){
              $operand_byte = uc sprintf '%02x', $operand_byte if $flg_hex;
            } else {
              $operand_byte = lc sprintf '%02x', $operand_byte if $flg_hex;
            }
          }    # NOT redundant in this particular case
          #print "$opcode $operand_byte\n";
          print "$opcode $operand_byte";
          if ($flg_quiet) { print "\n";}
        } else {
          die "No! Can not be $category,\n MUST BE CATEGORY 1";
        }
      } elsif ( $operand =~ /(.+)\)$/ ) {
        # 251 REM IT BETTER BE A JMP (INDIRECT), ELSE ERROR
        if ($category != 6){
          die "No! Not a JMP (INDIRECT)";
        } else {
          # 254 N=VAL(MID$(C$,2,L-2)):HI=INT(N/256):BY=3
          $num_bytes = 3;
          $operand_byte=$1;
          $operand_byte = check4variable($operand_byte);

          #if ($operand_byte =~ /^\$(.+)/){
          if ($operand_byte =~ s/^\$(.+)/$1/){
            $operand_byte = hex $operand_byte;  # convert to decimal
                                    # this makes the hex stuff below redundant
          }

=pod

          if ($operand_byte =~ s/^\$(.+)/$1/){ # Detect & Strip $ from hex input
          # redundant
            # We have hex input
            # ... and we need to split the bytes
            if (hex $operand_byte > 256){
              $operand_byte_two = substr($operand_byte,0,2);
              $operand_byte = substr($operand_byte,2,2);
              if ($flg_upper){
                $operand_byte = uc $operand_byte;
                $operand_byte_two = uc $operand_byte_two;
              } else {
                $operand_byte = lc $operand_byte;
                $operand_byte_two = lc $operand_byte_two;
              }
           } else {
              $operand_byte_two = "00";
              if ($flg_upper){
                $operand_byte = uc $operand_byte;
              } else {
                $operand_byte = lc $operand_byte;
              }
            }
          } else {    # redundant

=cut

            # We have dec input
            # ... and we need to split the bytes
            $operand_byte_two = int $operand_byte/256;
            $operand_byte = $operand_byte - ($operand_byte_two * 256);
            if ($flg_upper){
              $operand_byte = uc sprintf '%02x', $operand_byte if $flg_hex;
              $operand_byte_two = uc sprintf '%02x', $operand_byte_two if $flg_hex;
            } else {
              $operand_byte = lc sprintf '%02x', $operand_byte if $flg_hex;
              $operand_byte_two = lc sprintf '%02x', $operand_byte_two if $flg_hex;
            }
          #}      # redundant
          $opcode = $baseOpCode{$mnemonic} + 32;
          $timing = $cycles{$mnemonic} + 2;
          opcode2casehex();
          #$operand_byte =~ s/^\$(.+)/$1/;      # Strip $ from hex input (done above)
          #print "$opcode $operand_byte $operand_byte_two\n";
          print "$opcode $operand_byte $operand_byte_two";
          if ($flg_quiet) { print "\n";}
        }
      } else {
          die "No! Not ')'";
      }
    } elsif ( $operand =~ /(.+),X$/ ) {
      # 260 IF RIGHT$(C$,2)<>",X" GOTO 280
      # TODO
      my $number = $1;
      $number = check4variable($number);
      #if ($number =~ /^\$(.+)/){
      if ($number =~ s/^\$(.+)/$1/){
        $number = hex $number;  # convert to decimal
                                # this makes the hex stuff below redundant
      }
      if ($number < 256){
        # 265 REM HANDLE "ZERO PAGE,X" HERE
        $num_bytes = 2;
        $operand_byte = $number;
        $operand_byte = check4variable($operand_byte);
        if ($category == 2){
          $opcode = $baseOpCode{$mnemonic}+16;
          $timing = $cycles{$mnemonic} + 1;
        } elsif ($category == 1 || $category == 3 || $category == 5) {
          $opcode = $baseOpCode{$mnemonic} + 20;
          if ($category == 1) {
            $timing = $cycles{$mnemonic} - 2;
            #$opcode = $baseOpCode{$mnemonic} + 20;
          } elsif ($category == 3) {
            $timing = $cycles{$mnemonic} + 1;
            #$opcode = $baseOpCode{$mnemonic} + 16;
          } elsif ($category == 5) {
            $timing = $cycles{$mnemonic} + 2;
            #$opcode = $baseOpCode{$mnemonic} + 20;
         }
        } else {
          die "No!";
        }
        opcode2casehex();
        #$operand_byte =~ s/^\$(.+)/$1/;      # Strip $ from hex input
        #if ($operand_byte !~ s/^\$(.+)/$1/) {      # Strip $ from hex input
        # redundant
          if ($flg_upper){
            $operand_byte = uc sprintf '%02x', $operand_byte if $flg_hex;
          } else {
            $operand_byte = uc sprintf '%02x', $operand_byte if $flg_hex;
          }
        #}    # redundant
        #print "$opcode $operand_byte\n";
        print "$opcode $operand_byte";
        if ($flg_quiet) { print "\n";}
      } else {
        $num_bytes = 3;
        $operand_byte=$number;

=pod

        if ($operand_byte =~ s/^\$(.+)/$1/){ # Detect & Strip $ from hex input
        # redundant
          # We have hex input
          # ... and we need to split the bytes
          if (hex $operand_byte > 256){
            $operand_byte_two = substr($operand_byte,0,2);
            $operand_byte = substr($operand_byte,2,2);
            if ($flg_upper){
              $operand_byte = uc $operand_byte;
              $operand_byte_two = uc $operand_byte_two;
            } else {
              $operand_byte = lc $operand_byte;
              $operand_byte_two = lc $operand_byte_two;
            }
          } else {
            $operand_byte_two = "00";
            if ($flg_upper){
              $operand_byte = uc $operand_byte;
            } else {
              $operand_byte = lc $operand_byte;
            }
          }
        } else {     # redundant

=cut

          # We have dec input
          # ... and we need to split the bytes
          $operand_byte_two = int $operand_byte/256;
          $operand_byte = $operand_byte - ($operand_byte_two * 256);
          print $prefix_debug."oprnd#1 $operand_byte oprnd#2 $operand_byte_two$suffix_debug" if $flg_debug;
          if ($flg_upper){
            $operand_byte = uc sprintf '%02x', $operand_byte if $flg_hex;
            $operand_byte_two = uc sprintf '%02x', $operand_byte_two if $flg_hex;
          } else {
            $operand_byte = lc sprintf '%02x', $operand_byte if $flg_hex;
            $operand_byte_two = lc sprintf '%02x', $operand_byte_two if $flg_hex;
          }
        #}    # redundant
        if ($category == 2){
          # I don't believe that this code is ever reached for cat 2 absolute mode
          $opcode = $baseOpCode{$mnemonic} + 24;     # this should be +8
          $timing = $cycles{$mnemonic} + 1;
          die "This cat 2 absolute is never reached!";
        } elsif ($category == 1 || $category == 3 || $category == 5) {
          $opcode = $baseOpCode{$mnemonic}+28;
          if ($category == 1) {
            $timing = $cycles{$mnemonic} - 2;
          } elsif ($category == 3) {
            $timing = $cycles{$mnemonic} + 2;
          } elsif ($category == 5) {
            $timing = $cycles{$mnemonic} + 2;
          }
        } else {
          die "No!";
        }
        opcode2casehex();
        #$operand_byte =~ s/^\$(.+)/$1/;      # Strip $ from hex input (done above)
        #print "$opcode $operand_byte $operand_byte_two\n";
        print "$opcode $operand_byte $operand_byte_two";
        if ($flg_quiet) { print "\n";}
      }
    } elsif ( $operand =~ /(.+),Y$/ ) {
      # 280 IF RIGHT$(C$,2)<>",Y" GOTO 300 
      my $number = $1;
      $number = check4variable($number);
      #if ($number =~ /^\$(.+)/){
      if ($number =~ s/^\$(.+)/$1/){
        $number = hex $number;  # convert to decimal
                                # this makes the hex stuff below redundant
      }
      if ($number < 256){
        # 285 REM HANDLE ZERO PAGE,Y HERE
        $num_bytes = 2;
        if ($category == 2){
          $opcode = $baseOpCode{$mnemonic} + 16;
          $timing = $cycles{$mnemonic} + 1;
        } elsif ($category = 5) {
          $opcode = $baseOpCode{$mnemonic} + 20;
          $timing = $cycles{$mnemonic} + 2;
        } else {
          die "Not sure! Should continue to line 289/290???!!!???"; # I don't think so...
        }
        opcode2casehex();
        #$operand_byte =~ s/^\$(.+)/$1/;      # Strip $ from hex input
        #if ($operand_byte !~ s/^\$(.+)/$1/) {      # Strip $ from hex input
        # redundant
          if ($flg_upper){
            $operand_byte = uc sprintf '%02x', $operand_byte if $flg_hex;
          } else {
            $operand_byte = lc sprintf '%02x', $operand_byte if $flg_hex;
          }
        #}    # redundant
        #print "$opcode $operand_byte\n";
        print "$opcode $operand_byte";
        if ($flg_quiet) { print "\n";}
      } else {
        $num_bytes = 3;
        $operand_byte=$number;

=pod

        if ($operand_byte =~ s/^\$(.+)/$1/){ # Detect & Strip $ from hex input
        # redundant
          # We have hex input
          # ... and we need to split the bytes
          if (hex $operand_byte > 256){
            $operand_byte_two = substr($operand_byte,0,2);
            $operand_byte = substr($operand_byte,2,2);
            if ($flg_upper){
              $operand_byte = uc $operand_byte;
              $operand_byte_two = uc $operand_byte_two;
            } else {
              $operand_byte = lc $operand_byte;
              $operand_byte_two = lc $operand_byte_two;
            }
          } else {
            $operand_byte_two = "00";
            if ($flg_upper){
              $operand_byte = uc $operand_byte;
            } else {
              $operand_byte = lc $operand_byte;
            }
          }
        } else {    # redundant

=cut

          # We have dec input
          # ... and we need to split the bytes
          $operand_byte_two = int $operand_byte/256;
          $operand_byte = $operand_byte - ($operand_byte_two * 256);
          if ($flg_upper){
            $operand_byte = uc sprintf '%02x', $operand_byte if $flg_hex;
            $operand_byte_two = uc sprintf '%02x', $operand_byte_two if $flg_hex;
          } else {
            $operand_byte = lc sprintf '%02x', $operand_byte if $flg_hex;
            $operand_byte_two = lc sprintf '%02x', $operand_byte_two if $flg_hex;
          }
        #}    # redundant
        if ($category == 1){
          $opcode = $baseOpCode{$mnemonic}+24;
          $timing = $cycles{$mnemonic} - 2;
        } elsif ($category = 5) {
          $opcode = $baseOpCode{$mnemonic}+28;
          $timing = $cycles{$mnemonic} + 2;
        } else {
          die "No!";
        }
        opcode2casehex();
        #$operand_byte =~ s/^\$(.+)/$1/;      # Strip $ from hex input (done above)
        #print "$opcode $operand_byte $operand_byte_two\n";
        print "$opcode $operand_byte $operand_byte_two";
        if ($flg_quiet) { print "\n";}
      }
    } else {
      # 300 N=VAL(C$):REM NOW, FOR NUMERICAL OPERANDS
      my $number = $operand;  # Not really needed??? I think it is.
      $number = check4variable($number);
      #if ($number =~ /^\$(.+)/){
      if ($number =~ s/^\$(.+)/$1/){
        $number = hex $number;  # convert to decimal
                                # this makes the hex stuff below redundant
      }
      if ($category != 8) {
        if ($number < 256){
          $num_bytes = 2;
          $operand_byte = $number;
          if ($category == 2 || $category == 7) {
            $opcode = $baseOpCode{$mnemonic};
            $timing = $cycles{$mnemonic};
          } elsif ($category == 1 || $category == 3 || $category == 4 || $category == 5) {
            $opcode = $baseOpCode{$mnemonic} + 4;
            if ($category == 1) {
              $timing = $cycles{$mnemonic} - 3;
            } elsif ($category == 3) {
              $timing = $cycles{$mnemonic};
            } elsif ($category == 4 || $category == 5) {
              $timing = $cycles{$mnemonic} + 1;
            }
          } else {
            die "Not sure! Should continue to line 330???!!!???";
          }
          opcode2casehex();
          #$operand_byte =~ s/^\$(.+)/$1/;      # Strip $ from hex input
          #if ($operand_byte !~ s/^\$(.+)/$1/) {      # Strip $ from hex input
          # redundant
            if ($flg_upper){
              $operand_byte = uc sprintf '%02x', $operand_byte if $flg_hex;
            } else {
              $operand_byte = lc sprintf '%02x', $operand_byte if $flg_hex;
            }

=pod

          #}      # redundant

=cut

          #print "$opcode $operand_byte\n";
          print "$opcode $operand_byte";
          if ($flg_quiet) { print "\n";}
        } else {
          # 330 HI=INT(N/256):POKE 999,HI:POKE998,N-256*HI:BY=3
          $num_bytes = 3;
          $operand_byte=$number;

=pod

          #if ($operand_byte =~ s/^\$(.+)/$1/){ # Detect & Strip $ from hex input
          # redundant
            # We have hex input
            # ... and we need to split the bytes
            if (hex $operand_byte > 256){
              $operand_byte_two = substr($operand_byte,0,2);
              $operand_byte = substr($operand_byte,2,2);
              if ($flg_upper){
                $operand_byte = uc $operand_byte;
                $operand_byte_two = uc $operand_byte_two;
              } else {
                $operand_byte = lc $operand_byte;
                $operand_byte_two = lc $operand_byte_two;
              }
            } else {
              $operand_byte_two = "00";
              if ($flg_upper){
                $operand_byte = uc $operand_byte;
              } else {
                $operand_byte = lc $operand_byte;
              }
            }
          #} else {      # redundant

=cut

            # We have dec input
            # ... and we need to split the bytes
            $operand_byte_two = int $operand_byte/256;
            $operand_byte = $operand_byte - ($operand_byte_two * 256);
            if ($flg_upper){
              $operand_byte = uc sprintf '%02x', $operand_byte if $flg_hex;
              $operand_byte_two = uc sprintf '%02x', $operand_byte_two if $flg_hex;
            } else {
              $operand_byte = lc sprintf '%02x', $operand_byte if $flg_hex;
              $operand_byte_two = lc sprintf '%02x', $operand_byte_two if $flg_hex;
            }
          #}      # redundant
          if ($category == 2 || $category == 7) {
            $opcode = $baseOpCode{$mnemonic} + 8;
            $timing = $cycles{$mnemonic} + 1;
          } elsif ($category == 1 || $category == 3 || $category == 4 || $category == 5) {
            $opcode = $baseOpCode{$mnemonic} + 12;
            if ($category == 1) {
              $timing = $cycles{$mnemonic} - 2;
            } elsif ($category == 3) {
              $timing = $cycles{$mnemonic} + 1;
            } elsif ($category == 4 || $category == 5) {
              $timing = $cycles{$mnemonic} + 2;
            }
          } elsif ($category == 6 || $category == 9) {
            $opcode = $baseOpCode{$mnemonic};
            $timing = $cycles{$mnemonic};
          } else {
            die "No!";
          }
          opcode2casehex();
          #$operand_byte =~ s/^\$(.+)/$1/;      # Strip $ from hex input (done above)
          #print "$opcode $operand_byte $operand_byte_two\n";
          print "$opcode $operand_byte $operand_byte_two";
          if ($flg_quiet) { print "\n";}
        }
      } else {
        # 340 N=N-AD-2:IF N<-128 OR N>127 THEN PRINT "CAN'T BRANCH";N:GOTO 100
        # Do branches
        $number = $number - $address - 2;
        print $prefix_debug."number: $number address: $address$suffix_debug" if $flg_debug;

        if ($number < -128 || $number > 127) {
          die "No! CAN'T BRANCH $number!";                   # 3 - 826 - 2
        } elsif ($number < 0){
          $number += 256;
        }
        $num_bytes = 2;
        $operand_byte = $number;
        $opcode = $baseOpCode{$mnemonic};
        $timing = $cycles{$mnemonic};
        opcode2casehex();
        #$operand_byte =~ s/^\$(.+)/$1/;      # Strip $ from hex input
        if ($operand_byte !~ s/^\$(.+)/$1/) {      # Strip $ from hex input
          if ($flg_upper){
            $operand_byte = uc sprintf '%02x', $operand_byte if $flg_hex;
          } else {
            $operand_byte = lc sprintf '%02x', $operand_byte if $flg_hex;
          }
        }
        #print "$opcode $operand_byte\n";
        print "$opcode $operand_byte";
        if ($flg_quiet) { print "\n";}
      }
    }
    
=pod

    # Print label in separate column
    my $tmp_space = ("   " x (5 - $num_bytes));
    print $tmp_space;
    if ($label ne ""){
      $tmp_space = (" " x (10 - length($label)));
      print $label;
      
      $label = "";
    } else {
      $tmp_space = (" " x 10);
    }
    print $tmp_space;

=cut

    # Print line, timing with chomp
    chomp $line;
    print_line();
    print_timing();
    print "\n" if !$flg_quiet;

    #
    # Zimmermann engine (end)
    #
  }
}

#
# Print final newline, in case input file doesn't have one
#

print "\n";





