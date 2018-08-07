#!/usr/bin/perl
use strict;
use warnings;
use Data::Dumper;

my $ncores = 16;

my @rdafiles;

opendir (DIR, "../02_model_fit/SPOM03/") or die $!;
while (my $file = readdir(DIR)) {
    if ($file =~ /\.Rda$/) {    
		#print "$file\n";
		push @rdafiles,$file;
	}
}
closedir(DIR);
print "number of Rda files found: ",scalar(@rdafiles),"\n";

foreach my $rdafile (@rdafiles) {
	my $r_file = $rdafile."_Rscript4projections.R";
	open FH,">".$r_file;
	print FH "run_name = \"$rdafile\"\n\n";
	open IN,"03_projections_100years_SPOM03.R";
	while (my $line = <IN>) {print FH $line}
	close IN;
	close FH;
}

for (my $sup = 0; $sup < $ncores; ++$sup) {
	my $sh_file = "SPOM03_run_job_$sup.sh";
	open FH,">$sh_file";
	print FH '#!/bin/bash',"\n";
	for (my $base = 0; $base < scalar @rdafiles; $base += $ncores) {
		my $filenr = $base + $sup;
		if (scalar @rdafiles > $filenr) {
			my $rdafile = $rdafiles[$filenr];
			my $r_file = $rdafile."_Rscript4projections.R";
			print FH 'R --no-save < ',$r_file," 1> ",$r_file,"_screenout.txt 2> ".$r_file."_screenout.txt\n";
		}
	}
	close FH;
	system("nohup bash $sh_file &");
}
