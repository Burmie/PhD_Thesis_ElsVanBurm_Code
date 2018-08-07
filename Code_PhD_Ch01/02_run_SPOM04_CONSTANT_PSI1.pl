#!/usr/bin/perl
use strict;
use warnings;

my $ncores = 16;

my $q = "*.Rda";
system("cp ../01_data_simulation/SPOM04/$q .");
my @rdafiles = glob "'${q}'";
print "number of Rda files found: ",scalar(@rdafiles),"\n";

foreach my $rdafile (@rdafiles) {
	my $r_file = $rdafile."_Rscript4MCMC.R";
	open FH,">".$r_file;
	print FH "run_name = \"$rdafile\"\n\n";
	open IN,"02_model_fit_SPOM04_CONSTANT_PSI1.R";
	while (my $line = <IN>) {print FH $line}
	close IN;
	close FH;
}

for (my $sup = 0; $sup < $ncores; ++$sup) {
	my $sh_file = "SPOM04_run_job_$sup.sh";
	open FH,">$sh_file";
	print FH '#!/bin/bash',"\n";
	for (my $base = 0; $base < scalar @rdafiles; $base += $ncores) {
		my $filenr = $base + $sup;
		if (scalar @rdafiles > $filenr) {
			my $rdafile = $rdafiles[$filenr];
			my $r_file = $rdafile."_Rscript4MCMC.R";
			print FH 'R --no-save < ',$r_file," 1> ",$rdafile,"_screenout.txt 2> ".$rdafile."_screenout.txt\n";
		}
	}
	close FH;
	system("nohup bash $sh_file &");
}
