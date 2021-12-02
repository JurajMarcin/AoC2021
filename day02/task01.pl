use 5.034;

my @position = (0, 0);

while (<>) {
    if ($_ =~ /forward (\d+)/) {
        $position[0] += $1;
    } elsif ($_ =~ /up (\d+)/) {
        $position[1] -= $1;
    } elsif ($_ =~ /down (\d+)/) {
        $position[1] += $1;
    }
}

say($position[0] * $position[1]);
