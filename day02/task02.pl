use 5.034;

my @position = (0, 0, 0);

while (<>) {
    if ($_ =~ /forward (\d+)/) {
        $position[0] += $1;
        $position[1] += $1 * $position[2];
    } elsif ($_ =~ /up (\d+)/) {
        $position[2] -= $1;
    } elsif ($_ =~ /down (\d+)/) {
        $position[2] += $1;
    }
}

say($position[0] * $position[1]);
