my $prev_depth = -1;
my $increasing = 0;

while (<>) {
    my $depth = $_ + 0;
    if ($prev_depth != -1 and $depth > $prev_depth) {
        $increasing++;
    }
    $prev_depth = $depth;
}

print $increasing;
print "\n"
