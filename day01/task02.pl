my $prev_sum = -1;
my $increasing = 0;
@sliding_window = (-1, -1, -1);

while (<>) {
    my $depth = $_ + 0;
    $sliding_window[0] = $sliding_window[1];
    $sliding_window[1] = $sliding_window[2];
    $sliding_window[2] = $depth;
    $sum = $sliding_window[0] + $sliding_window[1] + $sliding_window[2];
    if ($sliding_window[0] != -1) {
        if ($prev_sum != -1 and $sum > $prev_sum) {
            $increasing++;
        }
        $prev_sum = $sum;
    }
}

print $increasing;
print "\n";
