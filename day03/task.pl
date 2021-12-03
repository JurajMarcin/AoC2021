use 5.034;

my @ones = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
# my @ones = (0, 0, 0, 0, 0);
my @numbers = ();

while (<>) {
    push(@numbers, $_);
    my $number = $_ + 0;
    my $index = scalar(@ones) - 1;
    while ($number > 0) {
        if ($number % 10 == 1) {
            $ones[$index] += 1;
        }
        $number = int($number / 10);
        $index--;
    }
}


# Task 01

my $gamma = 0;
my $epsilon = 0;

for (@ones) {
    if ($_ > scalar(@numbers) / 2) {
        $gamma = $gamma * 2 + 1;
        $epsilon = $epsilon * 2;
    } else {
        $gamma = $gamma * 2;
        $epsilon = $epsilon * 2 + 1;
    }
}

say($gamma * $epsilon);


# Task 02

my @generator_numbers = @numbers;

my $index = 0;
while (scalar(@generator_numbers) > 1) {
    my $ones = scalar(grep(/^[01]{$index}1/, @generator_numbers));
    my $most_common = $ones >= scalar(@generator_numbers) / 2 ? "1" : "0";
    my $before_filter = scalar(@generator_numbers);
    @generator_numbers = grep(/^[01]{$index}$most_common/, @generator_numbers);
    $index++;
}

my $generator_rating = oct("0b" . $generator_numbers[0]);


my @scrubber_numbers = @numbers;

my $index = 0;
while (scalar(@scrubber_numbers) > 1) {
    my $ones = scalar(grep(/^[01]{$index}1/, @scrubber_numbers));
    my $most_common = $ones >= scalar(@scrubber_numbers) / 2 ? "1" : "0";
    my $before_filter = scalar(@scrubber_numbers);
    @scrubber_numbers = grep(!/^[01]{$index}$most_common/, @scrubber_numbers);
    $index++;
}

my $scrubber_rating = oct("0b" . $scrubber_numbers[0]);

say($generator_rating * $scrubber_rating);
