#!/bin/bash

cd install/bin

/strawberry-perl/perl/bin/perl.exe /strawberry-perl/perl/site/bin/pp \
    -o plinker.exe plinker.pl
/strawberry-perl/perl/bin/perl.exe /strawberry-perl/perl/site/bin/pp \
    -o tce-dump.exe tce-dump.pl
