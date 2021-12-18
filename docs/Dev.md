# Notes on building docs


0) Install pygments (`make report-deps`)

1) Add these two lines into your settings.json (which overwrites the default compilation commands and uses our makefile instead)

    "latex-workshop.latex.external.build.command": "make",
    "latex-workshop.latex.external.build.args": [
        "report"
    ],

2) Install some additional cpan components

    cpan Unicode::GCString
    cpan App::cpanminus
    cpan YAML::Tiny
    perl -MCPAN -e 'install "File::HomeDir"'

3) When making big changes, try a clean build before pushing: `make clean-docs report`
