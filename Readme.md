# Tool for analyzing Lego project data #

We keep these separate from the actual program as this might be
redistributed and we don't want any clutter.

## Building

The program consists of some F# code that transforms the raw data and
outputs it again cleaned and csv-formatted. Build this program simply
by running:

```
$ sh build.sh
```

## Running

To plot this data, you need to pipe it into ```plot.py```, which is
automated in ```aggregate.sh```. The script aggregates any number of
files you pass to it. Single files are also valid. Just run something
like:

```
$ sh aggregate.sh logs/*{1,2}.csv
```
