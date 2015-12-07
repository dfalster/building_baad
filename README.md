# The challenge of combining 176 x #otherpeoplesdata to create the Biomass And Allometry Database (BAAD)

This code repository contains code needed to reproduce the slides of a talk given at the Ecological Society of Australia conference, in Adelaide on Wednesday, December 2, 2015:

Falster DS, FitzJohn RG, Duursma RA & Darneche D "The challenge of combining 176 x #otherpeoplesdata to create the Biomass And Allometry Database (BAAD)". doi: [xxx](http://xxx)

**Abstract**: Despite the hype around "big data", a more immediate problem facing many scientific analyses is that large-scale databases must be assembled from a collection of small independent and heterogeneous fragments -- the outputs of many and isolated scientific studies conducted around the globe. Together with 92 other co-authors, we recently published the Biomass And Allometry Database (BAAD) as a data paper in the journal Ecology, combining data from 176 different scientific studies into a single unified database. BAAD is unique in that the workflow -- from raw fragments to homogenised database -- is entirely open and reproducible. in this talk I introduce BAAD and illustrate solutions for some of the challenges of working with lots and lots of #otherpeople's data.

**Slides**: A copy of the compiled presentation is available at ...

# Rebuilding

## Requirements

Fonts (all are freely downloadable)

* [Font Awesome](http://fortawesome.github.io/Font-Awesome/)
* [Yanone Kaffeesatz](https://www.google.com/fonts#UsePlace:use/Collection:Yanone+Kaffeesatz)
* [Bitstream Vera Sans Mono](http://ftp.gnome.org/pub/GNOME/sources/ttf-bitstream-vera/1.10/)


Making the pdf requires a reasonably complete XeLaTeX (and fontspec) installation (e.g. [MacTeX](https://tug.org/mactex/) for OSX or [MikTex](http://miktex.org/) for windows).

To make figures and compile the pdf we we use the [remake](https://github.com/richfitz/remake) package for R. See the info there for installation instructions.

## Running

Download this repo, then open a new R session with this project set as working directory. We use a number of packages, these can be easily installed by remake:

```r
remake::install_missing_packages()
```

Then, to generate the figures in the paper, run

```r
remake::make("figures")
```


And then the slides:

```r
remake::make()
```


# Credits

The outline for this talk and some of content was forked from [Rich FitzJohn's talk on reproducibility](https://github.com/richfitz/reproducibility-2014/releases/download/v1.0/slides.pdf), which uses design from this [great talk](http://bost.ocks.org/mike/d3/workshop/#0).
