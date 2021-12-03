# An introduction to DDRP

Invasive pests present a significant threat to agricultural production
in the United States, yet decision support tools that can accurately
predict where and when to expect pests have not yet been fully developed
and utilized. Our spatial modeling platform known as DDRP (Degree-Days,
Risk, and Phenological event mapping) was designed to provide regularly
updated forecasts of the potential distribution (risk of establishment)
and timing of seasonal activities (phenology) of pests [(Barker et
al. 2020)](https://doi.org/10.1371/journal.pone.0244005). Currently we
are using DDRP to produce regularly updated (every three days) forecasts
for 15 high-risk pest insects for the USDA APHIS Cooperative
Agricultural Pest Survey (CAPS) program, available at
[USPest.org](http://uspest.org/CAPS). The program has also been adapted
to predict the phenology and voltinism (number of generations per year)
of three biological control insects that have photoperiod-cued diapause
(Grevstad et al. in press), also available at
[USPest](http://uspest.org/dd/dodmaps).

# Program overview

![‘Flow diagram of DDRP’s modeling
process’](https://github.com/bbarker505/ddrp_v2/blob/master/images/model_overview.png)\]

# Inputs and outputs

DDRP uses a process-based (mechanistic) approach to model
temperature-dependent development, phenology, and climate suitability of
target species. The platform requires gridded daily minimum and maximum
temperature data (Tmin and Tmax, respectively), and information on the
temperature requirements for development and survival of a species. We
typically run DDRP using current and forecast climate data for the
conterminous U.S. to provide real-time decision support for a species;
however, the platform accepts data for any time frame or region, such as
data for past years or for other countries. Model products include maps
of the predicted potential distribution (climate-based risk of
establishment), number of generations, and dates of phenological events
(pest forecasts). The potential distribution is represented by areas
where cold and heat stress accumulations have not exceeded the stress
limits of a species.

# Program features

Some of the major features of DDRP currently include: 1) Degree-day
parameters including durations and lower and upper developmental
thresholds for four separate life stages (these are the egg, the larva
or nymph, the pupa or pre-oviposition, and the adult), plus a separately
parameterized overwintering stage. 2) The ability to spread the
population using cohorts. Typically seven cohorts are specified but any
number can be used. While cohorts offer the ability to spread the
population in a Gaussian or other distribution, there is currently no
distributed-delay function, meaning that the spread does not increase
over multiple generations. 3) Phenological event maps (PEMs, also known
as pest event maps), which depict estimated calendar dates of seasonal
activities or population events. PEM parameters are specified as
degree-days within each of the four (plus overwintering) stages. For
example, DDRP can be parameterized to make first egg-hatch PEMs by
setting a degree-day value near the completion of the egg stage, or at
the beginning of the larval stage. If the former is used, then a second
PEM, say for mid-larval development, could be parameterized using a
value such as one-half of the degree-day total for larval development.
4) Climatic suitability maps, which show two levels of climatic
suitability (moderate and severe stress exclusions). These are intended
to indicate risk likelihood of short vs. long-term establishment but
could also indicate migration zones, and uncertainties such as in
species parameterization, model structure, and in the sources of climate
data.

# Setup and usage

DDRP is an R script (“DDRP\_v2.R”) and must be within the same directory
an auxilliary R script that contains program functions
(“DDRP\_v2\_funcs.R”). DDRP has not yet been formatted into an R package
because we designed the code to be run from the command line on a Linux
server. The program can also be run on a Windows system but parallel
processing capabilities will be limited. The user manual
“DDRP\_user\_guide\_and\_platform\_requirements\_V4.pdf” is the only
instruction document that is currently available, but stay tuned on the
development of an R package for DDRP and a vignette on how to use the
platform. See the instruction manual for details on program
requirements, input data, input options, examples of command line
arguments, types of output files, and run times.

# Philosophy

Our development of DDRP has strived to achieve a parsimonious balance of
both model simplicity and accuracy, with a focus on four philosophies:
1) simplicity, in that existing data and results for well-studied, major
invasive threats can be readily adapted for use; 2) universality, to
accommodate a wide range of organisms; 3) robustness, by having the
emphasis on use of first-principles that lend to process-driven rather
than statistical correlation-driven models; and 4) practicality, by
focusing on models that can be used for decision support rather than
more complex research-only models.

# Example

# References

Barker, B. S., L. Coop, T. Wepprich, F. Grevstad, and G. Cook. 2020.
DDRP: real-time phenology and climatic suitability modeling of invasive
insects. PLoS ONE 15:e0244005.
<https://doi.org/10.1371/journal.pone.0244005>.

Grevstad, F. G., T. Wepprich, B. S. Barker, L. B. Coop, R. Shaw, and R.
S. Bourchier. In press. Combining photoperiod and thermal responses to
predict phenological mismatch for introduced insects. Ecological
Applications.