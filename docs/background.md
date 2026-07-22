# Background

This page collects the essential scientific context from the original PyGTide user guide: what Earth tides are, how ETERNA PREDICT works, how time is handled, what the tidal catalogues are, and — most importantly — how to interpret the absolute scale of the output.

## Earth tides

Gravity on Earth has an average value of 9.8 m/s² (1 Gal = 1 cm/s²) and can be measured with a precision of about 0.1 nm/s² (≈ 10⁻¹¹ g). Earth tides are variations in gravity induced by the relative movement of celestial bodies — mainly the Moon and the Sun — and they are the strongest disturbance of the gravity signal.

Earth-tide signatures appear in many geoscientific measurements, e.g. atmospheric pressure and groundwater levels, and can be exploited to quantify Earth processes and properties (aquifer parameters, for instance). Because Earth tides can be *predicted* very accurately, computed tides are routinely subtracted from gravity or water-level records to reveal the residual signal of interest. PyGTide provides exactly this prediction capability inside Python.

## ETERNA PREDICT heritage

- **ETERNA 3.3** was written in Fortran 77 by Prof. Dr.-Ing. Hans-Georg Wenzel (Black Forest Observatory, Universität Karlsruhe) and released in October 1996. Its subroutine PREDICT remains one of the most sophisticated routines for computing synthetic model tides (Wenzel, 1996).
- **ETERNA 3.4** added the Kudryavtsev (2004) KSM03 tidal catalogue — the most accurate harmonic development of the tide-generating potential.
- **PyGTide** modernised the code (COMMON blocks → modules, new f2py interface routines `INIT`, `WAVES`, `PREDICT`, Python data exchange via the `inout` module) and fixed numerous bugs in the process, including a date/time rounding bug at sampling rates below 60 s and, in v0.9/0.9.1, several issues in the strain and geodetic-coefficient routines (see the CHANGELOG).

## Time scales and leap seconds

Computing the positions of celestial bodies requires connecting two time scales: **UTC** (civil clock time, kept in step with Earth's rotation by leap seconds) and **Terrestrial Time TT** (the uniform time of the solar system). The relationship between the common time scales:

```text
                                                 ET 1960-1983
                                                TDT 1984-2000
 UTC 1972-  GPS 1980-    TAI 1958-               TT 2001-
----+---------+-------------+-------------------------+-----
    |         |             |                         |
    |<-- TAI-UTC (leaps) -->|<-----   TT-TAI    ----->|
    |         |             |      32.184s fixed      |
    |<GPS-UTC>|<- TAI-GPS ->|                         |
    |         |  19s fixed  |                         |
    |                                                 |
    |<----- DDT = TT-UTC = delta-T + delta-UT ------->|
    |                                                 |
    <->| delta-UT = UT1-UTC (maximum +/-0.9 sec)      |
-------+----------------------------------------------+-----
       |<------------ delta-T = TT-UT1 -------------->|
      UT1 (UT)                                    TT/TDT/ET
```

PyGTide input and output times are **UTC**; the Fortran core converts internally using the leap-second table in `etddt.dat` (tabulated values from 1972 onward, interpolation before). The file must be updated occasionally — run `pygtide.update()` (see [Usage — Updating the internal databases](usage.md#updating-the-internal-databases)).

## Pole wobble and length of day

- The Earth's rotation axis moves within a square of roughly 20 m, with dominant periods of 12 months (annual wobble) and ~14 months (Chandler wobble). The resulting **pole tide** can change gravity by up to ~13 µGal and must be corrected for.
- The rotation rate (**length of day**, LOD) varies due to angular-momentum exchange with the oceans and atmosphere, causing additional small gravity variations.

Both corrections rely on daily pole coordinates and UT1 values in `etpolut1.dat` (IERS observations from 1962 plus USNO forecasts up to a year ahead). They are enabled by default for gravity (`poltidecor=1.16`, `lodtidecor=1.16`) and appear as separate output columns.

## Tidal catalogues

The tide-generating potential is a harmonic development with thousands of waves; successive catalogues improved the accuracy by about an order of magnitude per generation:

| Catalogue | Name | Waves | RMSE [nGal] | `tidalpoten` |
|---|---|---|---|---|
| Doodson (1921) | — | 378 | 102 | 1 |
| Cartwright–Tayler–Edden (1973) | — | 505 | 37.4 | 2 |
| Buellesfeld (1985) | — | 656 | 24 | 3 |
| Tamura (1987) | T87 | 1200 | 6.7 | 4 |
| Xi (1989) | XI1989 | 2933 | 7.9 | 5 |
| Roosbeek (1996) | RATGP95 | 6499 | 2 | 6 |
| Hartmann & Wenzel (1995) | HW95 | 12935 | 0.14 | 7 |
| Kudryavtsev (2004) | KSM03 | 28806 | 0.025 | 8 |

All catalogues are stored in the HW95 normalisation. HW95 owes its accuracy to the DE200 ephemerides over 300 years; KSM03 uses DE/LE405 over 1600–2200. PyGTide defaults to KSM03.

## The elastic Earth model

Body tides are computed with the **Wahr–Dehant–Zschau model**: latitude-dependent gravimetric factors δ, Love numbers h and k, Shida numbers l and tilt factors γ = 1 + k − h from Dehant (1987) for an elliptical, rotating, oceanless Earth with a liquid outer core and inelastic mantle. The nearly-diurnal free wobble (NDFW) resonance near the K1 frequency is included, which is why diurnal waves (O1 vs K1) have noticeably different amplitude factors. Strain components use a spherical approximation with a Poisson ratio of 0.25 (Zürn & Wilhelm, 1984).

## Absolute scale of the output

> [!IMPORTANT]
> This is the single most important convention to understand before interpreting PyGTide amplitudes.

PyGTide follows the ETERNA PREDICT design: the wave-group normalisation divides the body-tide factor of each group's *main wave* out of the signal. As a result:

- **Potential, gravity and tilt** (`tidalcompo = -1, 0, 1`) are **rigid-Earth (geometric) tides**. The Wahr–Dehant–Zschau factors (δ ≈ 1.16 for gravity, 1 + k ≈ 1.30 for the potential, γ ≈ 0.69 for tilt) only modulate the *relative* amplitudes of satellite waves within a group — including the NDFW resonance in the diurnal band.
- **Displacement and strain** (`tidalcompo = 2…8`) already include the Love numbers h and l absolutely — they are **body tides**.

Keep this in mind when combining components (e.g. gravity with displacement or strain). To obtain body-tide gravity or tilt, set the wave-group amplitude factor accordingly, e.g. for gravity:

```python
pt.set_wavegroup(np.asarray([[0, 10, 1.16, 0]]))
```

A single 0–10 cpd group carries only one factor for the whole spectrum; because δ varies between bands, a per-band split is more accurate (full recipe in [Usage — Wave groups](usage.md#wave-groups)).

## Validation

The original user guide compared PyGTide with the peer-reviewed package [TSoft](https://seismologie.oma.be/en/downloads/tsoft) (Van Camp, 2005) for Karlsruhe, January 2017:

- With the **same** catalogue (T87), PyGTide and TSoft agree to rounding level.
- With **KSM03**, residuals against TSoft reflect the catalogue improvement (KSM03 is ~7× more accurate than HW95, ~50× more accurate than T87) and TSoft's lack of pole/LOD corrections.

PyGTide's own test suite additionally regression-checks an hourly strain series and the M2 frequency — run it any time with `python -c "import pygtide; pygtide.test(msg=True)"`.

## References

- Agnew, D. C. (2010), Earth tides, in *Treatise on Geophysics: Geodesy*, Elsevier.
- Dehant, V. (1987), Tidal parameters for an inelastic Earth, *Physics of the Earth and Planetary Interiors*, 49, 97–116.
- Hartmann, T., and H.-G. Wenzel (1995), The HW95 tidal potential catalogue, *Geophysical Research Letters*, 22(24), 3553–3556, <https://doi.org/10.1029/95GL03324>.
- Kudryavtsev, S. M. (2004), Improved harmonic development of the Earth tide-generating potential, *Journal of Geodesy*, 77(12), 829–838, <https://doi.org/10.1007/s00190-003-0361-2>.
- McMillan, T. C., Rau, G. C., Timms, W. A., and Andersen, M. S. (2019), Utilizing the impact of Earth and atmospheric tides on groundwater systems, *Reviews of Geophysics*, <https://dx.doi.org/10.1029/2018RG000630>.
- Van Camp, M., and P. Vauterin (2005), Tsoft: graphical and interactive software for the analysis of time series and Earth tides, *Computers & Geosciences*, 31(5), 631–640.
- Wahr, J. M. (1985), Deformation induced by polar motion, *Journal of Geophysical Research*, 90(B11), 9363–9368.
- Wenzel, H.-G. (1996), The nanogal software: Earth tide data processing package ETERNA 3.30, *Bulletin d'Informations des Marées Terrestres*, 124, 9425–9439.
- Zürn, W., and H. Wilhelm (1984), Tides of the solid Earth, in *Landolt-Börnstein, New Series, Group V, Vol. 2*, Springer.
