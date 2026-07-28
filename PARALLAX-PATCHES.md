# Parallax divergences from Denton's original MULTALL-20.6

This repository is a Parallax-owned copy of John Denton's MULTALL-open 20.6
distribution. Everything here is Denton's work except the changes recorded
below. If you are diffing this tree against the original distribution, this
file is the complete list of what is ours and why.

Every modification is marked in the source with a banner comment reading
`PARALLAX MODIFICATION <date> -- NOT IN DENTON'S ORIGINAL`, and each keeps the
original statement it replaced on a `WAS:` line, so any change can be reverted
from the source alone.

## 2026-07-27 — fixed-width/list-directed field collisions in the pressure records

**Symptom.** Above roughly 10 bar the toolchain could not be driven at all:
MEANGEN succeeded, STAGEN aborted immediately with

```
At line 532 of file stagen-18.1.f (unit = 7, file = 'stagen.dat')
Fortran runtime error: Bad real number in item 1 of list input
```

**Cause.** Each program *writes* its deck with fixed-width `Fw.d` edit
descriptors and the next one *reads* it list-directed. `Fw.d` right-justifies
in exactly `w` columns, so a value whose printed form is `w` characters long
leaves no blank in front of it, and list-directed input needs that blank to
separate one value from the next. The two halves therefore agree only while
the values stay narrow enough — which, for a pressure in pascals, means below
10 bar. Every realistic gas-turbine core condition is above that.

The values were never wrong; the formats were too narrow.

| file | format | record | collided at | now |
| --- | --- | --- | --- | --- |
| `MEANGEN/meangen program/meangen-17.4.f` | `1011` | `RPM, STATIC PRESSURES THROUGH ROW` (`stagen.dat`, unit 9) | 10 bar | `5(1X,F11.2)`, tab `T55`→`T65` |
| `STAGEN/stagen program/stagen-18.1.f` | new `1718` | every pressure record of `stage_new.dat` (unit 10): `PUPROW/PLEROW/PTEROW/PDROW`, the spanwise `PO1`, `PDOWN_HUB/PDOWN_TIP` | 100 bar | `8(1X,F11.1)`; `1701` itself left as Denton wrote it — see below |

`1X` rather than a wider `F` alone: widening `5F10.2` to `5F12.2` only moves
the cliff from 10 bar to 100 bar, because the failure is a *full* field, not an
overflowing one. A leading `1X` guarantees the separator at any magnitude, and
the `F11` mantissa then carries the value itself to about 1000 bar.

**Tabs moved with the widths.** `1011`'s numbers grew from 50 to 60 columns, so
its `T55` label position would have overwritten the data; it is now `T65`.

**Read side checked before the write side was changed.** Every numeric read of
`stagen.dat` in `stagen-18.1.f` is list-directed `READ(7,*)`; the only
formatted reads of that unit are the text header lines (`FORMAT(72A)`).
Likewise `multall-open-20.6.f` reads all of the unit 10 records list-directed
on the new-readin path (lines 964, 1909, 1963).

**Why `1701` is split rather than widened.** The old-readin path *is*
column-sensitive (`FORMAT(8F10.6)` and friends, from line 2214), it is selected
by `intype = "O"`, and it reads `stage_old.dat`. `stage_old.dat` is unit 4, and
unit 4 is **not** written by FORMAT `1605` alone: `1605` carries one record
(line 565), but `1701` carries three more — `PUPHUB/PUPTIP/PDHUB/PDTIP` and the
spanwise `PO1` and `TO1`, at lines 2215, 2221, 2222. `1701` is therefore shared
between the list-directed new-readin deck and the column-sensitive old-readin
deck, and widening it in place reaches both.

Measured, at 30 bar: widening `1701` in place pushes those three records from
80 to 96 columns, and MULTALL's `OLD_READIN` — still slicing at 80 via
`FORMAT(8F10.6)` — reads a `3000000.0` Pa inlet stagnation pressure back as
`3.0` and `0.03` Pa. **Silently**: wrong numbers, no error, no abort.

So the format is split. A new `1718 FORMAT(8(1X,F11.1))` takes the four unit 10
writes (lines 610, 617, 2244, 2261); `1701 FORMAT(8F10.1)` keeps the three
unit 4 writes and is byte-identical to Denton's original. `stage_old.dat` is
then byte-for-byte upstream — verified at 30 bar and 110 bar — while
`stage_new.dat` gets the separator it needs.

**Formats examined and deliberately left alone** (shown not to be reachable,
rather than blanket-widened):

| file | format | writes to | why it is safe |
| --- | --- | --- | --- |
| `meangen-17.4.f` `1014` | `6I5,F10.5,F10.2` | `stagen.dat` | the six integers are literals `0,0,1,1,1,1`; `FRACTIP` is a fraction; `RPMHUB` would need 1,000,000 rpm to fill `F10.2` |
| `meangen-17.4.f` `129` | `8F10.4` | `meangen.out` (unit 10, the echo file) | `XMEAN`/`RMEAN` are metres and `VMRAT` is a ratio; `F10.4` would need 10000 to collide, and nothing parses this file numerically |
| `meangen-17.4.f` `304` | `8F10.5` | unit 6, the terminal | hub/tip axial positions and radii in metres; not a deck |
| `meangen-17.4.f` `1021` | `4F15.3` | `stagen.dat` | pascals, but 15 columns reach 1e11 Pa |
| `stagen-18.1.f` `1702` | `F10.1,3F10.3,F10.1,2F10.3` | `stage_new.dat` (unit 10) and `stage_old.dat` (unit 4) | the two `F10.1` fields are `REYNO` and `TURBVIS_LIM`. A Reynolds number would have to reach 10,000,000.0 to fill the field; at the top of the declared operating domain (100 bar) a blade-chord Reynolds number is around 4.5e6, so it keeps its separator. Reachable only for a machine far outside this envelope — recorded here rather than widened, so the next person knows it was looked at |
| `stagen-18.1.f` `1605` | `F10.1,4F10.1,F10.5,F10.1` | `stage_old.dat` | same hazard in principle, but the old-format deck is not produced or consumed by any current workflow; left as Denton wrote it |
| `stagen-18.1.f` `1701` | `8F10.1` | `stage_old.dat` (unit 4), 3 records | column-sensitive on the old-readin path — deliberately **not** widened; the unit 10 writes moved to `1718` instead. See "Why `1701` is split" above |

**Verification.** meangen and stagen rebuilt from the patched sources with
`gfortran -O2 -std=legacy` and driven end to end through MULTALL 20.6 at
10.7 bar and 30 bar, plus the NASA CR-165608 E³ HP-turbine design point at
13.245 bar. The 13.245 bar case reproduces a run made from the unpatched
sources (with the collided record repaired by hand) to the last printed digit —
η_tt 0.916576147, pressure ratio 3.40803599, stage work 12289.1504 kW —
confirming the patch changes formatting only and no computed value.
