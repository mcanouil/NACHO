# verbs point NACHO 2 objects to upgrade_nacho()

    Code
      normalise(nacho_2())
    Condition
      Error in `normalise()`:
      ! `nacho_object` is a NACHO 2 object, which NACHO 3 cannot use.
      i Convert it with `upgrade_nacho(nacho_object)`, or read the saved file with `read_nacho()`.

# autoplot() points NACHO 2 objects to upgrade_nacho()

    Code
      autoplot(nacho_2(), type = "BD")
    Condition
      Error in `autoplot()`:
      ! `object` is a NACHO 2 object, which NACHO 3 cannot use.
      i Convert it with `upgrade_nacho(object)`, or read the saved file with `read_nacho()`.

# check_nacho() names an incomplete NACHO 2 object

    Code
      normalise(broken)
    Condition
      Error in `normalise()`:
      ! `nacho_object` has the NACHO 2 class <nacho>, but not the NACHO 2 data.
      i Create a NACHO 3 object with `load_rcc()`.

# check_nacho() refuses an object from another schema

    Code
      nacho_samples(x)
    Condition
      Error in `nacho_samples()`:
      ! `x` was made with object schema 99.
      i Schema 99 is newer than schema 1, which this NACHO reads. Update NACHO to read it.

