all: decoder
.PHONY: all

sources = \
  ../src/value.sig \
  ../src/value.sml \
  ../src/parse_toml.sml \
  ../src/validate_utf8.sml \
  ../toml.mlb

decoder: decoder.mlb decoder.sml $(sources)
	mlton -default-ann 'warnUnused true' decoder.mlb
