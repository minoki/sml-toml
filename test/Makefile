all: decoder utf8
.PHONY: all

sources = \
  ../src/validate_utf8.sml \
  ../src/error.sml \
  ../src/value_handler.sig \
  ../src/parse_toml.sml \
  ../src/value.sig \
  ../src/value.sml \
  ../toml.mlb

decoder: decoder.mlb decoder.sml $(sources)
	mlton -default-ann 'warnUnused true' decoder.mlb

utf8: utf8.mlb utf8.sml $(sources)
	mlton utf8.mlb
