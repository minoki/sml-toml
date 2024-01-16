typecheck:
	mlton -stop tc -default-ann "warnUnused true" toml.mlb

check-typespec:
	lunarml compile --default-ann "valDescInComments error" toml.mlb

.PHONY: typecheck check-typespec
