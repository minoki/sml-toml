typecheck:
	mlton -stop tc toml.mlb

check-typespec:
	lunarml compile --default-ann "valDescInComments error" toml.mlb

.PHONY: typecheck check-typespec
