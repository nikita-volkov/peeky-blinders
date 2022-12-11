build: format
	stack build --fast --test --bench --no-run-benchmarks

format:
	path=peeky-blinders.cabal && cabal-fmt -c --indent 2 $$path || cabal-fmt -i --indent 2 $$path
	ormolu --mode inplace -ce $$(find . -name "*.hs" -not -path "./.stack-snapshot/*" -not -path "./dist/*" -not -path "./dist-newstyle/*" -not -path "./*.stack-work/*" -not -path "./sketches/*")
