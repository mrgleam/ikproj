# Variable for filename for store running procees id
PID_FILE = /tmp/ikapi.pid

test:
	stack test

restart-test: test
	@echo "RESTARTED live reload testing" && printf '%*s\n' "40" '' | tr ' ' -

test-watch: test
	fswatch -or --event=Updated ./src ./app ./test | \
	xargs -n1 -I {} make restart-test

# .PHONY is used for reserving tasks words
.PHONY: test-watch test