KEY := corvus-test-key

.PHONY: all build check clean rebuild cache-clean

all: build

build:
	@test -f $(KEY) || ssh-keygen -t ed25519 -f $(KEY) -N '' -C corvus-test
	@test -f $(KEY).pub

check:
	@test -f $(KEY) -a -f $(KEY).pub

# The key is intentionally retained by normal image cleanup so rebuilding an
# image does not silently rotate credentials used by a running test node.
clean cache-clean:
	@:

rebuild: build
