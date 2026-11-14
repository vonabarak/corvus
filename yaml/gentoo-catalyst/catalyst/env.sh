#!/bin/bash
# Catalyst owns FEATURES and emerge invocation; Portage owns remote trust checks.
export EMERGE_DEFAULT_OPTS="--getbinpkg=y --binpkg-respect-use=y"
export GPG_VERIFY_USER_DROP=""
export GPG_VERIFY_GROUP_DROP=""
export PORTAGE_TRUST_HELPER=getuto
