#!/usr/bin/env bash

set -euo pipefail
set -x

cargo +nightly fmt
cargo test
cargo run
cargo clippy
