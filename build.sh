#!/usr/bin/env bash

set -euo pipefail
set -x

cargo fmt
cargo test
cargo run
cargo clippy
