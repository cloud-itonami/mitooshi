# mitooshi — standalone actor repository

Mitooshi is a probabilistic forecasting observatory. It emits distributions,
scores them only against later observations, and permits promotion only through
calibration and skill gates. It must never emit trading instructions or claim a
point forecast as certainty.

## Canonical layout

- `manifest.edn`: actor metadata and constitutional gates
- `src/mitooshi/`: native CLJC methods, cells, and visualization builder
- `test/mitooshi/`: complete invariant suite
- `contracts/lexicons/`: canonical EDN lexicons
- `data/`: canonical observations, fixtures, scorecards, and trails
- `wire/`: JSON lexicons, identity/profile, input fixture, and visualization payload
- `docs/`: actor ADR and rendered visualization documents

EDN is canonical. JSON and HTML are external or rendered representations only.

Run `bb test`. Go, TinyGo, Python parity code, WASM build artifacts, root-level
shell runners, and duplicated JSON-LD metadata are deprecated.
