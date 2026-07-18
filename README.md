# mitooshi 見通し — probabilistic forecasting observatory

Mitooshi produces leak-free distribution forecasts for resilience planning. It
records observations with information boundaries, evaluates Gaussian,
quantile, categorical, and ensemble forecasts with proper scoring rules, and
updates models only through explicit calibration and promotion gates.

Core invariants:

- forecasts are distributions, never asserted points;
- use is resilience analysis, never trading or positioning;
- observations must occur strictly after forecast information boundaries;
- calibration and skill must beat an appropriate baseline;
- live ingest, publication, training, and promotion remain operator gated;
- no server-held signing key is permitted.

## Layout

- `src/mitooshi/methods/`: analysis, bridges, forecasting, scoring, persistence,
  promotion, and synthesis
- `src/mitooshi/cells/`: five native CLJC state machines
- `src/mitooshi/viz/`: forecast visualization payload builder
- `test/mitooshi/`: standalone tests
- `contracts/lexicons/`: canonical EDN contracts
- `data/`: canonical graph, fixtures, bridge samples, and persisted evidence
- `wire/`: JSON exchange formats and generated visualization payload
- `docs/viz/`: rendered/template HTML

Run the full suite with `bb test`. Canonical metadata and contracts are EDN;
JSON-LD actor metadata is intentionally removed.
