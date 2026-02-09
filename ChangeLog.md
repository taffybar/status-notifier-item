# Changelog for status-notifier-item

## 0.3.2.1 - 2026-02-09
- Add name-owner resolution fallback for signal sender identification, fixing
  noisy errors when items register under well-known bus names.
- Downgrade "Failed to identify sender" log from ERROR to DEBUG when the item
  simply does not implement the getId method.
- Downgrade "Property update failures" log from ERROR to DEBUG when the failures
  are InvalidArgs (item does not support optional properties like OverlayIcon).

## 0.3.2.0 - 2026-02-05
- Report full `bus/path` identifiers for non-default SNI object paths so hosts
  can resolve Ayatana items reliably.
- Accept `bus/path` identifiers in watcher/host lookups for compatibility with
  pathful registrations.
