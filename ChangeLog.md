# Changelog for status-notifier-item

## 0.3.2.5 - 2026-02-12
- Test suite: start an isolated `dbus-daemon` using a session config shipped
  alongside the `dbus-daemon` executable when available (fixes Nix sandbox
  builds where `/etc/dbus-1/session.conf` is absent).

## 0.3.2.4 - 2026-02-12
- Host: deduplicate items that re-register under a different bus name after a
  watcher restart (e.g. unique name vs well-known name), preventing duplicate
  tray icons.
- Add a regression test for watcher-restart re-registration deduplication.

## 0.3.2.3 - 2026-02-11
- Downgrade unknown-sender update logs from WARNING to DEBUG to avoid noisy
  false alarms for routine tray signals.
- Treat UnknownMethod property refresh failures as expected optional-property
  misses (same as InvalidArgs), logging them at DEBUG unless no updater
  succeeded.
- Add host tests for property update failure log-level classification.

## 0.3.2.2 - 2026-02-11
- Fix watcher registration ownership checks by requiring explicit service-name
  registrations to be initiated by the current owner.
- Fix watcher duplicate handling by coalescing path-first and name-first
  registrations from the same sender/path pair.
- Add an isolated DBus integration test suite covering watcher/host behavior
  and regression tests for registration ownership and deduplication.

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
