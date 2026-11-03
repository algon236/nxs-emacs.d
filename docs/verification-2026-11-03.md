# Verification on 3 November 2026

The active macOS Emacs 32.0.50 daemon was shut down normally and restarted by
its existing launchd service. Its PID changed and `init-file-had-error` was
nil. `emacs-init-time` reported 1.015068 seconds. The previous long-running
session had reported 5.531407 seconds; these are separate, uncontrolled
startup measurements, not an isolated benchmark of the changes.

Two graphical frames and all previously visited file buffers were restored.
The modified document was saved before shutdown. PDF Tools opened the existing
PDF buffer, global auto-revert was enabled, native clipboard reads matched
pbpaste, and reopening PerInf installed its 10-second clock-display timer.

## Performance and correctness changes

- PerInf display timers run every 10 seconds and do not parse project files
  while their buffer is hidden. Elapsed time still comes from timestamps.
  The separate inactivity check retains its 60-second interval. Redisplaying
  a previously hidden clock can take until the next display timer tick.
- Internal PerInf Org readers disable agenda-menu rebuilding and visual
  startup only while initializing their temporary buffers.
- Frame titles and mode lines share a five-second, buffer-local project-label
  cache, including negative results and file-access failures. Remote project
  labels are omitted, while explicit project commands remain available.
- Graphical macOS frames use the native clipboard; terminal frames retain
  pbcopy/pbpaste.
- Keyword highlighting uses a named programming-mode hook rather than an idle
  callback that did not retain its originating buffer.

Ten old PerInf display updates took 1.200570 seconds in the running session.
Ten hidden-buffer calls after the change took 0.000003 seconds because they
skipped the work. Ten builds of the clock text with the updated readers took
0.628359 seconds; this latter measurement omits display comparison and is not
an identical workload. Twenty clipboard reads took 0.231479 seconds using
pbpaste and 0.000486 seconds using the native API, with equal content.
These small benchmarks do not measure overall interactive editing speed.

## Other local changes included in this update

- PerInf meeting duration corrections and additional-time tracking, agenda
  document attachments, macOS recording import and project statistics with
  versioned snapshots.
- Task associations can refer to existing files as well as open buffers;
  meeting scans skip Emacs lock files, and imported audio copies are checked.
- Sleep report version 21, including table recalculation, compilation logging,
  PDF refresh and measurement summary rows.
- Existing local font-selection module, Dired/Ibuffer preview bindings,
  start-page updates and scoped iCloud Trash fallback.

These features predate the performance changes. Restart verification confirms
that they load together, but is not an exhaustive test of recording, export,
file deletion or every interactive workflow.

## Checks and remaining warnings

The 73 local Lisp files passed the parentheses check, and the ten focused ERT
checks passed in a separate clean Emacs process. PerInf metadata, task and
meeting reads were compared before and after the live update and were equal.
The ERT checks are now repository-relative in `tests/performance-tests.el`;
clipboard-specific tests are skipped outside macOS.

Startup still reports an unsupported `en_DK.UTF-8` locale and falls back to
`en_US.UTF-8`, and the installed PDF Tools package uses obsolete `when-let`
macros. A bookmark auto-save recovery notice also appeared; its saved data
was not discarded or overwritten as part of this work. None of these
prevented startup. External packages were not upgraded.

Local session recovery files, backups and measurements remain outside the
repository. Generated PerInf state, autoloads, platform-specific tree-sitter
libraries and dated backups are ignored by Git.
