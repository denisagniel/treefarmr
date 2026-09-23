# Session Logs — HISTORICAL, FROZEN 2026-09-01

These files are a **historical archive**. Do not add to them; do not append to them.

The single session record is `session_notes/YYYY-MM-DD.md` — see
`.claude/rules/session-notes.md`. The rule that mandated this directory is retired; see
`.claude/rules/session-logging.md` for why.

## Two things to know when reading these

**1. Stray compaction markers were removed 2026-09-01.** A `PreCompact` hook used to append
`**Context compaction (...) at HH:MM**` to whichever file here had the newest mtime — never
the session that was actually running. **393 such markers across 124 files in 14 projects**
were stripped. If you diff these files against an old commit, those deletions are that
cleanup, not lost content. Three markers were deliberately left in place because genuine
prose sits next to them; they are listed in the plan below.

**2. Content here may be interleaved across sessions.** Because the hook appended to the
mtime-latest file rather than the current one, a log can contain traces of work it never
covered. Treat cross-references with suspicion; `git log` is the reliable record of what
changed when.

Full diagnosis:
`missing-data-did/quality_reports/plans/2026-09-01_session-logging-architecture-remediation.md`
