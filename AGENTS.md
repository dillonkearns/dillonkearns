# Agents Guide

## Testing Practices

### Red-First TDD

Always follow the Red-Green-Refactor cycle:

1. **RED**: Write a failing test that describes the desired behavior. Run it and confirm it fails for the right reason.
2. **GREEN**: Write the minimum code to make the test pass.
3. **REFACTOR**: Clean up the code while keeping tests green.

Never write production code without a failing test first. The failing test proves the test is actually exercising the code path you think it is.

### Testing with `Test.BackendTask`

This project uses `elm-pages` scripts tested with the `Test.BackendTask` module. This allows pure, deterministic testing of scripts that perform HTTP requests, file I/O, and shell commands — without any actual side effects.

#### Key patterns

**Starting a test from a `BackendTask`:**

```elm
myBackendTask
    |> BackendTaskTest.fromBackendTask
    |> BackendTaskTest.expectSuccess
```

**Starting a test from a `Script` (with CLI args and virtual filesystem):**

```elm
myScript
    |> BackendTaskTest.fromScriptWith
        (BackendTaskTest.init
            |> BackendTaskTest.withFile "README.md" "initial content"
        )
        [] -- CLI args
    |> BackendTaskTest.expectSuccess
```

**Simulating HTTP responses:**

```elm
-- For BackendTask.Http.getJson or BackendTask.Http.get with expectString
|> BackendTaskTest.simulateHttpGet url (Encode.string "response body")

-- For error responses (e.g. 404)
|> BackendTaskTest.simulateHttp
    { method = "GET", url = "https://example.com/missing" }
    { statusCode = 404, statusText = "Not Found", headers = [], body = Encode.string "" }
```

**Asserting on outputs and files:**

```elm
|> BackendTaskTest.ensureStdout [ "expected log message" ]
|> BackendTaskTest.ensureFile "output.txt" "expected content"
```

#### What gets simulated automatically

- File reads/writes (`BackendTask.File`, `Script.writeFile`)
- Logging (`Script.log`)
- Environment variables (`BackendTask.Env`)
- Glob patterns (`BackendTask.Glob`)
- Time (`BackendTask.Time.now`)

#### What you must simulate explicitly

- HTTP requests → `simulateHttpGet`, `simulateHttp`, `simulateHttpError`
- Shell commands → `simulateCommand`
- Custom ports → `simulateCustom`

### Test Structure

- **Pure function tests**: Test parsing, formatting, and transformation logic directly with `Expect.equal`. These are fast and don't need `Test.BackendTask`.
- **BackendTask integration tests**: Test the full script pipeline with simulated HTTP responses and virtual filesystem. These verify the script's I/O behavior end-to-end.

### Running Tests

```bash
cd script && elm-test
```

## Project Structure

```
script/
├── elm.json              # Elm dependencies (application type)
├── package.json          # npm dependencies (elm-pages)
├── src/
│   └── UpdateReleases.elm  # The script
└── tests/
    └── UpdateReleasesTest.elm  # Tests
```

## Script Architecture

The `UpdateReleases.elm` script:

1. Fetches the elm-greenwood RSS feed for `dillonkearns/*` packages
2. Parses RSS XML to extract package releases (name, version, docs URL)
3. Deduplicates releases (keeps only the latest version per package)
4. For each release, fetches the changelog file at the git tag
5. Finds the `## [VERSION]` heading and computes the GitHub anchor
6. Generates markdown lines with docs + changelog links
7. Updates README.md between the `<!-- PACKAGE-RELEASES:START/END -->` markers

### Design principles

- **Separate pure logic from effects**: Parsing, formatting, and deduplication are pure functions testable with standard elm-test. HTTP fetching and file I/O use `BackendTask` and are tested with `Test.BackendTask`.
- **Graceful degradation**: If a changelog file doesn't exist or lacks a version heading, the link falls back to the file without an anchor.
- **Deterministic output**: Given the same RSS feed and changelog contents, the script always produces the same README content.
