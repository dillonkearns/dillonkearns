module UpdateReleasesTest exposing (all)

import Expect
import Json.Encode as Encode
import Test exposing (Test, describe, test)
import Test.BackendTask as BackendTaskTest
import UpdateReleases


sampleRss : String
sampleRss =
    """<?xml version="1.0" encoding="utf-8"?>
<rss version="2.0">
<channel>
<title>Elm packages releases of dillonkearns/*</title>
<item>
<title>dillonkearns/elm-snapshot 1.1.1</title>
<link>https://package.elm-lang.org/packages/dillonkearns/elm-snapshot/1.1.1/</link>
<description><![CDATA[Snapshot testing framework]]></description>
<comments>https://github.com/dillonkearns/elm-snapshot/tree/1.1.1</comments>
</item>
<item>
<title>dillonkearns/elm-pages 12.1.0</title>
<link>https://package.elm-lang.org/packages/dillonkearns/elm-pages/12.1.0/</link>
<description><![CDATA[Hybrid Elm framework]]></description>
<comments>https://github.com/dillonkearns/elm-pages/tree/12.1.0</comments>
</item>
<item>
<title>dillonkearns/elm-cli-options-parser 5.0.1</title>
<link>https://package.elm-lang.org/packages/dillonkearns/elm-cli-options-parser/5.0.1/</link>
<description><![CDATA[Type-safe CLI options parser]]></description>
</item>
</channel>
</rss>"""


all : Test
all =
    describe "UpdateReleases"
        [ parseRssItemsTests
        , extractTagTests
        , changelogFileForPackageTests
        , findChangelogHeadingTests
        , computeGithubAnchorTests
        , changelogUrlTests
        , formatReleaseLineTests
        , updateReadmeContentTests
        , deduplicateTests
        , fullScriptTests
        ]


parseRssItemsTests : Test
parseRssItemsTests =
    describe "parseRssItems"
        [ test "parses package releases from RSS XML" <|
            \() ->
                UpdateReleases.parseRssItems sampleRss
                    |> Expect.equal
                        [ { name = "elm-snapshot"
                          , version = "1.1.1"
                          , docsUrl = "https://package.elm-lang.org/packages/dillonkearns/elm-snapshot/1.1.1/"
                          }
                        , { name = "elm-pages"
                          , version = "12.1.0"
                          , docsUrl = "https://package.elm-lang.org/packages/dillonkearns/elm-pages/12.1.0/"
                          }
                        , { name = "elm-cli-options-parser"
                          , version = "5.0.1"
                          , docsUrl = "https://package.elm-lang.org/packages/dillonkearns/elm-cli-options-parser/5.0.1/"
                          }
                        ]
        , test "returns empty list for invalid XML" <|
            \() ->
                UpdateReleases.parseRssItems "not xml"
                    |> Expect.equal []
        , test "returns empty list for XML with no items" <|
            \() ->
                UpdateReleases.parseRssItems "<rss><channel></channel></rss>"
                    |> Expect.equal []
        ]


extractTagTests : Test
extractTagTests =
    describe "extractTag"
        [ test "extracts content from a simple tag" <|
            \() ->
                UpdateReleases.extractTag "title" "<title>Hello World</title>"
                    |> Expect.equal (Just "Hello World")
        , test "returns Nothing when tag is missing" <|
            \() ->
                UpdateReleases.extractTag "title" "<link>url</link>"
                    |> Expect.equal Nothing
        , test "trims whitespace" <|
            \() ->
                UpdateReleases.extractTag "title" "<title>  spaced  </title>"
                    |> Expect.equal (Just "spaced")
        ]


changelogFileForPackageTests : Test
changelogFileForPackageTests =
    describe "changelogFileForPackage"
        [ test "elm-pages uses CHANGELOG-ELM.md" <|
            \() ->
                UpdateReleases.changelogFileForPackage "elm-pages"
                    |> Expect.equal "CHANGELOG-ELM.md"
        , test "elm-graphql uses CHANGELOG-ELM-PACKAGE.md" <|
            \() ->
                UpdateReleases.changelogFileForPackage "elm-graphql"
                    |> Expect.equal "CHANGELOG-ELM-PACKAGE.md"
        , test "other packages use CHANGELOG.md" <|
            \() ->
                UpdateReleases.changelogFileForPackage "elm-markdown"
                    |> Expect.equal "CHANGELOG.md"
        , test "unknown packages use CHANGELOG.md" <|
            \() ->
                UpdateReleases.changelogFileForPackage "some-new-package"
                    |> Expect.equal "CHANGELOG.md"
        ]


findChangelogHeadingTests : Test
findChangelogHeadingTests =
    describe "findChangelogHeading"
        [ test "finds heading with date" <|
            \() ->
                UpdateReleases.findChangelogHeading "12.1.0"
                    "## [Unreleased]\n\n## [12.1.0] - 2026-03-14\n\nSome changes"
                    |> Expect.equal (Just "[12.1.0] - 2026-03-14")
        , test "finds heading without date" <|
            \() ->
                UpdateReleases.findChangelogHeading "5.0.1"
                    "## [5.0.1]\n\nSome changes"
                    |> Expect.equal (Just "[5.0.1]")
        , test "returns Nothing when version not found" <|
            \() ->
                UpdateReleases.findChangelogHeading "1.0.0"
                    "## [2.0.0] - 2026-01-01\n\nChanges"
                    |> Expect.equal Nothing
        , test "matches exact version, not prefix" <|
            \() ->
                UpdateReleases.findChangelogHeading "1.0.0"
                    "## [1.0.0-beta] - 2026-01-01\n## [1.0.0] - 2026-02-01"
                    |> Expect.equal (Just "[1.0.0] - 2026-02-01")
        ]


computeGithubAnchorTests : Test
computeGithubAnchorTests =
    describe "computeGithubAnchor"
        [ test "heading with date" <|
            \() ->
                UpdateReleases.computeGithubAnchor "[12.1.0] - 2026-03-14"
                    |> Expect.equal "1210---2026-03-14"
        , test "heading without date" <|
            \() ->
                UpdateReleases.computeGithubAnchor "[5.0.1]"
                    |> Expect.equal "501"
        , test "heading with single digit versions" <|
            \() ->
                UpdateReleases.computeGithubAnchor "[1.0.0] - 2024-01-02"
                    |> Expect.equal "100---2024-01-02"
        ]


changelogUrlTests : Test
changelogUrlTests =
    describe "changelogUrl"
        [ test "with heading anchor" <|
            \() ->
                let
                    release =
                        { name = "elm-pages"
                        , version = "12.1.0"
                        , docsUrl = "https://package.elm-lang.org/packages/dillonkearns/elm-pages/12.1.0/"
                        }
                in
                UpdateReleases.changelogUrl release (Just "[12.1.0] - 2026-03-14")
                    |> Expect.equal
                        "https://github.com/dillonkearns/elm-pages/blob/12.1.0/CHANGELOG-ELM.md#1210---2026-03-14"
        , test "without heading falls back to file link" <|
            \() ->
                let
                    release =
                        { name = "elm-snapshot"
                        , version = "1.1.1"
                        , docsUrl = "https://package.elm-lang.org/packages/dillonkearns/elm-snapshot/1.1.1/"
                        }
                in
                UpdateReleases.changelogUrl release Nothing
                    |> Expect.equal
                        "https://github.com/dillonkearns/elm-snapshot/blob/1.1.1/CHANGELOG.md"
        , test "elm-graphql uses correct changelog file" <|
            \() ->
                let
                    release =
                        { name = "elm-graphql"
                        , version = "5.0.12"
                        , docsUrl = "https://package.elm-lang.org/packages/dillonkearns/elm-graphql/5.0.12/"
                        }
                in
                UpdateReleases.changelogUrl release (Just "[5.0.12] - 2023-08-29")
                    |> Expect.equal
                        "https://github.com/dillonkearns/elm-graphql/blob/5.0.12/CHANGELOG-ELM-PACKAGE.md#5012---2023-08-29"
        ]


formatReleaseLineTests : Test
formatReleaseLineTests =
    describe "formatReleaseLine"
        [ test "formats a release line with changelog link" <|
            \() ->
                let
                    release =
                        { name = "elm-pages"
                        , version = "12.1.0"
                        , docsUrl = "https://package.elm-lang.org/packages/dillonkearns/elm-pages/12.1.0/"
                        }
                in
                UpdateReleases.formatReleaseLine release
                    "https://github.com/dillonkearns/elm-pages/blob/12.1.0/CHANGELOG-ELM.md#1210---2026-03-14"
                    |> Expect.equal
                        "- [elm-pages 12.1.0](https://package.elm-lang.org/packages/dillonkearns/elm-pages/12.1.0/) · [changelog](https://github.com/dillonkearns/elm-pages/blob/12.1.0/CHANGELOG-ELM.md#1210---2026-03-14)"
        ]


updateReadmeContentTests : Test
updateReadmeContentTests =
    describe "updateReadmeContent"
        [ test "replaces content between markers" <|
            \() ->
                let
                    readme =
                        "before\n<!-- PACKAGE-RELEASES:START -->\nold content\n<!-- PACKAGE-RELEASES:END -->\nafter"

                    newContent =
                        "- line 1\n- line 2"
                in
                UpdateReleases.updateReadmeContent newContent readme
                    |> Expect.equal
                        "before\n<!-- PACKAGE-RELEASES:START -->\n- line 1\n- line 2\n<!-- PACKAGE-RELEASES:END -->\nafter"
        , test "handles empty existing content between markers" <|
            \() ->
                let
                    readme =
                        "before\n<!-- PACKAGE-RELEASES:START -->\n<!-- PACKAGE-RELEASES:END -->\nafter"
                in
                UpdateReleases.updateReadmeContent "- new" readme
                    |> Expect.equal
                        "before\n<!-- PACKAGE-RELEASES:START -->\n- new\n<!-- PACKAGE-RELEASES:END -->\nafter"
        , test "preserves readme when markers are missing" <|
            \() ->
                UpdateReleases.updateReadmeContent "new content" "no markers here"
                    |> Expect.equal "no markers here"
        ]


deduplicateTests : Test
deduplicateTests =
    describe "deduplicateReleases"
        [ test "keeps only the first (latest) version of each package" <|
            \() ->
                let
                    releases =
                        [ { name = "elm-pages", version = "12.1.0", docsUrl = "https://example.com/12.1.0/" }
                        , { name = "elm-cli-options-parser", version = "5.0.1", docsUrl = "https://example.com/5.0.1/" }
                        , { name = "elm-cli-options-parser", version = "5.0.0", docsUrl = "https://example.com/5.0.0/" }
                        , { name = "elm-pages", version = "12.0.2", docsUrl = "https://example.com/12.0.2/" }
                        , { name = "elm-ts-json", version = "2.1.2", docsUrl = "https://example.com/2.1.2/" }
                        ]
                in
                UpdateReleases.deduplicateReleases releases
                    |> Expect.equal
                        [ { name = "elm-pages", version = "12.1.0", docsUrl = "https://example.com/12.1.0/" }
                        , { name = "elm-cli-options-parser", version = "5.0.1", docsUrl = "https://example.com/5.0.1/" }
                        , { name = "elm-ts-json", version = "2.1.2", docsUrl = "https://example.com/2.1.2/" }
                        ]
        , test "preserves all releases when no duplicates" <|
            \() ->
                let
                    releases =
                        [ { name = "elm-pages", version = "12.1.0", docsUrl = "a" }
                        , { name = "elm-snapshot", version = "1.1.1", docsUrl = "b" }
                        ]
                in
                UpdateReleases.deduplicateReleases releases
                    |> Expect.equal releases
        , test "handles empty list" <|
            \() ->
                UpdateReleases.deduplicateReleases []
                    |> Expect.equal []
        , test "script deduplicates in full pipeline" <|
            \() ->
                let
                    rss =
                        """<?xml version="1.0"?>
<rss><channel>
<item>
<title>dillonkearns/elm-pages 12.1.0</title>
<link>https://package.elm-lang.org/packages/dillonkearns/elm-pages/12.1.0/</link>
</item>
<item>
<title>dillonkearns/elm-cli-options-parser 5.0.1</title>
<link>https://package.elm-lang.org/packages/dillonkearns/elm-cli-options-parser/5.0.1/</link>
</item>
<item>
<title>dillonkearns/elm-cli-options-parser 5.0.0</title>
<link>https://package.elm-lang.org/packages/dillonkearns/elm-cli-options-parser/5.0.0/</link>
</item>
</channel></rss>"""

                    readmeTemplate =
                        "<!-- PACKAGE-RELEASES:START -->\n<!-- PACKAGE-RELEASES:END -->"

                    expectedReadme =
                        "<!-- PACKAGE-RELEASES:START -->\n"
                            ++ "- [elm-pages 12.1.0](https://package.elm-lang.org/packages/dillonkearns/elm-pages/12.1.0/) · [changelog](https://github.com/dillonkearns/elm-pages/blob/12.1.0/CHANGELOG-ELM.md#1210---2026-03-14)\n"
                            ++ "- [elm-cli-options-parser 5.0.1](https://package.elm-lang.org/packages/dillonkearns/elm-cli-options-parser/5.0.1/) · [changelog](https://github.com/dillonkearns/elm-cli-options-parser/blob/5.0.1/CHANGELOG.md#501)"
                            ++ "\n<!-- PACKAGE-RELEASES:END -->"
                in
                UpdateReleases.run
                    |> BackendTaskTest.fromScriptWith
                        (BackendTaskTest.init
                            |> BackendTaskTest.withFile "README.md" readmeTemplate
                        )
                        []
                    |> BackendTaskTest.simulateHttpGet
                        UpdateReleases.rssFeedUrl
                        (Encode.string rss)
                    |> BackendTaskTest.simulateHttpGet
                        "https://raw.githubusercontent.com/dillonkearns/elm-pages/12.1.0/CHANGELOG-ELM.md"
                        (Encode.string "## [12.1.0] - 2026-03-14\n\nChanges")
                    |> BackendTaskTest.simulateHttpGet
                        "https://raw.githubusercontent.com/dillonkearns/elm-cli-options-parser/5.0.1/CHANGELOG.md"
                        (Encode.string "## [5.0.1]\n\nChanges")
                    |> BackendTaskTest.ensureFile "README.md" expectedReadme
                    |> BackendTaskTest.expectSuccess
        ]


fullScriptTests : Test
fullScriptTests =
    describe "full script"
        [ test "fetches RSS, changelogs, and updates README" <|
            \() ->
                let
                    rss =
                        """<?xml version="1.0"?>
<rss><channel>
<item>
<title>dillonkearns/elm-pages 12.1.0</title>
<link>https://package.elm-lang.org/packages/dillonkearns/elm-pages/12.1.0/</link>
</item>
<item>
<title>dillonkearns/elm-snapshot 1.1.1</title>
<link>https://package.elm-lang.org/packages/dillonkearns/elm-snapshot/1.1.1/</link>
</item>
</channel></rss>"""

                    readmeTemplate =
                        "# Hi\n<!-- PACKAGE-RELEASES:START -->\n<!-- PACKAGE-RELEASES:END -->\n"

                    expectedReadme =
                        "# Hi\n<!-- PACKAGE-RELEASES:START -->\n"
                            ++ "- [elm-pages 12.1.0](https://package.elm-lang.org/packages/dillonkearns/elm-pages/12.1.0/) · [changelog](https://github.com/dillonkearns/elm-pages/blob/12.1.0/CHANGELOG-ELM.md#1210---2026-03-14)\n"
                            ++ "- [elm-snapshot 1.1.1](https://package.elm-lang.org/packages/dillonkearns/elm-snapshot/1.1.1/) · [changelog](https://github.com/dillonkearns/elm-snapshot/blob/1.1.1/CHANGELOG.md#111)"
                            ++ "\n<!-- PACKAGE-RELEASES:END -->\n"
                in
                UpdateReleases.run
                    |> BackendTaskTest.fromScriptWith
                        (BackendTaskTest.init
                            |> BackendTaskTest.withFile "README.md" readmeTemplate
                        )
                        []
                    |> BackendTaskTest.simulateHttpGet
                        UpdateReleases.rssFeedUrl
                        (Encode.string rss)
                    |> BackendTaskTest.simulateHttpGet
                        "https://raw.githubusercontent.com/dillonkearns/elm-pages/12.1.0/CHANGELOG-ELM.md"
                        (Encode.string "## [12.1.0] - 2026-03-14\n\nChanges here")
                    |> BackendTaskTest.simulateHttpGet
                        "https://raw.githubusercontent.com/dillonkearns/elm-snapshot/1.1.1/CHANGELOG.md"
                        (Encode.string "## [1.1.1]\n\nFixes")
                    |> BackendTaskTest.ensureFile "README.md" expectedReadme
                    |> BackendTaskTest.expectSuccess
        , test "handles missing changelog gracefully" <|
            \() ->
                let
                    rss =
                        """<?xml version="1.0"?>
<rss><channel>
<item>
<title>dillonkearns/elm-oembed 1.1.0</title>
<link>https://package.elm-lang.org/packages/dillonkearns/elm-oembed/1.1.0/</link>
</item>
</channel></rss>"""

                    readmeTemplate =
                        "<!-- PACKAGE-RELEASES:START -->\n<!-- PACKAGE-RELEASES:END -->"

                    expectedReadme =
                        "<!-- PACKAGE-RELEASES:START -->\n"
                            ++ "- [elm-oembed 1.1.0](https://package.elm-lang.org/packages/dillonkearns/elm-oembed/1.1.0/) · [changelog](https://github.com/dillonkearns/elm-oembed/blob/1.1.0/CHANGELOG.md)"
                            ++ "\n<!-- PACKAGE-RELEASES:END -->"
                in
                UpdateReleases.run
                    |> BackendTaskTest.fromScriptWith
                        (BackendTaskTest.init
                            |> BackendTaskTest.withFile "README.md" readmeTemplate
                        )
                        []
                    |> BackendTaskTest.simulateHttpGet
                        UpdateReleases.rssFeedUrl
                        (Encode.string rss)
                    |> BackendTaskTest.simulateHttp
                        { method = "GET"
                        , url = "https://raw.githubusercontent.com/dillonkearns/elm-oembed/1.1.0/CHANGELOG.md"
                        }
                        { statusCode = 404
                        , statusText = "Not Found"
                        , headers = []
                        , body = Encode.string ""
                        }
                    |> BackendTaskTest.ensureFile "README.md" expectedReadme
                    |> BackendTaskTest.expectSuccess
        , test "changelog heading without date links without anchor" <|
            \() ->
                let
                    rss =
                        """<?xml version="1.0"?>
<rss><channel>
<item>
<title>dillonkearns/elm-cli-options-parser 5.0.1</title>
<link>https://package.elm-lang.org/packages/dillonkearns/elm-cli-options-parser/5.0.1/</link>
</item>
</channel></rss>"""

                    readmeTemplate =
                        "<!-- PACKAGE-RELEASES:START -->\n<!-- PACKAGE-RELEASES:END -->"

                    expectedReadme =
                        "<!-- PACKAGE-RELEASES:START -->\n"
                            ++ "- [elm-cli-options-parser 5.0.1](https://package.elm-lang.org/packages/dillonkearns/elm-cli-options-parser/5.0.1/) · [changelog](https://github.com/dillonkearns/elm-cli-options-parser/blob/5.0.1/CHANGELOG.md#501)"
                            ++ "\n<!-- PACKAGE-RELEASES:END -->"
                in
                UpdateReleases.run
                    |> BackendTaskTest.fromScriptWith
                        (BackendTaskTest.init
                            |> BackendTaskTest.withFile "README.md" readmeTemplate
                        )
                        []
                    |> BackendTaskTest.simulateHttpGet
                        UpdateReleases.rssFeedUrl
                        (Encode.string rss)
                    |> BackendTaskTest.simulateHttpGet
                        "https://raw.githubusercontent.com/dillonkearns/elm-cli-options-parser/5.0.1/CHANGELOG.md"
                        (Encode.string "## [5.0.1]\n\n### Changed\n\n- Something")
                    |> BackendTaskTest.ensureFile "README.md" expectedReadme
                    |> BackendTaskTest.expectSuccess
        ]
