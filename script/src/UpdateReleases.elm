module UpdateReleases exposing
    ( PackageRelease
    , ReleaseRow
    , changelogFileForPackage
    , changelogUrl
    , computeGithubAnchor
    , deduplicateReleases
    , extractTag
    , fetchChangelogHeading
    , findChangelogHeading
    , formatReleaseTable
    , generateReleaseLines
    , maxReleases
    , parseReleaseNotes
    , parseRssItems
    , releaseNotesKey
    , rssFeedUrl
    , run
    , updateReadmeContent
    )

import BackendTask exposing (BackendTask)
import BackendTask.File
import BackendTask.Http
import Dict exposing (Dict)
import FatalError exposing (FatalError)
import Json.Decode as Decode
import Pages.Script as Script exposing (Script)


type alias PackageRelease =
    { name : String
    , version : String
    , docsUrl : String
    }


type alias ReleaseRow =
    { release : PackageRelease
    , changelogLink : String
    , highlight : String
    }



-- CONFIGURATION


maxReleases : Int
maxReleases =
    5


rssFeedUrl : String
rssFeedUrl =
    "https://releases.elm.dmy.fr/.rss?dillonkearns=*"


changelogFileForPackage : String -> String
changelogFileForPackage packageName =
    case packageName of
        "elm-pages" ->
            "CHANGELOG-ELM.md"

        "elm-graphql" ->
            "CHANGELOG-ELM-PACKAGE.md"

        _ ->
            "CHANGELOG.md"



-- RSS PARSING


parseRssItems : String -> List PackageRelease
parseRssItems xml =
    xml
        |> extractItemBlocks
        |> List.filterMap parseItem


extractItemBlocks : String -> List String
extractItemBlocks xml =
    xml
        |> String.split "<item>"
        |> List.drop 1
        |> List.filterMap
            (\block ->
                case String.split "</item>" block of
                    content :: _ ->
                        Just content

                    _ ->
                        Nothing
            )


parseItem : String -> Maybe PackageRelease
parseItem itemXml =
    Maybe.map2
        (\title link ->
            let
                parts =
                    String.split " " title

                fullName =
                    parts |> List.head |> Maybe.withDefault ""

                version =
                    parts
                        |> List.drop 1
                        |> List.head
                        |> Maybe.withDefault ""
                        |> String.toList
                        |> List.filter (\c -> Char.isDigit c || c == '.')
                        |> String.fromList

                name =
                    fullName
                        |> String.split "/"
                        |> List.drop 1
                        |> String.join "/"
            in
            { name = name
            , version = version
            , docsUrl = link
            }
        )
        (extractTag "title" itemXml)
        (extractTag "link" itemXml)


extractTag : String -> String -> Maybe String
extractTag tagName xml =
    case String.split ("<" ++ tagName ++ ">") xml of
        _ :: rest :: _ ->
            case String.split ("</" ++ tagName ++ ">") rest of
                content :: _ ->
                    Just (String.trim content)

                _ ->
                    Nothing

        _ ->
            Nothing



-- DEDUPLICATION


deduplicateReleases : List PackageRelease -> List PackageRelease
deduplicateReleases releases =
    releases
        |> List.foldl
            (\release ( seen, acc ) ->
                if List.member release.name seen then
                    ( seen, acc )

                else
                    ( release.name :: seen, release :: acc )
            )
            ( [], [] )
        |> Tuple.second
        |> List.reverse



-- RELEASE NOTES


releaseNotesKey : PackageRelease -> String
releaseNotesKey release =
    release.name ++ "@" ++ release.version


parseReleaseNotes : String -> Dict String String
parseReleaseNotes json =
    json
        |> Decode.decodeString (Decode.dict Decode.string)
        |> Result.withDefault Dict.empty



-- CHANGELOG


findChangelogHeading : String -> String -> Maybe String
findChangelogHeading version content =
    content
        |> String.lines
        |> List.filter (\line -> String.startsWith ("## [" ++ version ++ "]") line)
        |> List.head
        |> Maybe.map (String.dropLeft 3)


computeGithubAnchor : String -> String
computeGithubAnchor headingText =
    headingText
        |> String.toLower
        |> String.toList
        |> List.filter (\c -> Char.isAlphaNum c || c == '-' || c == ' ')
        |> String.fromList
        |> String.replace " " "-"


changelogUrl : PackageRelease -> Maybe String -> String
changelogUrl release maybeHeading =
    let
        file =
            changelogFileForPackage release.name

        baseUrl =
            "https://github.com/dillonkearns/"
                ++ release.name
                ++ "/blob/"
                ++ release.version
                ++ "/"
                ++ file
    in
    case maybeHeading of
        Just headingText ->
            baseUrl ++ "#" ++ computeGithubAnchor headingText

        Nothing ->
            baseUrl



-- FORMATTING


formatReleaseTable : List ReleaseRow -> String
formatReleaseTable rows =
    let
        header =
            "| Package | Highlights |\n| :------ | :--------- |"

        formatRow row =
            "| ["
                ++ row.release.name
                ++ " "
                ++ row.release.version
                ++ "]("
                ++ row.release.docsUrl
                ++ ") · [changelog]("
                ++ row.changelogLink
                ++ ") | "
                ++ row.highlight
                ++ " |"
    in
    case rows of
        [] ->
            header

        _ ->
            header ++ "\n" ++ String.join "\n" (List.map formatRow rows)



-- README


updateReadmeContent : String -> String -> String
updateReadmeContent newContent readme =
    let
        startMarker =
            "<!-- PACKAGE-RELEASES:START -->"

        endMarker =
            "<!-- PACKAGE-RELEASES:END -->"
    in
    case String.split startMarker readme of
        before :: rest :: _ ->
            case String.split endMarker (String.join startMarker (rest :: [])) of
                _ :: after ->
                    before ++ startMarker ++ "\n" ++ newContent ++ "\n" ++ endMarker ++ String.join endMarker after

                _ ->
                    readme

        _ ->
            readme



-- BACKEND TASKS


fetchChangelogHeading : PackageRelease -> BackendTask FatalError (Maybe String)
fetchChangelogHeading release =
    let
        file =
            changelogFileForPackage release.name

        url =
            "https://raw.githubusercontent.com/dillonkearns/"
                ++ release.name
                ++ "/"
                ++ release.version
                ++ "/"
                ++ file
    in
    BackendTask.Http.get url BackendTask.Http.expectString
        |> BackendTask.toResult
        |> BackendTask.map
            (\result ->
                case result of
                    Ok content ->
                        findChangelogHeading release.version content

                    Err _ ->
                        Nothing
            )


loadReleaseNotes : BackendTask FatalError (Dict String String)
loadReleaseNotes =
    BackendTask.File.rawFile "../release-notes.json"
        |> BackendTask.toResult
        |> BackendTask.map
            (\result ->
                case result of
                    Ok content ->
                        parseReleaseNotes content

                    Err _ ->
                        Dict.empty
            )


generateReleaseLines : BackendTask FatalError String
generateReleaseLines =
    BackendTask.map2 Tuple.pair
        (BackendTask.Http.get rssFeedUrl BackendTask.Http.expectString
            |> BackendTask.allowFatal
            |> BackendTask.map parseRssItems
            |> BackendTask.map deduplicateReleases
            |> BackendTask.map (List.take maxReleases)
        )
        loadReleaseNotes
        |> BackendTask.andThen
            (\( releases, notes ) ->
                releases
                    |> List.map
                        (\release ->
                            fetchChangelogHeading release
                                |> BackendTask.map
                                    (\maybeHeading ->
                                        { release = release
                                        , changelogLink = changelogUrl release maybeHeading
                                        , highlight =
                                            Dict.get (releaseNotesKey release) notes
                                                |> Maybe.withDefault ""
                                        }
                                    )
                        )
                    |> BackendTask.combine
                    |> BackendTask.map formatReleaseTable
            )



-- SCRIPT


run : Script
run =
    Script.withoutCliOptions
        (generateReleaseLines
            |> BackendTask.andThen
                (\releaseLines ->
                    BackendTask.File.rawFile "../README.md"
                        |> BackendTask.allowFatal
                        |> BackendTask.andThen
                            (\readme ->
                                Script.writeFile
                                    { path = "../README.md"
                                    , body = updateReadmeContent releaseLines readme
                                    }
                                    |> BackendTask.allowFatal
                            )
                )
        )
