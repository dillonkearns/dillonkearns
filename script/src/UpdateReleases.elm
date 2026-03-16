module UpdateReleases exposing
    ( PackageRelease
    , changelogFileForPackage
    , changelogUrl
    , deduplicateReleases
    , computeGithubAnchor
    , extractTag
    , fetchChangelogHeading
    , findChangelogHeading
    , formatReleaseLine
    , generateReleaseLines
    , maxReleases
    , parseRssItems
    , rssFeedUrl
    , run
    , updateReadmeContent
    )

import BackendTask exposing (BackendTask)
import BackendTask.File
import BackendTask.Http
import FatalError exposing (FatalError)
import Json.Encode as Encode
import Pages.Script as Script exposing (Script)


type alias PackageRelease =
    { name : String
    , version : String
    , docsUrl : String
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
                    parts |> List.drop 1 |> List.head |> Maybe.withDefault ""

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


formatReleaseLine : PackageRelease -> String -> String
formatReleaseLine release changelogLink =
    "- ["
        ++ release.name
        ++ " "
        ++ release.version
        ++ "]("
        ++ release.docsUrl
        ++ ") · [changelog]("
        ++ changelogLink
        ++ ")"



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


generateReleaseLines : BackendTask FatalError String
generateReleaseLines =
    BackendTask.Http.get rssFeedUrl BackendTask.Http.expectString
        |> BackendTask.allowFatal
        |> BackendTask.map parseRssItems
        |> BackendTask.map deduplicateReleases
        |> BackendTask.map (List.take maxReleases)
        |> BackendTask.andThen
            (\releases ->
                releases
                    |> List.map
                        (\release ->
                            fetchChangelogHeading release
                                |> BackendTask.map
                                    (\maybeHeading ->
                                        formatReleaseLine release (changelogUrl release maybeHeading)
                                    )
                        )
                    |> BackendTask.combine
                    |> BackendTask.map (String.join "\n")
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
