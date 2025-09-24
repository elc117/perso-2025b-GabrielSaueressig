module Frontend exposing (main)

import Browser
import Dict
import Html exposing (Html, div, input, label, text, img, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import String


-- MODELO

-- Tipo unificado para filmes
type alias Movie =
    { title : String
    , releaseDate : String
    , voteAverage : Float
    , genres : List String
    , poster : String
    , imdbId : Maybe String  -- Para identificar favoritos
    }

-- Função para gerar ID único quando IMDB ID não disponível
getMovieId : Movie -> String
getMovieId movie =
    case movie.imdbId of
        Just id -> id
        Nothing -> movie.title ++ "_" ++ movie.releaseDate

type alias Model =
    { genresSelected : List String
    , searchTitle : String
    , movies : List Movie
    , favorites : Dict.Dict String Movie
    , recommended : List Movie
    , status : String
    , modalOpen : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { genresSelected = []
      , searchTitle = ""
      , movies = []
      , favorites = Dict.empty
      , recommended = []
      , status = ""
      , modalOpen = True
      }
    , Cmd.none
    )


-- MENSAGENS

type Msg
    = ToggleGenre String
    | UpdateSearch String
    | GotMovies (Result Http.Error (List Movie))
    | ToggleFavorite Movie
    | SendAll
    | GotRecommended (Result Http.Error (List Movie))
    | CloseModal
    | OpenModal


-- DECODERS

-- Decoder unificado para filmes
movieDecoder : Decoder Movie
movieDecoder =
    Decode.map6 Movie
        (Decode.field "title" Decode.string)
        (Decode.field "releaseDate" Decode.string)
        (Decode.field "voteAverage" Decode.float)
        (Decode.field "genres" (Decode.list Decode.string))
        (Decode.field "poster" Decode.string)
        (Decode.maybe (Decode.field "imdbId" Decode.string))

moviesDecoder : Decoder (List Movie)
moviesDecoder =
    Decode.list movieDecoder


-- GÊNEROS POSSÍVEIS

genres : List String
genres =
    [ "Ação", "Aventura", "Animação", "Comédia", "Crime", "Documentário"
    , "Drama", "Família", "Fantasia", "História", "Terror", "Música"
    , "Mistério", "Romance", "Ficção Científica", "Thriller", "Guerra", "Western"
    ]


translateGenre : String -> String
translateGenre genre =
    case genre of
        "Ação" -> "Action"
        "Aventura" -> "Adventure"
        "Animação" -> "Animation"
        "Comédia" -> "Comedy"
        "Crime" -> "Crime"
        "Documentário" -> "Documentary"
        "Drama" -> "Drama"
        "Família" -> "Family"
        "Fantasia" -> "Fantasy"
        "História" -> "History"
        "Terror" -> "Horror"
        "Música" -> "Music"
        "Mistério" -> "Mystery"
        "Romance" -> "Romance"
        "Ficção Científica" -> "Science Fiction"
        "Thriller" -> "Thriller"
        "Guerra" -> "War"
        "Western" -> "Western"
        _ -> genre


-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        ToggleGenre g ->
            let
                already = List.member g model.genresSelected
                newList =
                    if already then
                        List.filter ((/=) g) model.genresSelected
                    else if List.length model.genresSelected < 3 then
                        g :: model.genresSelected
                    else
                        model.genresSelected
            in
            ( { model | genresSelected = newList }, Cmd.none )

        UpdateSearch s ->
            if String.isEmpty s then
                ( { model | searchTitle = s, movies = [], status = "" }, Cmd.none )
            else
                ( { model | searchTitle = s, status = "Buscando..." }
                , searchMovies s
                )

        GotMovies (Ok ms) ->
            ( { model | movies = ms, status = "" }, Cmd.none )

        GotMovies (Err _) ->
            ( { model | status = "Erro ao buscar filmes!" }, Cmd.none )

        ToggleFavorite movie ->
            let
                movieId = getMovieId movie
                isFav = Dict.member movieId model.favorites
                newFavs =
                    if isFav then
                        Dict.remove movieId model.favorites
                    else
                        Dict.insert movieId movie model.favorites
            in
            ( { model | favorites = newFavs }, Cmd.none )

        SendAll ->
            let
                -- Pegar IDs dos favoritos
                favIds = Dict.keys model.favorites
                englishGenres = List.map translateGenre model.genresSelected
                body =
                    Encode.object
                        [ ( "generos", Encode.list Encode.string englishGenres )
                        , ( "favoritos", Encode.list Encode.string favIds )
                        ]
            in
            ( { model | status = "Carregando recomendações...", modalOpen = False }
            , Http.post
                { url = "http://localhost:3000/recommend/genero/recentes"
                , body = Http.jsonBody body
                , expect = Http.expectJson GotRecommended moviesDecoder 
                }
            )

        GotRecommended (Ok movies) ->
            ( { model | recommended = movies, status = "✨ " ++ String.fromInt (List.length movies) ++ " filmes recomendados carregados!" }, Cmd.none )

        GotRecommended (Err err) ->
            let
                errorMsg = case err of
                    Http.BadUrl _ -> "URL inválida"
                    Http.Timeout -> "Timeout - tente novamente"
                    Http.NetworkError -> "Erro de rede - verifique sua conexão"
                    Http.BadStatus code -> "Erro do servidor " ++ String.fromInt code
                    Http.BadBody errorBody -> "Erro de dados: " ++ errorBody
            in
            ( { model | status = "❌ Erro ao carregar recomendações: " ++ errorMsg }, Cmd.none )

        CloseModal ->
            ( { model | modalOpen = False }, Cmd.none )

        OpenModal ->
            ({ model | modalOpen = True }, Cmd.none)


-- COMUNICAÇÃO COM BACKEND

searchMovies : String -> Cmd Msg
searchMovies s =
    Http.get
        { url = "http://localhost:3000/search/movie?title=" ++ s
        , expect = Http.expectJson GotMovies moviesDecoder
        }


-- VIEW

view : Model -> Html Msg
view model =
    div [ class "min-h-screen bg-gray-50" ]
        [ -- Header fixo
          div [ class "fixed top-0 left-0 right-0 bg-white shadow-md z-50 p-4" ]
            [ div [ class "max-w-6xl mx-auto flex justify-between items-center" ]
                [ div [ class "text-2xl font-bold text-blue-600" ] [ text "Movie Recomedd" ]
                , div [ class "flex gap-4 items-center" ]
                    [ div [ class "text-sm text-gray-600" ] [ text ("Favoritos: " ++ String.fromInt (Dict.size model.favorites)) ]
                    , button
                        [ onClick OpenModal
                        , class "bg-blue-500 hover:bg-blue-600 text-white px-4 py-2 rounded-lg transition-colors"
                        ]
                        [ text "🔍 Buscar Filmes" ]
                    ]
                ]
            ]

        -- Conteúdo principal
        , div [ class "pt-24 pb-8 px-4 max-w-6xl mx-auto" ]
            [ -- Status
              if not (String.isEmpty model.status) then
                div [ class "mb-6 p-4 bg-blue-50 border border-blue-200 rounded-lg text-center" ]
                    [ text model.status ]
              else
                text ""

            -- Modal de busca e gêneros
            , if model.modalOpen then
                div [ class "fixed inset-0 bg-black bg-opacity-50 flex justify-center items-center z-40" ]
                    [ div [ class "bg-white p-6 rounded-lg shadow-2xl w-full max-w-4xl max-h-[90vh] overflow-y-auto mx-4" ]
                        [ -- Header do modal
                          div [ class "flex justify-between items-center mb-4 border-b pb-4" ]
                            [ div [ class "text-xl font-bold" ] [ text "Configurar Preferências" ]
                            , button 
                                [ onClick CloseModal
                                , class "text-gray-500 hover:text-gray-700 text-2xl font-bold px-3 py-1 rounded"
                                ] 
                                [ text "×" ]
                            ]

                        -- Seleção de gêneros
                        , div [ class "mb-6" ]
                            [ div [ class "mb-3 font-bold text-lg" ] [ text "📽️ Escolha até 3 gêneros favoritos:" ]
                            , div [ class "grid grid-cols-3 md:grid-cols-4 lg:grid-cols-6 gap-3" ] 
                                (List.map (genreCheckbox model.genresSelected) genres)
                            ]

                        -- Busca de filmes
                        , div [ class "mb-4" ]
                            [ div [ class "mb-3 font-bold text-lg" ] [ text "🎭 Adicionar filmes favoritos:" ]
                            , input 
                                [ type_ "text"
                                , placeholder "Digite o nome do filme..."
                                , onInput UpdateSearch
                                , class "border border-gray-300 px-4 py-2 w-full rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
                                ] []
                            ]

                        -- Resultados da busca
                        , if not (List.isEmpty model.movies) then
                            div [ class "mb-6" ]
                                [ div [ class "mb-3 font-semibold" ] [ text "Resultados da busca:" ]
                                , div [ class "grid grid-cols-2 md:grid-cols-4 lg:grid-cols-6 gap-4 max-h-80 overflow-y-auto" ] 
                                    (List.map viewMovie model.movies)
                                ]
                          else
                            text ""

                        -- Botão de envio
                        , div [ class "text-center pt-4 border-t" ]
                            [ button 
                                [ onClick SendAll
                                , class "bg-green-500 hover:bg-green-600 text-white px-8 py-3 rounded-lg font-bold text-lg transition-colors"
                                , disabled (List.isEmpty model.genresSelected && Dict.isEmpty model.favorites)
                                ] 
                                [ text "Gerar Recomendações" ]
                            ]
                        ]
                    ]
              else
                text ""

            -- Lista de favoritos
            , if not (Dict.isEmpty model.favorites) then
                div [ class "mb-8" ]
                    [ div [ class "mb-4 text-xl font-bold text-gray-800" ] [ text "⭐ Seus Filmes Favoritos" ]
                    , div [ class "grid grid-cols-2 md:grid-cols-4 lg:grid-cols-6 gap-4" ] 
                        (List.map viewFavorite (Dict.values model.favorites))
                    ]
              else
                text ""

            -- Lista de recomendações
            , if not (List.isEmpty model.recommended) then
                div []
                    [ div [ class "mb-4 text-xl font-bold text-gray-800" ] [ text "🎯 Filmes Recomendados Para Você" ]
                    , div [ class "grid grid-cols-2 md:grid-cols-4 lg:grid-cols-6 gap-4" ]
                        (List.map viewRecommendedMovie model.recommended)
                    ]
              else if Dict.isEmpty model.favorites && List.isEmpty model.genresSelected then
                div [ class "text-center py-16" ]
                    [ div [ class "text-6xl mb-4" ] [ text "🎬" ]
                    , div [ class "text-xl text-gray-600 mb-4" ] [ text "Bem-vindo ao Movie Recommed!" ]
                    , div [ class "text-gray-500 mb-6" ] [ text "Clique em 'Buscar Filmes' para começar a adicionar seus favoritos" ]
                    , button
                        [ onClick OpenModal
                        , class "bg-blue-500 hover:bg-blue-600 text-white px-6 py-3 rounded-lg font-semibold transition-colors"
                        ]
                        [ text "Começar" ]
                    ]
              else
                text ""
            ]
        ]


genreCheckbox : List String -> String -> Html Msg
genreCheckbox selected g =
    let
        isChecked = List.member g selected
        isDisabled = not isChecked && List.length selected >= 3
        checkboxClass = if isChecked then
            "bg-blue-500 border-blue-500 text-white"
            else if isDisabled then
                "bg-gray-200 border-gray-300 text-gray-500 cursor-not-allowed"
            else
                "bg-white border-gray-300 hover:border-blue-400 text-gray-700"
    in
    button
        [ onClick (ToggleGenre g)
        , disabled isDisabled
        , class ("px-3 py-2 rounded-lg border-2 text-sm font-medium transition-all " ++ checkboxClass)
        ]
        [ text (if isChecked then "✓ " ++ g else g) ]


viewMovie : Movie -> Html Msg
viewMovie movie =
    div [ class "bg-white border border-gray-200 rounded-lg shadow-md overflow-hidden hover:shadow-lg transition-shadow" ]
        [ img [ src movie.poster, alt movie.title, class "w-full h-36 object-cover" ] []
        , div [ class "p-3" ]
            [ div [ class "text-sm font-bold mb-1 line-clamp-2" ] [ text movie.title ]
            , div [ class "text-xs text-gray-500 mb-1" ] [ text (String.left 4 movie.releaseDate) ]
            , div [ class "text-xs text-blue-600 mb-2" ] [ text ("⭐ " ++ String.fromFloat movie.voteAverage) ]
            , button 
                [ onClick (ToggleFavorite movie)
                , class "w-full bg-yellow-400 hover:bg-yellow-500 text-white px-2 py-1 rounded text-xs font-medium transition-colors"
                ]
                [ text "⭐ Favoritar" ]
            ]
        ]


viewFavorite : Movie -> Html Msg
viewFavorite movie =
    div [ class "bg-white border border-yellow-200 rounded-lg shadow-md overflow-hidden hover:shadow-lg transition-shadow" ]
        [ img [ src movie.poster, alt movie.title, class "w-full h-36 object-cover" ] []
        , div [ class "p-3" ]
            [ div [ class "text-sm font-bold mb-1 line-clamp-2" ] [ text movie.title ]
            , div [ class "text-xs text-gray-500 mb-1" ] [ text (String.left 4 movie.releaseDate) ]
            , div [ class "text-xs text-blue-600 mb-2" ] [ text ("⭐ " ++ String.fromFloat movie.voteAverage) ]
            , button 
                [ onClick (ToggleFavorite movie)
                , class "w-full bg-red-500 hover:bg-red-600 text-white px-2 py-1 rounded text-xs font-medium transition-colors"
                ]
                [ text "🗑️ Remover" ]
            ]
        ]


-- Agora usa a mesma função viewRecommendedMovie para recomendações
viewRecommendedMovie : Movie -> Html Msg
viewRecommendedMovie movie =
    div [ class "bg-white border border-green-200 rounded-lg shadow-md overflow-hidden hover:shadow-lg transition-shadow" ]
        [ img [ src movie.poster, alt movie.title, class "w-full h-36 object-cover" ] []
        , div [ class "p-3" ]
            [ div [ class "text-sm font-bold mb-1 line-clamp-2" ] [ text movie.title ]
            , div [ class "text-xs text-gray-500 mb-1" ] [ text (String.left 4 movie.releaseDate) ]
            , div [ class "text-xs text-blue-600 mb-1 font-medium" ] [ text ("⭐ " ++ String.fromFloat movie.voteAverage) ]
            , div [ class "text-xs text-green-600 line-clamp-2" ] [ text (String.join " • " movie.genres) ]
            ]
        ]


-- SUBSCRIPTIONS

subscriptions _ =
    Sub.none


-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }