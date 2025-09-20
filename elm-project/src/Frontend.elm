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

type alias Movie =
    { title : String
    , year : String
    , imdbID : String
    , poster : String
    }

-- Novo tipo para filmes recomendados (TMDB)
type alias RecommendedMovie =
    { title : String
    , releaseDate : String  
    , voteAverage : Float
    , genres : List String
    , poster : String  -- Adicionado campo poster
    }

type alias Model =
    { genresSelected : List String
    , searchTitle : String
    , movies : List Movie
    , favorites : Dict.Dict String Movie
    , recommended : List RecommendedMovie  -- Mudança aqui
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
    | GotRecommended (Result Http.Error (List RecommendedMovie))  -- Mudança aqui
    | CloseModal
    | OpenModal


-- DECODERS

movieDecoder : Decoder Movie
movieDecoder =
    Decode.map4 Movie
        (Decode.field "Title" Decode.string)
        (Decode.field "Year" Decode.string)
        (Decode.field "imdbID" Decode.string)
        (Decode.field "Poster" Decode.string)


moviesDecoder : Decoder (List Movie)
moviesDecoder =
    Decode.field "Search" (Decode.list movieDecoder)

-- Novo decoder para filmes recomendados (TMDB)
recommendedMovieDecoder : Decoder RecommendedMovie
recommendedMovieDecoder =
    Decode.map5 RecommendedMovie
        (Decode.field "title" Decode.string)
        (Decode.field "releaseDate" Decode.string)
        (Decode.field "voteAverage" Decode.float)
        (Decode.field "genres" (Decode.list Decode.string))
        (Decode.field "poster" Decode.string)

recommendedDecoder : Decoder (List RecommendedMovie)
recommendedDecoder =
    Decode.list recommendedMovieDecoder


-- GÊNEROS POSSÍVEIS

genres : List String
genres =
    [ "Ação", "Drama", "Comédia", "Terror", "Romance", "Sci-Fi" ]


translateGenre : String -> String
translateGenre genre =
    case genre of
        "Ação" -> "Action"
        "Drama" -> "Drama"
        "Comédia" -> "Comedy"
        "Terror" -> "Horror"
        "Romance" -> "Romance"
        "Sci-Fi" -> "Sci-Fi"
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
                isFav = Dict.member movie.imdbID model.favorites
                newFavs =
                    if isFav then
                        Dict.remove movie.imdbID model.favorites
                    else
                        Dict.insert movie.imdbID movie model.favorites
            in
            ( { model | favorites = newFavs }, Cmd.none )

        SendAll ->
            let
                favIds = Dict.keys model.favorites
                englishGenres = List.map translateGenre model.genresSelected
                body =
                    Encode.object
                        [ ( "generos", Encode.list Encode.string englishGenres )    -- Corrigido
                        , ( "favoritos", Encode.list Encode.string favIds )        -- Corrigido
                        ]
            in
            ( { model | status = "Enviando...", modalOpen = False }
            , Http.post
                { url = "http://localhost:3000/recommend/genero/recentes"
                , body = Http.jsonBody body
                , expect = Http.expectJson GotRecommended recommendedDecoder
                }
            )

        GotRecommended (Ok movies) ->
            ( { model | recommended = movies, status = "Recomendações carregadas" }, Cmd.none )

        GotRecommended (Err err) ->
            let
                errorMsg = case err of
                    Http.BadUrl _ -> "URL inválida"
                    Http.Timeout -> "Timeout"
                    Http.NetworkError -> "Erro de rede"
                    Http.BadStatus code -> "Erro " ++ String.fromInt code
                    Http.BadBody errorBody -> "Erro de decodificação: " ++ errorBody
            in
            ( { model | status = "Erro ao carregar recomendações: " ++ errorMsg }, Cmd.none )

        CloseModal ->
            ( { model | modalOpen = False }, Cmd.none )

        OpenModal ->
            ({ model | modalOpen = True }, Cmd.none)


-- COMUNICAÇÃO COM BACKEND

searchMovies : String -> Cmd Msg
searchMovies s =
    Http.get
        { url = "http://localhost:3000/filmeGeral?title=" ++ s ++ "&option=s"
        , expect = Http.expectJson GotMovies moviesDecoder
        }


-- VIEW

view : Model -> Html Msg
view model =
    div [ class "p-8 max-w-3xl mx-auto relative" ]
        [ -- Botão para abrir modal
          button
            [ onClick OpenModal
            , class "fixed top-4 right-4 bg-blue-500 hover:bg-blue-600 text-white px-4 py-2 rounded z-50"
            ]
            [ text "Abrir Busca" ]

        , -- Modal de busca e gêneros
          if model.modalOpen then
            div [ class "fixed inset-0 bg-black bg-opacity-50 flex justify-center items-start pt-8 z-40" ]
                [ div [ class "bg-white p-8 rounded shadow-lg w-96 max-h-[80vh] overflow-y-auto" ]
                    [ button [ onClick CloseModal, class " hover:text-gray text-black font-bold px-3 py-1 rounded shadow" ] [ text "X" ]
                    , div [ class "mb-1 font-bold" ] [ text "Escolha até 3 gêneros:" ]
                    , div [ class "mb-1 flex flex-wrap gap-4" ] (List.map (genreCheckbox model.genresSelected) genres)
                    , input [ type_ "text", placeholder "Digite o título do filme", onInput UpdateSearch, class "border px-2 py-1 w-full mb-2" ] []
                    , div [ class "mt-1 h-80 overflow-y-auto" ] (List.map viewMovie model.movies)
                    , button [ onClick SendAll, class "bg-green-500 hover:bg-green-600 text-white px-4 py-2 rounded mt-2" ] [ text "Enviar favoritos e gêneros" ]
                    ]
                ]
          else
            text ""

        , -- Status
          if not (String.isEmpty model.status) then
            div [ class "mb-4 p-3 bg-blue-100 border border-blue-300 rounded" ]
                [ text model.status ]
          else
            text ""

        , -- Lista de favoritos
          div [ class "mb-4 mt-4 font-bold text-lg" ] [ text "Favoritos:" ]
        , div [ class "grid grid-cols-4 gap-4 mt-2" ] (List.map viewFavorite (Dict.values model.favorites))

         -- Lista de recomendações
        , div [ class "mb-4 mt-4 font-bold text-lg" ] [ text "Recomendações Baseados nos Gêneros que você gosta:" ]
        , div [ class "grid grid-cols-4 gap-4 mt-2" ]
            (List.map viewRecommendedMovie model.recommended)  -- Nova função
        ]


genreCheckbox : List String -> String -> Html Msg
genreCheckbox selected g =
    let
        isChecked = List.member g selected
    in
    div [ class "flex items-center gap-2" ]
        [ input [ type_ "checkbox", Html.Attributes.checked isChecked, onClick (ToggleGenre g) ] []
        , label [] [ text g ]
        ]


viewMovie : Movie -> Html Msg
viewMovie movie =
    div [ class "border rounded shadow p-2 flex flex-col items-center mb-2" ]
        [ img [ src movie.poster, alt movie.title, class "w-24 h-32 object-cover rounded mb-2" ] []
        , div [ class "text-center text-sm font-bold" ] [ text movie.title ]
        , div [ class "text-xs text-gray-600 mb-1" ] [ text ("(" ++ movie.year ++ ")") ]
        , button [ onClick (ToggleFavorite movie), class "bg-yellow-400 hover:bg-yellow-500 text-white px-2 py-1 rounded text-xs" ]
            [ text "Favoritar" ]
        ]


viewFavorite : Movie -> Html Msg
viewFavorite movie =
    div [ class "border rounded shadow p-2 flex flex-col items-center" ]
        [ img [ src movie.poster, alt movie.title, class "w-24 h-32 object-cover rounded mb-2" ] []
        , div [ class "text-center text-sm font-bold" ] [ text movie.title ]
        , div [ class "text-xs text-gray-600" ] [ text ("(" ++ movie.year ++ ")") ]
        , button [ onClick (ToggleFavorite movie), class "bg-red-500 hover:bg-red-600 text-white px-2 py-1 rounded text-xs mt-1" ]
            [ text "Remover" ]
        ]

-- Nova função para exibir filmes recomendados
viewRecommendedMovie : RecommendedMovie -> Html Msg
viewRecommendedMovie movie =
    div [ class "border rounded shadow p-2 flex flex-col items-center" ]
        [ -- Agora usando o poster real da OMDB
          img [ src movie.poster, alt movie.title, class "w-24 h-32 object-cover rounded mb-2" ] []
        , div [ class "text-center text-sm font-bold" ] [ text movie.title ]
        , div [ class "text-xs text-gray-600 mb-1" ] [ text movie.releaseDate ]
        , div [ class "text-xs text-blue-600 mb-1" ] [ text ("⭐ " ++ String.fromFloat movie.voteAverage) ]
        , div [ class "text-xs text-green-600" ] [ text (String.join ", " movie.genres) ]
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