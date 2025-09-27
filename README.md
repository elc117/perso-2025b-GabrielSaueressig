**Desenvolvido como Trabalho da disciplina Paradigmas de Programação**  
**Universidade Federal de Santa Maria - UFSM**  
**Autor: Gabriel Saueressig**  
**Ano: 2025**

### Informações Importantes

**Para rodar os testes utilize** - cabal test

**Para rodar o sistema utilize** - cabal run e abra um servidor no arquivo html

# O Começo da Ideia

Como membro do PET-SI, tenho a oportunidade de participar de diversas atividades acadêmicas promovidas pelo grupo. Em uma dessas atividades recentes, o Professor Gabriel Lunardi ministrou uma palestra introdutória sobre sistemas de recomendação, área que despertou meu interesse imediato pela combinação de aspectos técnicos e comportamentais envolvidos. 

Reconhecendo a oportunidade  que o trabalho da disciplina representava, tanto para aprender sobre o assunto, quanto para aplicar estes conhecimentos, decidi direcionar meus esforços para explorar e utilizar conceitos dessa área. Sinceramente estavaa bem confuso no início sobre como progredir, tinha a ideia, mas pouco conhecimento de como aplicá-la, porém tive colegas que me ajudaram, principalmente no começo com relação a estrutura e parte conceitual do código: Michel foi fundamental ao sugerir a implementação de uma API e ao me introduzir aos métodos HTTP GET e POST, conceitos que se tornaram centrais para a arquitetura do sistema. Mateus contribuiu significativamente ao me ensinar sobre o sistema de build Cabal, ferramenta utilizada durante todo o desenvolvimento.


# Sistema de Recomendação de Filmes

Um sistema web fullstack desenvolvido em Haskell (backend) e a linguegem funcional recomendada ela professora Elm (frontend) que recomenda filmes baseado nas preferências de gênero do usuário e filmes favoritos. O projeto utiliza a API do TMDB (The Movie Database) para buscar informações detalhadas sobre filmes e aplicar um algoritmo próprio de recomendação.

## A Jornada de Desenvolvimento

Este projeto passou por múltiplas iterações significativas, com mudanças fundamentais na arquitetura, algoritmo de recomendação e integração de APIs. O desenvolvimento foi marcado por aprendizados práticos sobre programação funcional, integração de sistemas e design de algoritmos.

---

### Da OMDB para TMDB: Uma Migração Necessária

A primeira versão do sistema utilizava a OMDB (Open Movie Database) API, que parecia adequada inicialmente. A implementação original era relativamente simples:

```haskell
getFilmes :: String -> T.Text -> IO Value
getFilmes nome option = runReq defaultHttpConfig $ do
    let opts = option =: nome <> "apikey" =: "ae725e23"
    r <- req GET (http "www.omdbapi.com") NoReqBody jsonResponse opts
    pure (responseBody r :: Value)
```

No entanto, problemas sérios começaram a emergir durante o desenvolvimento. O mais crítico foi descobrir que gêneros importantes como "Romance" simplesmente não apareciam nos resultados da OMDB. Filmes claramente românticos não eram classificados adequadamente, e a inconsistência nos dados de gêneros tornava o sistema de recomendação praticamente inútil para certas categorias, além disso era impossível classificar dados pelo OMDB, por exemplo, pegar os filmes mais recentes.

A busca por uma solução levou à descoberta do TMDB, que oferecia dados muito mais ricos e consistentes. A migração não foi imediata, primeiro se tentou usar ambas as APIs em paralelo, com TMDB para busca por gêneros e OMDB para detalhes específicos. Mas logo ficou claro que manter duas APIs diferentes criava inconsistências e complexidade desnecessária.

A decisão de migrar completamente para TMDB transformou o projeto. Não apenas resolveu o problema dos gêneros, mas também trouxe benefícios inesperados como posters de alta qualidade, dados mais precisos sobre lançamentos, e um sistema de IDs mais robusto.

```haskell
-- Implementação final unificada com TMDB
buscarFilmesPorTitulo :: T.Text -> IO [Movie]
buscarFilmesPorTitulo titulo = runReq defaultHttpConfig $ do
    let opts = "api_key" =: tmdbApiKey <>
               "query" =: titulo <>
               "include_adult" =: ("false" :: T.Text) <>
               "language" =: ("en-US" :: T.Text)
    r <- req GET (https "api.themoviedb.org" /: "3" /: "search" /: "movie")
             NoReqBody jsonResponse opts
    let body = responseBody r :: SearchResp
    return (searchResults body)
```

### A Questão do Conteúdo Adulto

Um desafio inesperado surgiu quando o sistema começou a recomendar filmes inadequados. O TMDB classifica "adult content" principalmente como material adulto, mas filmes com violência intensa, linguagem forte ou temas maduros não recebem essa classificação. Isso resultava em recomendações ocasionais de filmes adultos para um sistema que deveria ser mais geral.

A solução envolveu múltiplas camadas de filtragem. Além do parâmetro básico `include_adult: false`, foram implementados filtros de classificação etária usando o sistema americano:

```haskell
buscarChunkGenerosPagina :: [T.Text] -> Int -> IO [Movie]
buscarChunkGenerosPagina genreIds pagina = runReq defaultHttpConfig $ do
    let opts = "api_key" =: tmdbApiKey <>
               "with_genres" =: genreString <>
               "include_adult" =: ("false" :: T.Text) <>
               "certification_country" =: ("US" :: T.Text) <>
               "certification.lte" =: ("R" :: T.Text) <>
               "vote_average.gte" =: ("7" :: T.Text) <>
               -- outros parâmetros...
```

Esta abordagem garantiu que filmes extremamente inadequados fossem filtrados, mantendo ainda uma seleção interessante de conteúdo.

### Conversão de Gêneros: O Problema da Tradução

Um dos primeiros obstáculos técnicos foi descobrir que o TMDB não trabalha com nomes de gêneros diretamente, mas sim com IDs numéricos. Cada gênero tem um ID específico (por exemplo, "Action" = 28, "Horror" = 27, "Romance" = 10749). Isso criou um problema de mapeamento entre a interface em português e a API em inglês com IDs numéricos.

A solução envolveu criar duas funções de mapeamento:

```haskell
-- Mapear IDs numéricos do TMDB para nomes legíveis
tmdbIdToGenre :: Int -> T.Text
tmdbIdToGenre genreId = case genreId of
    28 -> "Action"
    27 -> "Horror"
    10749 -> "Romance"
    16 -> "Animation"
    -- ... outros mapeamentos

-- Mapear nomes de gêneros para IDs do TMDB
genreToTMDBId :: T.Text -> T.Text
genreToTMDBId genre = case T.toLower genre of
    "action" -> "28"
    "horror" -> "27"
    "romance" -> "10749"
    "animation" -> "16"
    -- ... outros mapeamentos
```

Esta conversão dupla permitiu que o frontend trabalhasse com nomes amigáveis enquanto o backend comunicava eficientemente com a API do TMDB usando os IDs corretos.

### Evolução do Algoritmo de Recomendação

O algoritmo de recomendação passou por três versões principais, cada uma abordando limitações descobertas na anterior.

**Primeira Versão: O Contador**

A abordagem inicial era embaraçosamente simples. Contava-se quantos gêneros de um filme coincidiam com as preferências do usuário, armazenava-se em uma tupla (filme, quantidade_matches), ordenava-se por quantidade e pegavam-se os 20 primeiros:

```haskell
pontuarFilme :: [T.Text] -> Movie -> Double
pontuarFilme generosPreferidos filme =
    let matches = filter (`elem` generosPreferidos) (genres filme)
    in fromIntegral (length matches)
```

Esta versão tratava um filme trash de ação com nota 3.0 da mesma forma que um clássico como "The Dark Knight" - desde que ambos fossem do gênero "Action". Rapidamente se percebeu que a qualidade do filme precisava ser considerada.

**Segunda Versão: Qualidade**

A segunda iteração introduziu a nota do filme como fator, somando-se linearmente o número de matches com a nota normalizada:

```haskell
pontuarFilme generosPreferidos filme =
    let matches = filter (`elem` generosPreferidos) (genres filme)
        scoreGeneros = fromIntegral (length matches)
        bonusNota = voteAverage filme / 10.0
    in scoreGeneros + bonusNota
```

Esta versão era melhor, mas ainda tinha problemas fundamentais. Um filme com dois gêneros compatíveis sempre vencia um filme com um gênero, independentemente de quão bem ele se encaixava nas preferências totais do usuário. Além disso, o sistema era puramente aditivo, sem penalizações para filmes completamente inadequados.

**Versão Final: Compatibilidade Percentual**

A versão final introduziu o conceito de compatibilidade percentual e um sistema de pontuação mais sofisticado:

```haskell
pontuarFilme :: [T.Text] -> Movie -> Double
pontuarFilme generosPreferidos filme =
    let generosFilme = genres filme
        matches = filter (`elem` generosPreferidos) generosFilme
        numMatches = length matches
        totalPreferidos = length generosPreferidos
        
        -- Compatibilidade como porcentagem
        compatibilidade = fromIntegral numMatches / fromIntegral totalPreferidos
        
        -- Score exponencial favorece múltiplos matches
        scoreGeneros = compatibilidade ** 1.5 * 10.0
        
        -- Bonus normalizado pela qualidade
        bonusNota = (voteAverage filme - 5.0) / 5.0
        
        -- Bonus especial para alta compatibilidade
        bonusCompatibilidade = if compatibilidade >= 0.5
                              then compatibilidade * 3.0
                              else 0.0
        
        -- Penalização severa para filmes sem matches
        penalidade = if numMatches == 0 then -20.0 else 0.0
    --Não permite score negativo
    in max 0.0 (scoreGeneros + bonusNota + bonusCompatibilidade + penalidade)
```

Esta abordagem resolve vários problemas. Se um usuário gosta de ["Horror", "Romance"], um filme que é ["Horror", "Romance", "Comedy"] recebe score baseado em 100% de compatibilidade (2/2 gêneros preferidos), enquanto um filme apenas ["Horror"] recebe 50% de compatibilidade. O componente exponencial (`** 1.5`) garante que múltiplos matches sejam significativamente mais valiosos que matches únicos.

### Estratégias de Busca: Do AND para OR

Paralelamente à evolução do algoritmo de pontuação, foi necessário repensar como buscar filmes na API do TMDB.

A primeira abordagem usava lógica AND - buscavam-se filmes que tivessem todos os gêneros preferidos simultaneamente. Para um usuário que gostava de "Horror", "Romance" e "Animation", procuravam-se filmes que fossem simultaneamente terror romântico animado. Como era de esperar, isso resultava em zero filmes encontrados na maioria dos casos.

A solução foi mudar para lógica OR com processamento inteligente:

```haskell
buscarFilmesAmplos :: [T.Text] -> IO [Movie]
buscarFilmesAmplos genreIds = do
    -- Divide gêneros em chunks pequenos para múltiplas buscas
    let chunksGeneros = chunksOf 3 genreIds
    
    -- Busca com OR logic: (Horror OR Romance OR Animation)
    todosFilmes <- mapM buscarMultiplasPaginasChunk chunksGeneros
    
    -- Remove duplicatas por título
    let filmesUnicos = nubBy (\a b -> title a == title b) (concat todosFilmes)
    return filmesUnicos
```

Esta abordagem busca filmes que tenham qualquer um dos gêneros preferidos, depois aplica o algoritmo de pontuação para rankeá-los por relevância. O resultado é muito mais robusto - sempre se encontram filmes, e os melhores matches naturalmente sobem ao topo.

Além disso, após pesquisas, foi implementado a tecnologia de chuncks, onde separa-se os gêneros totais em pequenas listas, neste caso, de três elementos, e é feita uma busca pra cada uma das listas


### Aleatoriedade e Descoberta

Uma questão importante era como garantir que o sistema não ficasse "preso" sempre recomendando os mesmos filmes populares. A solução foi introduzir aleatoriedade estratégica na busca:

```haskell
buscarMultiplasPaginasChunk :: [T.Text] -> IO [Movie]
buscarMultiplasPaginasChunk genreIds = do
    -- Busca 3-5 páginas aleatórias diferentes
    numPaginas <- randomRIO (3, 5 :: Int)
    paginasAleatorias <- sequence $ replicate numPaginas (randomRIO (1, 25 :: Int))
    
    resultados <- mapM (buscarChunkGenerosPagina genreIds) paginasAleatorias
    return (concat resultados)
```
Isso garante que usuários com as mesmas preferências não vejam sempre as mesmas recomendações, criando um elemento de descoberta que torna o sistema mais interessante.

O sistema busca de 3 a 5 paginás aleatórias entre 1 e 25 do sistema do IMDB, que está ordenado porpopularidade, tendo a possibilidade de buscar filmes mais antigos dependendo dos números gerados, vale ressaltar que está busca aleatória é feita em chuncks, fazedo assim cada chunk de gêneros ter entre 60 e 100 filmes, que posteriormente serão ordenados por pontuação

---

### Desafios de Integração Frontend-Backend

A integração entre Elm e Haskell apresentou desafios únicos que vão além da simples comunicação HTTP. O principal obstáculo foi a diferença fundamental entre os sistemas de tipos das duas linguagens e como elas lidam com erros e valores ausentes.

**Os Tipos Haskell**

Uma das dificuldades mais inesperadas foi compreender e trabalhar com os tipos complexos das bibliotecas Haskell. O ecossistema Haskell faz uso extensivo de tipos como `Maybe`, `Either`, `T.Text`, `IO`, e combinações desses tipos que podem ser difíceis para quem não está familiarizado.

Por exemplo, uma função simples como buscar um filme retorna:

```haskell
buscarFilmePorImdbId :: T.Text -> IO (Maybe Movie)
```

Este tipo de retorno significa que a função:
1. Executa uma operação IO (rede)
2. Pode não encontrar o filme (`Maybe`)
3. Trabalha com `T.Text` em vez de `String`

Para alguém acostumado com linguagens mais permissivas, isso cria várias camadas de complexidade. É necessário usar `liftIO` para trabalhar dentro de contextos IO, pattern matching para extrair valores de `Maybe`, e conversões constantes entre `String` e `T.Text`.

```haskell
-- Código que inicialmente parecia simples se torna complexo
buscarGenerosPorId :: String -> IO [T.Text]
buscarGenerosPorId favoriteId = do
    if "tt" `isPrefixOf` favoriteId
    then do
        filme <- buscarFilmePorImdbId (T.pack favoriteId)  -- Conversão String -> T.Text
        case filme of                                       -- Pattern matching no Maybe
            Just f -> return (genres f)                     -- Extração do valor
            Nothing -> return []                            -- Tratamento do caso vazio
    else do
        let tituloFilme = takeWhile (/= '_') favoriteId
        filmes <- buscarFilmesPorTitulo (T.pack tituloFilme)
        case filmes of
            (filme:_) -> return (genres filme)
            [] -> return []
```

**Tratamento Obrigatório de Exceções no Elm**

Do lado do Elm, o sistema de tipos força o tratamento explícito de todos os cenários de erro. Isso é benéfico para robustez, mas adiciona complexidade significativa ao código. Não existe "null pointer exception" em Elm porque é obrigatório considerar todos os casos.

```elm
-- Elm força o tratamento de todos os casos possíveis
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotMovies (Ok movies) ->
            ({ model | movies = movies, status = "" }, Cmd.none)
        
        GotMovies (Err err) ->
            let errorMsg = case err of
                Http.BadUrl _ -> "URL inválida"
                Http.Timeout -> "Timeout - tente novamente"
                Http.NetworkError -> "Erro de rede"
                Http.BadStatus code -> "Erro do servidor " ++ String.fromInt code
                Http.BadBody errorBody -> "Erro de dados: " ++ errorBody
            in
            ({ model | status = "Erro: " ++ errorMsg }, Cmd.none)
```

Esse rigor torna o desenvolvimento mais lento inicialmente, mas resulta em aplicações muito mais estáveis. No entanto, a curva de aprendizado é íngreme quando se vem de linguagens que permitem ignorar tratamento de erros.

**Design Frontend Gerado por IA**

Um aspecto interessante do desenvolvimento foi que todo o design e implementação visual do frontend foi criado através de colaboração com IA. Descrições eram fornecidas do que se queria - "um modal centralizado para busca de filmes", "cards responsivos para exibir filmes", "uma interface moderna com Tailwind" - e a IA traduzia essas ideias diretamente para código Elm funcional.

Isso acelerou significativamente o desenvolvimento, permitindo focar na lógica de negócio em vez de detalhes de CSS e layout. No entanto, também criou uma dependência: quando algo precisava ser ajustado no design, frequentemente era necessário explicar as mudanças em linguagem natural em vez de simplesmente editar o código diretamente.

**Serialização JSON**

A comunicação entre Elm e Haskell via JSON se tornou um ponto de fricção constante. Haskell serializa dados usando suas convenções (snake_case para campos da API, tipos Maybe que podem ser omitidos), enquanto Elm precisa de decoders explícitos que correspondam exatamente ao formato recebido.

```elm
-- Decoder que precisa corresponder exatamente ao JSON do Haskell
movieDecoder : Decoder Movie
movieDecoder =
    Decode.map6 Movie
        (Decode.field "title" Decode.string)
        (Decode.field "releaseDate" Decode.string)  -- Deve coincidir com o campo Haskell
        (Decode.field "voteAverage" Decode.float)
        (Decode.field "genres" (Decode.list Decode.string))
        (Decode.field "poster" Decode.string)
        (Decode.maybe (Decode.field "imdbId" Decode.string))  -- Campo opcional
```

Qualquer mudança na estrutura de dados do Haskell quebrava o frontend, exigindo atualizações coordenadas em ambos os lados. Isso é diferente de linguagens mais dinâmicas onde se pode ignorar campos não utilizados.

**Debugging**

Debuggar problemas entre as duas linguagens foi particularmente desafiador. Um erro no frontend Elm poderia ser causado por:
1. Dados mal formatados vindos do Haskell
2. Decoder incorreto no Elm
3. Lógica de negócio errada no backend
4. Problemas de rede/CORS
5. Incompatibilidades de tipo entre as linguagens

Não existe um debugger unificado, então é necessário usar as ferramentas específicas de cada linguagem e correlacionar os problemas mentalmente.

---

### A Complexidade dos Testes em Haskell

Criar um arquivo de testes unitários revelou-se uma tarefa muito mais complexa do que inicialmente antecipado. A experiência de testing em Haskell difere significativamente de linguagens imperativas, criando várias camadas de dificuldade técnica e conceitual.

**Tipos e Compatibilidade**

O primeiro obstáculo foi a incompatibilidade de tipos entre as funções do sistema e as expectativas do framework HUnit. Muitas funções trabalhavam com `T.Text`, `Maybe`, `IO`, entre outras, enquanto os testes precisavam comparar valores específicos. Isso exigia conversões constantes e compreensão profunda dos tipos envolvidos:

```haskell
-- Erro comum: misturar String e T.Text
testGenreToTMDBId :: Test
testGenreToTMDBId = TestCase $ do
    assertEqual "teste" "28" (genreToTMDBId "action")  -- Tipo incorreto
    
-- Versão corrigida com OverloadedStrings
{-# LANGUAGE OverloadedStrings #-}
testGenreToTMDBId :: Test
testGenreToTMDBId = TestCase $ do
    assertEqual "teste" "28" (genreToTMDBId "action")  -- Agora funciona
```

**Configuração de Build Problemática**

A configuração do sistema de build para testes foi particularmente frustrante. O Cabal exigia configurações específicas que não eram óbvias, especialmente quando se trabalhava com módulos `Main` duplicados:

```cabal
-- Problema: módulos Main conflitantes
executable recsystem
    main-is: Main.hs

test-suite tests  
    main-is: TestMyFunctions.hs  -- Também tinha 'module Main'
    -- Resultado: erro de módulos duplicados
```

A solução exigiu compreender a estrutura de módulos do Cabal e usar `ghc-options: -main-is TestMain` para resolver conflitos.

**Estruturas de Dados Complexas**

Criar estruturas de dados de teste que correspondessem exatamente aos tipos definidos no sistema principal foi trabalhoso. O tipo `Movie` tinha muitos campos, alguns opcionais, exigindo construção manual cuidadosa:

```haskell
-- Construir um Movie para teste requeria todos os campos
let filme1 = Movie 
    "Test Movie"           -- title :: T.Text
    "2023-01-01"          -- releaseDate :: T.Text  
    8.0                   -- voteAverage :: Double
    ["Action", "Adventure"] -- genres :: [T.Text]
    ""                    -- poster :: T.Text
    [28,12]               -- genreIds :: [Int]
    Nothing               -- imdbId :: Maybe T.Text
```

Qualquer mudança na definição do tipo `Movie` quebrava todos os testes que construíam instâncias manualmente.

**Cobertura de Teste Incompleta**

A distinção entre funções puras (facilmente testáveis) e funções com efeitos colaterais (difíceis de testar) resultou em cobertura de teste desigual. Muitas das funções mais importantes do sistema não podiam ser testadas adequadamente..

---

### Lições Aprendidas

O desenvolvimento deste projeto ensinou várias lições valiosas sobre desenvolvimento em paradigmas funcionais e design de sistemas de recomendação.

1 - A migração de OMDB para TMDB não foi apenas uma troca de APIs, foi uma transformação fundamental na qualidade das recomendações. Dados inconsistentes ou incompletos dificultam muito o andamento do projeto.

2 - algoritmos de recomendação precisam ser testados com casos extremos. A primeira versão funcionava razoavelmente bem para usuários com preferências "normais", mas falhava completamente para casos como usuários que gostavam de gêneros muito específicos ou combinações incomuns.

3 - a aleatoriedade controlada é crucial para a experiência do usuário. Sistemas determinísticos podem ser tecnicamente corretos mas entediantes na prática. Encontrar o equilíbrio entre relevância e descoberta foi um dos aspectos mais interessantes do projeto.

4 - a integração entre diferentes paradigmas funcionais (Elm e Haskell) requer cuidado especial com tipos e estruturas de dados. O que parece simples no papel pode se tornar complexo quando sistemas tipados rigorosos precisam se comunicar através de JSON.

O resultado final é um sistema que não apenas recomenda filmes adequados, mas faz isso de forma interessante e variada, mantendo os usuários engajados através de descobertas inesperadas dentro de suas preferências declaradas.

## Próximos Passos e Melhorias Futuras

O projeto atual oferece uma base sólida para evoluções futuras. Inspirado por outros trabalhos na disciplina, como do trabalho do Mateus, e motivado pelo aprendizado obtido, há planos para transformar esta aplicação em um sistema de recomendação mais robusto e escalável.

### Persistência de Dados com SQLite

A primeira melhoria planejada é a implementação de um banco de dados local usando SQLite para criar um backlog persistente de filmes. Atualmente, todas as informações são perdidas quando o usuário fecha o navegador. Com SQLite, o sistema poderá:

- Manter histórico de filmes avaliados pelos usuários
- Armazenar preferências personalizadas ao longo do tempo
- Cachear dados do TMDB para reduzir chamadas de API e melhorar performance
- Registrar interações (filmes visualizados, tempo gasto em cada recomendação)
- Implementar ratings implícitos baseados no comportamento do usuário

A implementação envolverá adicionar persistência tanto no backend Haskell quanto potencialmente no frontend através de localStorage ou IndexedDB, criando uma experiência mais fluida onde as preferências do usuário evoluem organicamente.

### Sistemas de Recomendação Coletiva (Collaborative Filtering)

O algoritmo atual é puramente baseado em conteúdo (content-based filtering). A próxima evolução natural é implementar filtragem colaborativa, que recomenda filmes baseado em usuários com gostos similares. Isso permitirá descobrir filmes que talvez não tenham os gêneros preferidos explícitos, mas que usuários similares gostaram.

As implementações planejadas incluem:

1. **User-Based Collaborative Filtering**: Encontrar usuários com preferências similares e recomendar filmes que eles gostaram
2. **Item-Based Collaborative Filtering**: Recomendar filmes similares aos que o usuário já demonstrou interesse
3. **Sistemas Híbridos**: Combinar recomendação por conteúdo com colaborativa para resultados mais robustos
4. **Matrix Factorization**: Técnicas mais avançadas como SVD para lidar com esparsidade de dados

### Aprofundamento em Conceitos de Recomendação

O projeto atual oferece uma base sólida para explorar conceitos mais avançados de sistemas de recomendação:

**Machine Learning Integration**: Implementar algoritmos de aprendizado que se adaptam automaticamente às preferências do usuário ao longo do tempo, possivelmente integrando bibliotecas Haskell como HLearn ou conectando com Python através de APIs.

**Diversidade e Serendipity**: garantir que o usuário não fique "preso" em uma bolha de recomendações previsíveis. Isso envolve algoritmos que intencionalmente introduzem variedade nas sugestões.

**Explicabilidade**: Implementar recursos que expliquem por que um filme foi recomendado ("Porque você gostou de filmes de terror dos anos 80" ou "Usuários com gostos similares também gostaram").

**Cold Start Problem**: Melhorar como o sistema lida com novos usuários que ainda não têm histórico suficiente para recomendações personalizadas.

   
### Motivação Pessoal

Este projeto despertou um interesse genuíno em sistemas de recomendação, um campo fascinante que combina matemática, programação, e compreensão do comportamento humano. A possibilidade de criar sistemas que ajudam pessoas a descobrir conteúdo relevante e interessante é motivadora tanto do ponto de vista técnico quanto pessoal.

O trabalho realizado até agora fornece uma fundação sólida, mas também revelou a profundidade e complexidade deste domínio. Há muito espaço para crescimento, desde implementações técnicas mais sofisticadas até compreensão mais profunda de como as pessoas realmente interagem com recomendações.

## Referências

### Documentação Técnica
1. **Haskell.org** - Haskell Language Documentation. https://www.haskell.org/documentation/
2. **Scotty Documentation** - Web Framework for Haskell. https://hackage.haskell.org/package/scotty
3. **Elm Guide** - An Introduction to Elm. https://guide.elm-lang.org/
4. **TMDB API Documentation** - The Movie Database API v3. https://developers.themoviedb.org/3
5. **OMDB API** - The Open Movie Database (usado inicialmente). http://www.omdbapi.com/
6. **Cabal User Guide** - Package Management for Haskell. https://cabal.readthedocs.io/
7. **HUnit Documentation** - Unit Testing for Haskell. https://hackage.haskell.org/package/HUnit

### Conceitos de Sistemas de Recomendação
8. **Ricci, F., Rokach, L., & Shapira, B.** (2015). Recommender Systems Handbook. Springer.
9. **Aggarwal, C. C.** (2016). Recommender Systems: The Textbook. Springer.
10. **Prof. Gabriel Lunnardi** Palestra sobre recomendação em parceria com o PET-SI

### Assistência de IA (Prompts Resumidos)
11. **Claude** - Debugging de tipos Haskell, como otimizar de algoritmos de recomendação
12. **Claude** - Como integrar APIs externas em Haskell, resolução de conflitos CORS
13. **Claude** - Entender Problemas Relacionados ao arquivo de Testes
14. **Claude** - Fazer interface frontend moderna

