module App.Portfolio where

import Prelude
import Data.Array (findIndex, length, (!!))
import Data.Int (toNumber)
import Data.Maybe (maybe)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Web.TouchEvent (TouchEvent)
import Web.TouchEvent.TouchEvent (touches)
import Web.TouchEvent.TouchList as TL
import Web.TouchEvent.Touch as T
import Util.Decode (decodeBase64)

type State =
  { menuOpen :: Boolean
  , lightboxImage :: String
  , currentImageIndex :: Int
  , touchStartX :: Number
  }

data Action
  = ToggleMenu
  | OpenLightbox String
  | CloseLightbox
  | NextImage
  | PreviousImage
  | TouchStart Number
  | TouchEnd Number

galleryImages :: Array String
galleryImages =
  [ "./images/session-01.jpg"
  , "./images/session-02.jpg"
  , "./images/session-03.jpg"
  , "./images/session-04.jpg"
  , "./images/session-05.jpg"
  , "./images/session-06.jpg"
  , "./images/session-07.jpg"
  , "./images/session-08.jpg"
  , "./images/session-09.jpg"
  ]

obfuscatedEmail :: String
obfuscatedEmail = "bWFpbHRvOmp1bGl5YS5nbGlua2FAZ21haWwuY29t"

obfuscatedTel :: String
obfuscatedTel = "dGVsOis0ODUxMzc3NTg1Nw=="

obfuscatedEmailText :: String
obfuscatedEmailText = "anVsaXlhLmdsaW5rYUBnbWFpbC5jb20="

obfuscatedTelText :: String
obfuscatedTelText = "NTEzIDc3NSA4NTc="

component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { menuOpen: false, lightboxImage: "", currentImageIndex: 0, touchStartX: 0.0 }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div [ HP.class_ (H.ClassName "bg-cream") ]
    [ renderNav state
    , HH.main_
        [ renderHero
        , renderIntro
        , renderAbout
        , renderServices
        , renderGallery
        , renderFeaturedQuote
        , renderPackages
        , renderTestimonials
        , renderContact
        ]
    , renderFooter
    , if state.lightboxImage /= "" then renderLightbox state.lightboxImage else HH.text ""
    ]

renderNav :: forall cs m. State -> H.ComponentHTML Action cs m
renderNav _ =
  HH.nav [ HP.class_ (H.ClassName "fixed top-0 left-0 right-0 z-50 bg-cream/90 backdrop-blur-md"), HP.attr (HH.AttrName "role") "navigation", HP.attr (HH.AttrName "aria-label") "Nawigacja główna" ]
    [ HH.div [ HP.class_ (H.ClassName "max-w-7xl mx-auto px-4 md:px-8 h-20 flex items-center justify-between") ]
        [ HH.a [ HP.href "#", HP.class_ (H.ClassName "font-handwriting text-lg md:text-2xl text-stone-800 hover:text-accent transition-colors"), HP.title "Strona główna" ]
            [ HH.text "Julia Glinka" ]
        , HH.ul [ HP.class_ (H.ClassName "flex gap-4 md:gap-12 text-xs md:text-sm") ]
            [ navLink "O mnie" "#about"
            , navLink "Galeria" "#gallery"
            , navLink "Oferta" "#packages"
            , navLink "Kontakt" "#contact"
            ]
        ]
    ]
  where
  navLink text href =
    HH.li_
      [ HH.a
          [ HP.href href
          , HP.class_ (H.ClassName "font-body text-xs md:text-sm tracking-widest uppercase text-stone-500 hover:text-stone-800 transition-colors")
          ]
          [ HH.text text ]
      ]

renderVideoBackground :: forall cs m. String -> String -> String -> H.ComponentHTML Action cs m
renderVideoBackground videoSrc posterSrc imgAlt =
  HH.div [ HP.class_ (H.ClassName "absolute inset-0") ]
    [ HH.img
        [ HP.src posterSrc
        , HP.alt imgAlt
        , HP.class_ (H.ClassName "w-full h-full object-cover")
        ]
    , HH.element (HH.ElemName "video")
        [ HP.attr (HH.AttrName "autoplay") ""
        , HP.attr (HH.AttrName "loop") ""
        , HP.attr (HH.AttrName "muted") ""
        , HP.attr (HH.AttrName "playsinline") ""
        , HP.attr (HH.AttrName "poster") posterSrc
        , HP.class_ (H.ClassName "absolute inset-0 w-full h-full object-cover")
        ]
        [ HH.element (HH.ElemName "source")
            [ HP.src videoSrc
            , HP.type_ "video/mp4"
            ]
            []
        ]
    ]

renderHero :: forall cs m. H.ComponentHTML Action cs m
renderHero =
  HH.section [ HP.id "home", HP.class_ (H.ClassName "min-h-screen relative overflow-hidden") ]
    [ renderVideoBackground "./videos/hero.mp4" "./images/session-08.jpg" "Elegancka sesja ciążowa"
    , HH.div [ HP.class_ (H.ClassName "absolute inset-0 gradient-overlay") ] []
    , HH.div [ HP.class_ (H.ClassName "relative z-10 min-h-screen flex flex-col justify-end pb-24 md:pb-32 px-8 md:px-16") ]
        [ HH.div [ HP.class_ (H.ClassName "max-w-4xl animate-fade-in") ]
            [ HH.p [ HP.class_ (H.ClassName "font-body text-sm tracking-widest uppercase text-white/70 mb-6") ]
                [ HH.text "Sesje Ciążowe • Warszawa" ]
            , HH.h1 [ HP.class_ (H.ClassName "font-handwriting text-5xl md:text-7xl lg:text-8xl text-white mb-8 text-shadow-lg font-normal") ]
                [ HH.text "Julia Glinka" ]
            , HH.p [ HP.class_ (H.ClassName "font-body text-lg md:text-xl text-white/90 max-w-xl leading-relaxed mb-10") ]
                [ HH.text "Tworzę artystyczne, eleganckie portrety ciążowe, które zachowają piękno tego wyjątkowego czasu na zawsze." ]
            , HH.a
                [ HP.href "#packages"
                , HP.class_ (H.ClassName "inline-block font-body text-sm tracking-widest uppercase text-white border-b-2 border-white pb-2 hover:border-accent hover:text-accent transition-colors")
                ]
                [ HH.text "Zobacz ofertę" ]
            ]
        ]
    ]

renderIntro :: forall cs m. H.ComponentHTML Action cs m
renderIntro =
  HH.section [ HP.class_ (H.ClassName "py-32 md:py-48 bg-cream") ]
    [ HH.div [ HP.class_ (H.ClassName "max-w-4xl mx-auto px-8 text-center") ]
        [ HH.h2 [ HP.class_ (H.ClassName "font-display text-4xl md:text-6xl text-stone-800 mb-12 font-normal leading-tight") ]
            [ HH.text "Uchwycę Twoje"
            , HH.br []
            , HH.em [ HP.class_ (H.ClassName "font-normal") ] [ HH.text "najpiękniejsze chwile" ]
            ]
        , HH.p [ HP.class_ (H.ClassName "font-body text-lg text-stone-500 leading-relaxed max-w-2xl mx-auto") ]
            [ HH.text "Każda kobieta w ciąży jest piękna. Moim celem jest pokazanie tego piękna w sposób elegancki i artystyczny, tworząc obrazy, które będziesz cenić przez całe życie." ]
        ]
    ]

renderAbout :: forall cs m. H.ComponentHTML Action cs m
renderAbout =
  HH.section [ HP.id "about", HP.class_ (H.ClassName "py-24 md:py-40 bg-white overflow-hidden") ]
    [ HH.div [ HP.class_ (H.ClassName "max-w-7xl mx-auto px-8") ]
        [ HH.div [ HP.class_ (H.ClassName "grid md:grid-cols-12 gap-8 md:gap-12 items-center") ]
            [ HH.div [ HP.class_ (H.ClassName "md:col-span-5 relative") ]
                [ HH.div [ HP.class_ (H.ClassName "absolute -top-8 -left-8 w-full h-full border border-accent/30") ] []
                , HH.img
                    [ HP.src "./images/julia.jpg"
                    , HP.alt "Julia Glinka - fotografka"
                    , HP.class_ (H.ClassName "relative z-10 w-full shadow-4xl")
                    ]
                ]
            , HH.div [ HP.class_ (H.ClassName "md:col-span-6 md:col-start-7") ]
                [ HH.p [ HP.class_ (H.ClassName "font-body text-sm tracking-widest uppercase text-accent mb-6") ]
                    [ HH.text "O mnie" ]
                , HH.h2 [ HP.class_ (H.ClassName "font-display text-4xl md:text-5xl text-stone-800 mb-8 font-normal") ]
                    [ HH.text "Pasja do fotografii ciążowej" ]
                , HH.div [ HP.class_ (H.ClassName "font-body text-stone-500 leading-relaxed space-y-6") ]
                    [ HH.p_ [ HH.text "Mam na imię Julia i od siedmiu lat specjalizuję się w profesjonalnych sesjach ciążowych. Moja przygoda z fotografią zaczęła się, gdy zaczęłam robić zdjęcia mojej córeczce." ]
                    , HH.p_ [ HH.text "Kocham wyjątkowy kształt kobiecego ciała w ciąży i z radością podkreślam jego piękno w elegancki, subtelny sposób. Każda sesja to dla mnie wyjątkowe doświadczenie." ]
                    ]
                ]
            ]
        ]
    ]

renderServices :: forall cs m. H.ComponentHTML Action cs m
renderServices =
  HH.section [ HP.id "services", HP.class_ (H.ClassName "py-24 md:py-40 bg-cream") ]
    [ HH.div [ HP.class_ (H.ClassName "max-w-7xl mx-auto px-8") ]
        [ HH.div [ HP.class_ (H.ClassName "text-center mb-20") ]
            [ HH.p [ HP.class_ (H.ClassName "font-body text-sm tracking-widest uppercase text-accent mb-6") ]
                [ HH.text "Usługi" ]
            , HH.h2 [ HP.class_ (H.ClassName "font-display text-4xl md:text-5xl text-stone-800 font-normal") ]
                [ HH.text "Co oferuję" ]
            ]
        , HH.div [ HP.class_ (H.ClassName "grid md:grid-cols-2 gap-16 md:gap-24") ]
            [ HH.div [ HP.class_ (H.ClassName "group") ]
                [ HH.div [ HP.class_ (H.ClassName "w-16 h-px bg-accent mb-8 group-hover:w-24 transition-all duration-500") ] []
                , HH.h3 [ HP.class_ (H.ClassName "font-display text-2xl text-stone-800 mb-4") ]
                    [ HH.text "Sesja Fotograficzna" ]
                , HH.p [ HP.class_ (H.ClassName "font-body text-stone-500 leading-relaxed") ]
                    [ HH.text "Podczas sesji wykorzystamy różne pozy i stylizacje. Będę Cię prowadzić krok po kroku, pokazując, jak pozować, abyś czuła się swobodnie i pięknie." ]
                ]
            , HH.div [ HP.class_ (H.ClassName "group") ]
                [ HH.div [ HP.class_ (H.ClassName "w-16 h-px bg-accent mb-8 group-hover:w-24 transition-all duration-500") ] []
                , HH.h3 [ HP.class_ (H.ClassName "font-display text-2xl text-stone-800 mb-4") ]
                    [ HH.text "Garderoba Studyjna" ]
                , HH.p [ HP.class_ (H.ClassName "font-body text-stone-500 leading-relaxed") ]
                    [ HH.text "Do Twojej dyspozycji jest garderoba studyjna pełna eleganckich sukienek, tkanin i dodatków, które podkreślą Twoje piękno." ]
                ]
            , HH.div [ HP.class_ (H.ClassName "group") ]
                [ HH.div [ HP.class_ (H.ClassName "w-16 h-px bg-accent mb-8 group-hover:w-24 transition-all duration-500") ] []
                , HH.h3 [ HP.class_ (H.ClassName "font-display text-2xl text-stone-800 mb-4") ]
                    [ HH.text "Galeria Online" ]
                , HH.p [ HP.class_ (H.ClassName "font-body text-stone-500 leading-relaxed") ]
                    [ HH.text "Po sesji otrzymasz dostęp do prywatnej galerii online, gdzie samodzielnie wybierzesz swoje ulubione kadry." ]
                ]
            , HH.div [ HP.class_ (H.ClassName "group") ]
                [ HH.div [ HP.class_ (H.ClassName "w-16 h-px bg-accent mb-8 group-hover:w-24 transition-all duration-500") ] []
                , HH.h3 [ HP.class_ (H.ClassName "font-display text-2xl text-stone-800 mb-4") ]
                    [ HH.text "Profesjonalna Edycja" ]
                , HH.p [ HP.class_ (H.ClassName "font-body text-stone-500 leading-relaxed") ]
                    [ HH.text "Otrzymasz profesjonalnie wyedytowane fotografie w wysokiej rozdzielczości, gotowe do druku i publikacji." ]
                ]
            ]
        ]
    ]

renderGallery :: forall cs m. H.ComponentHTML Action cs m
renderGallery =
  HH.section [ HP.id "gallery", HP.class_ (H.ClassName "py-24 md:py-40 bg-white") ]
    [ HH.div [ HP.class_ (H.ClassName "max-w-7xl mx-auto px-8") ]
        [ HH.div [ HP.class_ (H.ClassName "text-center mb-20") ]
            [ HH.p [ HP.class_ (H.ClassName "font-body text-sm tracking-widest uppercase text-accent mb-6") ]
                [ HH.text "Portfolio" ]
            , HH.h2 [ HP.class_ (H.ClassName "font-display text-4xl md:text-5xl text-stone-800 font-normal") ]
                [ HH.text "Galeria" ]
            ]
        , HH.div [ HP.class_ (H.ClassName "grid grid-cols-2 md:grid-cols-3 gap-4 md:gap-6") ]
            [ galleryImageLarge "./images/session-01.jpg"
            , galleryImage "./images/session-02.jpg"
            , galleryImage "./images/session-03.jpg"
            , galleryImage "./images/session-04.jpg"
            , galleryImageLarge "./images/session-05.jpg"
            , galleryImage "./images/session-06.jpg"
            , galleryImage "./images/session-07.jpg"
            , galleryImageLarge "./images/session-08.jpg"
            , galleryImage "./images/session-09.jpg"
            ]
        ]
    ]

galleryImage :: forall cs m. String -> H.ComponentHTML Action cs m
galleryImage src =
  HH.figure [ HP.class_ (H.ClassName "aspect-square overflow-hidden group cursor-pointer") ]
    [ HH.img
        [ HP.src src
        , HP.alt "Sesja ciążowa"
        , HP.class_ (H.ClassName "w-full h-full object-cover transition-all duration-700 group-hover:scale-105")
        , HE.onClick \_ -> OpenLightbox src
        ]
    ]

galleryImageLarge :: forall cs m. String -> H.ComponentHTML Action cs m
galleryImageLarge src =
  HH.figure [ HP.class_ (H.ClassName "aspect-square overflow-hidden group cursor-pointer md:col-span-2 md:row-span-2 md:aspect-auto md:h-full") ]
    [ HH.img
        [ HP.src src
        , HP.alt "Sesja ciążowa"
        , HP.class_ (H.ClassName "w-full h-full object-cover transition-all duration-700 group-hover:scale-105")
        , HE.onClick \_ -> OpenLightbox src
        ]
    ]

renderFeaturedQuote :: forall cs m. H.ComponentHTML Action cs m
renderFeaturedQuote =
  HH.section [ HP.class_ (H.ClassName "relative py-40 md:py-56 overflow-hidden") ]
    [ renderVideoBackground "./videos/quote.mp4" "./images/session-12.jpg" ""
    , HH.div [ HP.class_ (H.ClassName "absolute inset-0 bg-stone-800/70") ] []
    , HH.div [ HP.class_ (H.ClassName "relative z-10 max-w-4xl mx-auto px-8 text-center") ]
        [ HH.blockquote_
            [ HH.p [ HP.class_ (H.ClassName "font-display text-3xl md:text-5xl text-white mb-8 font-normal leading-tight text-shadow") ]
                [ HH.text "\"Sesja ciążowa przerosła moje oczekiwania! Julia potrafiła uchwycić emocje i stworzyć piękne, pełne ciepła kadry.\"" ]
            , HH.footer [ HP.class_ (H.ClassName "font-body text-sm tracking-widest uppercase text-white/70") ]
                [ HH.text "— Anna F." ]
            ]
        ]
    ]

renderPackages :: forall cs m. H.ComponentHTML Action cs m
renderPackages =
  HH.section [ HP.id "packages", HP.class_ (H.ClassName "py-24 md:py-40 bg-cream") ]
    [ HH.div [ HP.class_ (H.ClassName "max-w-7xl mx-auto px-8") ]
        [ HH.div [ HP.class_ (H.ClassName "text-center mb-20") ]
            [ HH.p [ HP.class_ (H.ClassName "font-body text-sm tracking-widest uppercase text-accent mb-6") ]
                [ HH.text "Oferta" ]
            , HH.h2 [ HP.class_ (H.ClassName "font-display text-4xl md:text-5xl text-stone-800 font-normal mb-6") ]
                [ HH.text "Pakiety Sesji Ciążowej" ]
            , HH.p [ HP.class_ (H.ClassName "font-body text-stone-500 max-w-xl mx-auto") ]
                [ HH.text "Wybierz pakiet dopasowany do Twoich potrzeb" ]
            ]
        , HH.div [ HP.class_ (H.ClassName "grid md:grid-cols-3 gap-8 mb-16") ]
            [ packageCard false "Basic" "650" "zł"
                [ "Sesja ok. 1 godziny"
                , "Garderoba studyjna"
                , "5 wyedytowanych zdjęć"
                , "Dodatkowe: 70 zł"
                ]
            , packageCard true "Standard" "970" "zł"
                [ "Sesja ok. 1,5 godziny"
                , "Garderoba studyjna"
                , "10 wyedytowanych zdjęć"
                , "5 zdjęć czarno-białych"
                , "Dodatkowe: 80 zł"
                ]
            , packageCard false "Premium" "1350" "zł"
                [ "Sesja ok. 2 godzin"
                , "Garderoba studyjna"
                , "15 wyedytowanych zdjęć"
                , "Zdjęcia 15×21 w pudełku"
                , "Wysyłka paczkomatem"
                ]
            ]
        , HH.div [ HP.class_ (H.ClassName "max-w-2xl mx-auto text-center") ]
            [ HH.p [ HP.class_ (H.ClassName "font-body text-stone-500 mb-4") ]
                [ HH.text "Makijaż — 250 zł  •  Makijaż + stylizacja włosów — 350 zł" ]
            , HH.p [ HP.class_ (H.ClassName "font-body text-sm text-stone-400 italic") ]
                [ HH.text "Rezerwacja: bezzwrotny zadatek 100 zł" ]
            ]
        ]
    ]

packageCard :: forall cs m. Boolean -> String -> String -> String -> Array String -> H.ComponentHTML Action cs m
packageCard featured name price currency features =
  HH.article [ HP.class_ (H.ClassName if featured then "bg-stone-800 text-white p-10 relative" else "bg-white p-10 hover:shadow-2xl transition-shadow duration-500") ]
    [ if featured then HH.div [ HP.class_ (H.ClassName "absolute top-0 right-0 bg-accent text-white text-xs tracking-widest uppercase px-4 py-2") ] [ HH.text "Polecany" ] else HH.text ""
    , HH.h3 [ HP.class_ (H.ClassName "font-display text-2xl mb-6") ]
        [ HH.text name ]
    , HH.div [ HP.class_ (H.ClassName "mb-8 pb-8 border-b border-stone-200 last:border-0") ]
        [ HH.span [ HP.class_ (H.ClassName "font-display text-5xl") ] [ HH.text price ]
        , HH.span [ HP.class_ (H.ClassName "font-body text-sm ml-1") ] [ HH.text currency ]
        ]
    , HH.ul [ HP.class_ (H.ClassName "space-y-3 font-body text-sm") ] $
        map
          ( \f -> HH.li [ HP.class_ (H.ClassName "flex items-start gap-3") ]
              [ HH.span [ HP.class_ (H.ClassName if featured then "text-accent" else "text-accent") ] [ HH.text "—" ]
              , HH.text f
              ]
          )
          features
    ]

renderTestimonials :: forall cs m. H.ComponentHTML Action cs m
renderTestimonials =
  HH.section [ HP.id "testimonials", HP.class_ (H.ClassName "py-24 md:py-40 bg-white") ]
    [ HH.div [ HP.class_ (H.ClassName "max-w-7xl mx-auto px-8") ]
        [ HH.div [ HP.class_ (H.ClassName "text-center mb-20") ]
            [ HH.p [ HP.class_ (H.ClassName "font-body text-sm tracking-widest uppercase text-accent mb-6") ]
                [ HH.text "Opinie" ]
            , HH.h2 [ HP.class_ (H.ClassName "font-display text-4xl md:text-5xl text-stone-800 font-normal") ]
                [ HH.text "Co mówią klientki" ]
            ]
        , HH.div [ HP.class_ (H.ClassName "grid md:grid-cols-2 gap-12 md:gap-16") ]
            [ testimonial "Magdalena K."
                "Julia potrafi uchwycić emocje i stworzyć zdjęcia, które mają w sobie coś wyjątkowego. Atmosfera podczas sesji była swobodna i przyjemna."
            , testimonial "Izabela D."
                "Sesja przebiegła w bardzo miłej atmosferze. Pani Julia to osoba niezwykle profesjonalna, pełna pasji. Z pełnym przekonaniem polecam każdej przyszłej mamie!"
            , testimonial "Anna F."
                "Sesja ciążowa przerosła moje oczekiwania! Julia potrafiła uchwycić emocje i stworzyć piękne kadry, które będą cudowną pamiątką na całe życie."
            , testimonial "Sandra G."
                "Byłam u Julii na dwóch sesjach ciążowych, obie wyszły obłędnie. Julia jest przesympatyczna, atmosfera na sesji jest świetna - zero stresu."
            ]
        ]
    ]

testimonial :: forall cs m. String -> String -> H.ComponentHTML Action cs m
testimonial name text =
  HH.article [ HP.class_ (H.ClassName "relative pl-8 border-l-2 border-accent/30") ]
    [ HH.blockquote [ HP.class_ (H.ClassName "font-body text-stone-500 leading-relaxed italic mb-6") ]
        [ HH.p_ [ HH.text text ] ]
    , HH.footer [ HP.class_ (H.ClassName "font-body text-sm text-accent tracking-wider") ]
        [ HH.text $ "— " <> name ]
    ]

renderContact :: forall cs m. H.ComponentHTML Action cs m
renderContact =
  HH.section [ HP.id "contact", HP.class_ (H.ClassName "py-24 md:py-40 bg-cream") ]
    [ HH.div [ HP.class_ (H.ClassName "max-w-4xl mx-auto px-8") ]
        [ HH.div [ HP.class_ (H.ClassName "text-center mb-12") ]
            [ HH.p [ HP.class_ (H.ClassName "font-body text-sm tracking-widest uppercase text-accent mb-6") ]
                [ HH.text "Rezerwacja" ]
            , HH.h2 [ HP.class_ (H.ClassName "font-display text-4xl md:text-5xl text-stone-800 font-normal") ]
                [ HH.text "Zarezerwuj sesję" ]
            ]
        , HH.element (HH.ElemName "div")
            [ HP.id "cal-booking"
            , HP.class_ (H.ClassName "min-h-[600px] md:min-h-[700px] mb-12")
            ]
            []
        -- Contact info preserved below widget
        , HH.div [ HP.class_ (H.ClassName "flex flex-col md:flex-row justify-center items-center gap-8 md:gap-16 mb-8") ]
            [ HH.a [ HP.href (decodeBase64 obfuscatedTel), HP.class_ (H.ClassName "font-body text-stone-500 hover:text-accent transition-colors") ]
                [ HH.text (decodeBase64 obfuscatedTelText) ]
            , HH.a [ HP.href (decodeBase64 obfuscatedEmail), HP.class_ (H.ClassName "font-body text-stone-500 hover:text-accent transition-colors") ]
                [ HH.text (decodeBase64 obfuscatedEmailText) ]
            ]
        , HH.div [ HP.class_ (H.ClassName "flex justify-center gap-8") ]
            [ HH.a
                [ HP.href "https://www.instagram.com/julia_glinka_fotografia"
                , HP.target "_blank"
                , HP.rel "noopener noreferrer"
                , HP.class_ (H.ClassName "font-body text-sm tracking-widest uppercase text-stone-400 hover:text-stone-800 transition-colors border-b border-stone-300 hover:border-stone-800 pb-1")
                ]
                [ HH.text "Instagram" ]
            , HH.a
                [ HP.href (decodeBase64 obfuscatedEmail)
                , HP.class_ (H.ClassName "font-body text-sm tracking-widest uppercase text-stone-400 hover:text-stone-800 transition-colors border-b border-stone-300 hover:border-stone-800 pb-1")
                ]
                [ HH.text "Email" ]
            ]
        ]
    ]

renderFooter :: forall cs m. H.ComponentHTML Action cs m
renderFooter =
  HH.footer [ HP.class_ (H.ClassName "bg-stone-900 py-16 text-stone-400"), HP.attr (HH.AttrName "role") "contentinfo" ]
    [ HH.div [ HP.class_ (H.ClassName "max-w-7xl mx-auto px-8") ]
        [ HH.div [ HP.class_ (H.ClassName "flex flex-col md:flex-row justify-between items-center gap-8") ]
            [ HH.div [ HP.class_ (H.ClassName "text-center md:text-left") ]
                [ HH.h3 [ HP.class_ (H.ClassName "font-handwriting text-xl text-white mb-2") ]
                    [ HH.text "Julia Glinka" ]
                , HH.p [ HP.class_ (H.ClassName "font-body text-sm text-stone-500") ]
                    [ HH.text "Fotografia Ciążowa • Warszawa" ]
                ]
            , HH.p [ HP.class_ (H.ClassName "font-body text-xs text-stone-600") ]
                [ HH.text "© 2026 Wszelkie prawa zastrzeżone" ]
            ]
        ]
    ]

renderLightbox :: forall cs m. String -> H.ComponentHTML Action cs m
renderLightbox src =
  HH.div
    [ HP.class_ (H.ClassName "fixed inset-0 z-50 flex items-center justify-center bg-black/90 backdrop-blur-sm touch-pan-x")
    , HE.onClick \_ -> CloseLightbox
    , HE.onTouchStart \e -> TouchStart (getTouchX e)
    , HE.onTouchEnd \e -> TouchEnd (getTouchX e)
    ]
    [ HH.button
        [ HP.class_ (H.ClassName "absolute top-4 right-4 text-white text-4xl hover:text-accent transition-colors z-50")
        , HP.attr (HH.AttrName "aria-label") "Zamknij"
        , HE.onClick \_ -> CloseLightbox
        , HP.attr (HH.AttrName "onclick") "event.stopPropagation()"
        ]
        [ HH.text "✕" ]
    , HH.button
        [ HP.class_ (H.ClassName "absolute left-2 md:left-4 top-1/2 -translate-y-1/2 text-white text-3xl md:text-5xl hover:text-accent transition-colors p-2 md:p-4 z-50")
        , HP.attr (HH.AttrName "aria-label") "Poprzednie zdjęcie"
        , HE.onClick \_ -> PreviousImage
        , HP.attr (HH.AttrName "onclick") "event.stopPropagation()"
        ]
        [ HH.text "‹" ]
    , HH.button
        [ HP.class_ (H.ClassName "absolute right-2 md:right-4 top-1/2 -translate-y-1/2 text-white text-3xl md:text-5xl hover:text-accent transition-colors p-2 md:p-4 z-50")
        , HP.attr (HH.AttrName "aria-label") "Następne zdjęcie"
        , HE.onClick \_ -> NextImage
        , HP.attr (HH.AttrName "onclick") "event.stopPropagation()"
        ]
        [ HH.text "›" ]
    , HH.img
        [ HP.src src
        , HP.alt "Zdjęcie w pełnym rozmiarze"
        , HP.class_ (H.ClassName "max-w-[90vw] max-h-[90vh] object-contain cursor-pointer select-none")
        , HP.attr (HH.AttrName "draggable") "false"
        , HP.attr (HH.AttrName "onclick") "event.stopPropagation()"
        ]
    ]

nextImageIndex :: Int -> Int -> Int
nextImageIndex currentIdx len =
  if currentIdx >= len - 1 then 0 else currentIdx + 1

prevImageIndex :: Int -> Int -> Int
prevImageIndex currentIdx len =
  if currentIdx <= 0 then len - 1 else currentIdx - 1

swipeThreshold :: Number
swipeThreshold = 50.0

data SwipeDirection = SwipeLeft | SwipeRight | NoSwipe

derive instance eqSwipeDirection :: Eq SwipeDirection
derive instance ordSwipeDirection :: Ord SwipeDirection

instance showSwipeDirection :: Show SwipeDirection where
  show SwipeLeft = "SwipeLeft"
  show SwipeRight = "SwipeRight"
  show NoSwipe = "NoSwipe"

determineSwipe :: Number -> Number -> SwipeDirection
determineSwipe touchStartX touchEndX =
  let
    diff = touchStartX - touchEndX
  in
    if diff > swipeThreshold then SwipeLeft
    else if diff < -swipeThreshold then SwipeRight
    else NoSwipe

getTouchX :: TouchEvent -> Number
getTouchX e = maybe 0.0 (\t -> toNumber $ T.clientX t) (TL.item 0 (touches e))

handleAction :: forall cs o m. MonadEffect m => Action -> H.HalogenM State Action cs o m Unit
handleAction = case _ of
  ToggleMenu -> do
    H.modify_ \st -> st { menuOpen = not st.menuOpen }
  OpenLightbox src -> do
    let index = maybe 0 identity (findIndex (_ == src) galleryImages)
    H.modify_ \st -> st { lightboxImage = src, currentImageIndex = index }
  CloseLightbox -> do
    H.modify_ \st -> st { lightboxImage = "" }
  NextImage -> do
    H.modify_ \st ->
      let
        len = length galleryImages
        idx = nextImageIndex st.currentImageIndex len
        nextImage = galleryImages !! idx
      in
        st { lightboxImage = maybe st.lightboxImage identity nextImage, currentImageIndex = idx }
  PreviousImage -> do
    H.modify_ \st ->
      let
        len = length galleryImages
        idx = prevImageIndex st.currentImageIndex len
        prevImage = galleryImages !! idx
      in
        st { lightboxImage = maybe st.lightboxImage identity prevImage, currentImageIndex = idx }
  TouchStart x -> do
    H.modify_ \st -> st { touchStartX = x }
  TouchEnd x -> do
    st <- H.get
    case determineSwipe st.touchStartX x of
      SwipeLeft -> do
        let
          len = length galleryImages
          idx = nextImageIndex st.currentImageIndex len
          nextImage = galleryImages !! idx
        H.modify_ \s -> s { lightboxImage = maybe s.lightboxImage identity nextImage, currentImageIndex = idx }
      SwipeRight -> do
        let
          len = length galleryImages
          idx = prevImageIndex st.currentImageIndex len
          prevImage = galleryImages !! idx
        H.modify_ \s -> s { lightboxImage = maybe s.lightboxImage identity prevImage, currentImageIndex = idx }
      NoSwipe -> pure unit
