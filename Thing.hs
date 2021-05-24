import Control.Applicative

data Case = Nominative | Accusative | Dative | Genitive deriving (Show, Eq)
data Number = Singular | Plural deriving (Show, Eq)
data Gender = Masculine | Feminine | Neuter deriving (Show, Eq)
data Person = FirstPerson | SecondPerson | ThirdPerson deriving (Show, Eq)
data NounPrefix = Das | Ein | Kein | Mein Person Number | NoPrefix deriving (Show, Eq)
data Noun = Noun { engN :: String, gender :: Gender, deuN :: Number -> Case -> String }

data EngStr = EngStr { fromEng :: String } deriving (Eq, Ord, Show)
data DeuStr = DeuStr { fromDeu :: String } deriving (Eq, Ord, Show)


embedEng1 :: (String -> String) -> (EngStr -> EngStr)
embedEng1 f = EngStr . f . fromEng

embedEng2 :: (String -> String -> String) -> (EngStr -> EngStr -> EngStr)
embedEng2 f a b = EngStr $ f (fromEng a) (fromEng b)

embedDeu1 :: (String -> String) -> (DeuStr -> DeuStr)
embedDeu1 f = DeuStr . f . fromDeu

embedDeu2 :: (String -> String -> String) -> (DeuStr -> DeuStr -> DeuStr)
embedDeu2 f a b = DeuStr $ f (fromDeu a) (fromDeu b)


instance Semigroup EngStr where
  (<>) = embedEng2 (<>)

instance Semigroup DeuStr where
  (<>) = embedDeu2 (<>)

instance Monoid EngStr where
  mempty = EngStr mempty

instance Monoid DeuStr where
  mempty = DeuStr mempty


engDeuDirect :: EngStr -> DeuStr
engDeuDirect = DeuStr . fromEng

deuEngDirect :: DeuStr -> EngStr
deuEngDirect = EngStr . fromDeu

-- proof by cases shows that this function is total:
-- [nounPrefixEng p n c | p <- [Das,Ein,Kein,NoPrefix], n <- [Singular, Plural], c <- [Nominative,Accusative,Dative,Genitive]]
nounPrefixEng :: NounPrefix -> Number -> Case -> Maybe EngStr
nounPrefixEng Das Singular Nominative = pure $ EngStr "der"
nounPrefixEng Das Singular Accusative = pure $ EngStr "den"
nounPrefixEng Das Singular Dative     = pure $ EngStr "dem"
nounPrefixEng Das Singular Genitive   = pure $ EngStr "des"
nounPrefixEng Das Plural   Nominative = pure $ EngStr "die"
nounPrefixEng Das Plural   Accusative = pure $ EngStr "dien"
nounPrefixEng Das Plural   Dative     = pure $ EngStr "diem"
nounPrefixEng Das Plural   Genitive   = pure $ EngStr "dies"
nounPrefixEng Ein Singular Nominative = pure $ EngStr "ein"
nounPrefixEng Ein Singular Accusative = pure $ EngStr "einen"
nounPrefixEng Ein Singular Dative     = pure $ EngStr "einem"
nounPrefixEng Ein Singular Genitive   = pure $ EngStr "eines"
nounPrefixEng Ein Plural   _          = empty
nounPrefixEng Kein a b = (EngStr "k" <>) <$> nounPrefixEng Ein a b
nounPrefixEng NoPrefix Singular Nominative = pure $ EngStr "r"
nounPrefixEng NoPrefix Singular Accusative = pure $ EngStr "n"
nounPrefixEng NoPrefix Singular Dative     = pure $ EngStr "m"
nounPrefixEng NoPrefix Singular Genitive   = pure $ EngStr "s"
nounPrefixEng NoPrefix Plural   cas        = (<> EngStr "s") <$> nounPrefixEng NoPrefix Singular cas

-- proof by cases shows that this function is total:
-- [nounPrefixDeu p g n c | p <- [Das,Ein,Kein,NoPrefix], g <- [Masculine,Feminine,Neuter], n <- [Singular, Plural], c <- [Nominative,Accusative,Dative,Genitive]]
nounPrefixDeu :: NounPrefix -> Gender -> Number -> Case -> Maybe DeuStr
nounPrefixDeu NoPrefix _ _ _ = mempty
nounPrefixDeu Kein Feminine a b = (DeuStr "k" <>) <$> nounPrefixDeu Ein Feminine a b
nounPrefixDeu Das _         Plural   Accusative = pure $ DeuStr "die"
nounPrefixDeu Das _         Plural   Dative     = pure $ DeuStr "den"
nounPrefixDeu Das _         Plural   Genitive   = pure $ DeuStr "der"
nounPrefixDeu pre _         Plural   cse        = engDeuDirect <$> nounPrefixEng pre Plural cse
nounPrefixDeu pre Masculine Singular cse        = engDeuDirect <$> nounPrefixEng pre Singular cse
nounPrefixDeu Das Neuter    Singular Nominative = pure $ DeuStr "das"
nounPrefixDeu pre Neuter    Singular Accusative = nounPrefixDeu pre Neuter    Singular Nominative
nounPrefixDeu pre Neuter    Singular cse        = nounPrefixDeu pre Masculine Singular cse
nounPrefixDeu Das Feminine  Singular Nominative = pure $ DeuStr "die"
nounPrefixDeu Das Feminine  Singular Accusative = pure $ DeuStr "die"
nounPrefixDeu Das Feminine  Singular _          = pure $ DeuStr "der"
nounPrefixDeu Ein Feminine  Singular Nominative = pure $ DeuStr "eine"
nounPrefixDeu Ein Feminine  Singular Accusative = pure $ DeuStr "eine"
nounPrefixDeu Ein Feminine  Singular _          = pure $ DeuStr "einer"
