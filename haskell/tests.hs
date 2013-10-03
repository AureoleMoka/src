data NotColors = Pink | Yellow
instance Eq NotColors where
    Pink == Yellow = True
    Yellow == Pink = True
    _ == _         = False

class YesNo a where
    yesno :: a -> Bool
instance YesNo [Char] where
    yesno "oui" = True
    yesno _     = False
