{-
LOSSLESS DATA COMPRESSION
Huffman encoding
-}

data Bit = Zero | One

instance Show Bit where
    show Zero = "0"
    show One = "1"

instance Eq Bit where
    Zero == Zero = True
    One == One = True
    _ == _ = False

instance Ord Bit where
    compare Zero One = compare 0 1
    compare One Zero = compare 1 0