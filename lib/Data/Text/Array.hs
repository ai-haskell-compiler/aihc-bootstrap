module Data.Text.Array where

-- The array of a text in the package text. Here a text is a ByteString
-- (Data.Text), and AIHC only uses the array to ask, with
-- reallyUnsafePtrEquality#, whether two texts share their bytes. The
-- answer is always no, which is correct: the texts are then compared.
data Array = ByteArray ()
