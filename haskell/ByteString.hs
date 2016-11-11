-- ByteString reduces the laziness or removes it completely from lists. It's useful when we need high performance 
-- string operations. 
--
-- Can write program using normal String and re-write it in bytestring

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S
