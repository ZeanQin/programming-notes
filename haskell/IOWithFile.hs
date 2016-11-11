import System.IO

main = 
  withFile "./zean.txt" ReadMode (\handle -> do
    -- You can control how exactly buffering is done by using the hSetBuffering
    -- function. It takes a handle and a BufferMode and returns an I/O action 
    -- that sets the buffering. BufferMode is a simple enumeration data type 
    -- and the possible values it can hold are: NoBuffering, LineBuffering 
    -- or BlockBuffering (Maybe Int). The Maybe Int is for how big the chunk 
    -- should be, in bytes. If it's Nothing, then the operating system 
    -- determines the chunk size. NoBuffering means that it will be read one 
    -- character at a time. NoBuffering usually sucks as a buffering mode because
    -- it has to access the disk so much.
    --
    -- We can also use hFlush, which is a function that takes a handle and returns 
    -- an I/O action that will flush the buffer of the file associated with the 
    -- handle. When we're doing line-buffering, the buffer is flushed after every 
    -- line. When we're doing block-buffering, it's after we've read a chunk. 
    -- It's also flushed after closing a handle. That means that when we've reached
    -- a newline character, the reading (or writing) mechanism reports all the data 
    -- so far. But we can use hFlush to force that reporting of data that has been 
    -- read so far. After flushing, the data is available to other programs that are running at the same time.
    hSetBuffering handle $ BlockBuffering (Just 2048)
    contents <- hGetContents handle
    putStr contents)
