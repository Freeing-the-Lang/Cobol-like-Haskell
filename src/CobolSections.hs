module CobolSections where
import CobolRuntime

-- COBOL-like section names with actions
perform :: String -> IO () -> IO ()
perform sectionName action = do
    display ("------------------------------")
    display ("ENTERING SECTION: " ++ sectionName)
    action
    display ("EXIT SECTION: " ++ sectionName)

-- COBOL-like simple calculation
computeDouble :: Int -> Int
computeDouble x = x * 2
