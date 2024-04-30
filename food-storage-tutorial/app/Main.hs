module Main (main) where

import System.IO ( hFlush, stdout, hPutStrLn, stderr )
import Data.Char (isDigit)
import Data.List (find)
import Data.List.Split (splitOn)
import Data.Time.Calendar.OrdinalDate ( Day )
import Data.Time
    ( getCurrentTime, defaultTimeLocale, parseTimeM, UTCTime(utctDay) )
import Control.Exception (catch, IOException)
import Control.Monad ( forM_ )
import qualified System.IO.Strict (readFile)
import Text.Read (readMaybe)
import System.Console.ANSI ( clearScreen, setCursorPosition )
import Text.Printf ( printf )
import System.Directory (doesFileExist)
import System.Exit (exitSuccess)

data FoodRecord = FoodRecord
    {
        foodID :: Integer,
        category :: FoodCategory,
        name :: String,
        stock :: Int,
        expDate :: Day
    } deriving (Show, Eq)

data FoodCategory = Dairy
    | Meat
    | Vegetable
    | Fruit
    deriving (Show, Eq)


---------------------- CRUD HELPER FUNCTIONS ----------------------------------------------------------

csvFile :: String
csvFile = "inventory.csv"

loadFileStrict :: FilePath -> IO String
loadFileStrict = System.IO.Strict.readFile


extractFields :: String -> (Integer, FoodCategory, String, Int, Day)
extractFields line =
    -- splits the input string line into a list of strings
    let fields = splitOn "," line

        -- `!!` is used to access elements using their index; similar to `[]` in imperative languages
        foodIDField = read (head fields) :: Integer
        categoryField = toCategoryStr (fields !! 1) :: FoodCategory
        nameField = fields !! 2
        stockField = read (fields !! 3) :: Int
        expDateField = read (fields !! 4) :: Day
    in (foodIDField, categoryField, nameField, stockField, expDateField)
    where
        toCategoryStr :: String -> FoodCategory
        toCategoryStr str = case str of
            "Dairy" -> Dairy
            "Meat" -> Meat
            "Vegetable" -> Vegetable
            "Fruit" -> Fruit
            _ -> error "| ERROR: Invalid choice!"


formatRecord :: FoodRecord -> String
-- converts a `FoodRecord` to a `String` adds commas to each field.
formatRecord food = concat [show (foodID food), ",", show (category food), ",", name food, ",", show (stock food), ",", show (expDate food)]


parseToRecord :: String -> FoodRecord
parseToRecord line =
    let (foodID, category, name, stock, expDate) = extractFields line
    in FoodRecord foodID category name stock expDate


createCsvFile :: FilePath -> IO ()
createCsvFile file = do
    -- checks if the file exists
    fileExists <- doesFileExist file

    if not fileExists
        -- if file does not exist, create one
        then writeFile file ""

        -- else, print that it already exists
        else putStrLn "| INFO: CSV file already exists"


---------------------- ADDING RECORDS ----------------------------------------------------------

appendRecord :: FoodRecord -> IO ()
appendRecord food = do
    -- appends a newline so that records don't print a single line
    -- also applies the `formatRecord` function which parses the FoodRecord to a String
    -- to write it to the file.
    let recordString = formatRecord food ++ "\n"

    -- appends (adds) the record to the files
    appendFile csvFile recordString

addRecord :: IO ()
addRecord = do
    clearScreen
    setCursorPosition 0 0
    printMenuHeader "Add"
    confirm <- getInput "| Do you want to add a food record? (y/n): "

    if confirm == "y" || confirm == "Y"
        then do
        id <- getID
        category <- getCategory
        name <- getName
        stock <- getStock
        expDate <- getExpDate


        -- create food object based on input
        let food = FoodRecord id category name stock expDate

        -- input created record in database
        appendRecord food

        -- test display
        printMenuHeader "Food Info"

        putStrLn $ "| ID: " ++ show id
        putStrLn $ "| Selected category: " ++ show category
        putStrLn $ "| Food name: " ++ name
        putStrLn $ "| Stock: " ++ show stock
    else if confirm == "n" || confirm == "N"
        then do
            mainMenu
    else putStrLn "| Invalid choice!" >> addRecord

---------------------- READING RECORDS ----------------------------------------------------------

readCsvFile :: FilePath -> IO [FoodRecord]
readCsvFile file = do
    -- loads the file eagerly (via loadFileStrict) to avoid lazy evaluation problems
    -- catch any read errors if there are any
    csvData <- loadFileStrict file `catch` handleReadError

    -- `(lines csvData)` creates a list of Strings separated by newline characters from the given String
    -- the `parseToRecord` is applied to each element in the list
    return $ map parseToRecord (lines csvData)
    where
        handleReadError :: IOException -> IO String
        handleReadError exception = do
            hPutStrLn stderr $ "| ERROR: " ++ show exception
            putStrLn "| ERROR: An error occurred while reading the CSV file. Please check if the file exists and has the correct permissions."
            return ""

---------------------- DISPLAYING RECORDS -------------------------------------------------------

tableCell :: String
tableCell = "| %-6s | %-9s | %-49s | %-10s | %-10s |"

displayRecords :: IO ()
displayRecords = do
    clearScreen
    setCursorPosition 0 0
    printMenuHeader "Display"

    csvData <- readCsvFile csvFile

    if null csvData
        then putStrLn "| INFO: There are no records in the database. Please add a record first."
        else do
            let table_header = printf tableCell "FoodID" "Category" "Name" "Stock" "Exp Date"

            putStrLn table_header
            putStrLn ("+" ++ replicate 8 '-' ++ "+" ++ replicate 11 '-' ++ "+" ++ replicate 51 '-' ++ "+" ++ replicate 12 '-' ++ "+" ++ replicate 12 '-' ++ "+")

            recordRows <- mapM printRecordRow csvData
            putStr $ unlines recordRows

            putStrLn ("+" ++ replicate 8 '-' ++ "+" ++ replicate 11 '-' ++ "+" ++ replicate 51 '-' ++ "+" ++ replicate 12 '-' ++ "+" ++ replicate 12 '-' ++ "+")

printRecordRow :: FoodRecord -> IO String
printRecordRow (FoodRecord id category name stock expDate) = do
    currentDay <- getCurrentDate
    let nameWithAsterisk
            | expDate < currentDay = name ++ "*"
            | otherwise = name
    return $ printf tableCell
        (show id)
        (show category)
        nameWithAsterisk
        (show stock)
        (show expDate)


---------------------- EDITING RECORDS ----------------------------------------------------------
searchRecordID :: Integer -> IO (Maybe FoodRecord)
searchRecordID searchID = do
    -- read the csv file
    records <- readCsvFile csvFile

    -- find the record with the matching ID
    let matchingRecord = Data.List.find (\record -> foodID record == searchID) records
    return matchingRecord


-- takes a function, the new value, a FoodRecord, and a string (for msgs) as parameters
updateRecord :: (a -> FoodRecord -> FoodRecord) -> a -> FoodRecord -> String -> IO ()
updateRecord updateFunc newValue record message = do
    csvData <- readCsvFile csvFile

    -- applies the updateFunc to newValue and record to create an updated record.
    let updatedRecord = updateFunc newValue record

    -- It uses the `map` function to apply the lambda function to each record in the list.
    let updatedCsvData = map (\r -> if r == record then updatedRecord else r) csvData

    -- This line writes the updated records back to the CSV file. 
    writeRecordsToFile csvFile updatedCsvData
    putStrLn ("| INFO: " ++ message ++ " updated successfully.")


updateCategory :: FoodCategory -> FoodRecord -> FoodRecord
updateCategory newCategory record = record { category = newCategory }

updateName :: String -> FoodRecord -> FoodRecord
updateName newName record = record { name = newName }

updateStock :: Int -> FoodRecord -> FoodRecord
updateStock newStock record = record { stock = newStock }

updateExpDate :: Day -> FoodRecord -> FoodRecord
updateExpDate newExpDate record = record { expDate = newExpDate }

editRecordDetails :: FoodRecord -> IO ()
editRecordDetails record = do
    putStrLn ('+' : replicate 98 '-' ++ "+")
    putStrLn ("| [1]. Category" ++ replicate 84 ' ' ++ "|")
    putStrLn ("| [2]. Name" ++ replicate 88 ' ' ++ "|")
    putStrLn ("| [3]. Stock" ++ replicate 87 ' ' ++ "|")
    putStrLn ("| [4]. Expiration Date" ++ replicate 77 ' ' ++ "|")
    putStrLn ('+' : replicate 98 '-' ++ "+")
    choiceInput <- selectMenuChoice "| Select the attribute you want to edit: " [1, 2, 3, 4]

    -- basically .get() the field name and writes it to file thus updating it
    case choiceInput of
        1 -> getCategory >>= \newCategory -> updateRecord updateCategory newCategory record "Category"
        2 -> getName >>= \newName -> updateRecord updateName newName record "Name"
        3 -> getStock >>= \newStock -> updateRecord updateStock newStock record "Stock"
        4 -> getExpDate >>= \newExpDate -> updateRecord updateExpDate newExpDate record "Expiration Date"
        _ -> putStrLn "| ERROR: Invalid choice."

writeRecordsToFile :: FilePath -> [FoodRecord] -> IO ()
writeRecordsToFile filePath records = do
    -- formats the records into `String` using the `formatRecord` function
    let csvLines = map formatRecord records
    writeFileWithErrorHandling filePath (unlines csvLines)

    where
        writeFileWithErrorHandling :: FilePath -> String -> IO ()
        writeFileWithErrorHandling fp content = writeFile fp content `catch` handleWriteError
        handleWriteError :: IOException -> IO ()
        handleWriteError exception = do
            hPutStrLn stderr $ "| ERROR: Error writing to file '" ++ filePath ++ "': " ++ show exception
            putStrLn "| ERROR: An error occurred while writing to the CSV file. Please check the file permissions and try again."


editRecord :: IO ()
editRecord = do
    clearScreen
    setCursorPosition 0 0
    printMenuHeader "Edit"
    csvData <- readCsvFile csvFile

    if null csvData
        then putStrLn "| INFO: There are no records in the database. Please add a record first."
        else do
            confirm <- getInput "| Do you want to edit a food record? (y/n): "

            if confirm == "y" || confirm == "Y"
                then do
                    displayRecords
                    idToEdit <- getInput "| Enter the ID of the record you want to edit: "
                    if all isDigit idToEdit
                        then do
                            let realID = read idToEdit
                            maybeRecord <- searchRecordID realID
                            case maybeRecord of
                                Just record -> editRecordDetails record
                                Nothing -> putStrLn "| INFO: Record not found."
                        else putStrLn "| ERROR: Invalid ID. Please enter a valid integer ID." >> editRecord
            else if confirm == "n" || confirm == "N"
                then do
                    mainMenu
            else putStrLn "| ERROR: Invalid input!" >> editRecord

---------------------- DELETING RECORDS ---------------------------------------------------------
deleteRecordFromFile :: Integer -> IO ()
deleteRecordFromFile idToDelete = do
    csvData <- readCsvFile csvFile
    let updatedRecords = filter (\r -> foodID r /= idToDelete) csvData
    writeRecordsToFile csvFile updatedRecords
    putStrLn "| INFO: Record deleted successfully."


deleteRecord :: IO ()
deleteRecord = do
    clearScreen
    setCursorPosition 0 0
    displayRecords
    printMenuHeader "Delete"

    csvData <- readCsvFile csvFile

    if null csvData
        then putStrLn "| INFO: There are no records in the database. Please add a record first."
        else do
            -- Allows the user to go back
            confirm <- getInput "| Do you want to delete a food record? (y/n): "

            if confirm == "y" || confirm == "Y"
                then do
                displayRecords
                idToDelete <- getInput "| Enter the ID of the record you want to delete:"

                -- checks if idToDelete is a number
                if all isDigit idToDelete
                    then do

                        -- convert the idToDelete into an integer
                        let realID = read idToDelete

                        -- returns a record if there is one
                        maybeRecord <- searchRecordID realID


                        case maybeRecord of
                            -- if there is a record, then:
                            Just record -> do
                                putStrLn $ "| Record found: " ++ show record
                                confirmDelete <- getInput "| Are you sure you want to delete this record? (y/n)"
                                if confirmDelete == "y"
                                    then deleteRecordFromFile realID
                                    else putStrLn "| INFO: Record deletion cancelled."
                            -- else
                            Nothing -> putStrLn "| ERROR: Record not found."
                    else putStrLn "| ERROR: Invalid ID. Please enter a valid integer ID." >> deleteRecord
            else if confirm == "n" || confirm == "N"
                then do
                    mainMenu
            else putStrLn "| ERROR: Invalid choice!" >> deleteRecord


---------------------- REMOVING SPOILED RECORDS -------------------------------------------------

filterFresh :: [FoodRecord] -> IO [FoodRecord]
filterFresh records = do
    -- get the current date
    currentDate <- getCurrentDate

    -- filter the dates in the list that are >= current date
    let freshItems = filter (\r -> expDate r >= currentDate) records
    return freshItems

removeSpoiledItems :: IO ()
removeSpoiledItems = do
    clearScreen
    setCursorPosition 0 0

    printMenuHeader "Spoiled"
    csvData <- readCsvFile csvFile

    if null csvData
        then putStrLn "| INFO: There are no records in the database. Please add a record first."
        else do
            removeItem <- getInput "| Do you want to spoiled food records? (y/n): "

            if removeItem == "Y" || removeItem == "y"
                then do
                    csvData <- readCsvFile csvFile

                    -- filter fresh (expDate >= current date) in the record
                    freshItems <- filterFresh csvData

                    -- overwrite the file with only the fresh items thus removing the `spoilt` items
                    writeRecordsToFile csvFile freshItems
                    displayRecords
            else if removeItem == "N" || removeItem == "n"
                then do
                    mainMenu
            else putStrLn "| ERROR: Invalid choice!" >> deleteRecord


---------------------- DISPLAY STOCK LEVELS  ----------------------------------------------------
stockCell :: String
stockCell = "| %-30s | %-22s | %-20s | %-15s |\n"

displayStockLevels :: IO ()
displayStockLevels = do
    clearScreen
    setCursorPosition 0 0

    printMenuHeader "Stock"

    csvData <- readCsvFile csvFile

    if null csvData
        then putStrLn "| INFO: There are no records in the database. Please add a record first."
        else do
            currentDay <- getCurrentDate
            let requiredStockLevel = 100

            putStrLn ("+" ++ replicate 32 '-' ++ "+" ++ replicate 24 '-' ++ "+" ++ replicate 22 '-' ++ "+" ++ replicate 17 '-' ++ "+")
            printf stockCell "Name" "Current Stock Level" "Required Stock Level" "Restock Number"
            putStrLn ("+" ++ replicate 32 '-' ++ "+" ++ replicate 24 '-' ++ "+" ++ replicate 22 '-' ++ "+" ++ replicate 17 '-' ++ "+")

            forM_ csvData $ \record -> do
                let currentStock = stock record
                    restockNumber = requiredStockLevel - currentStock
                    nameWithAsterisk
                        | expDate record < currentDay = name record ++ "*"
                        | otherwise = name record

                if restockNumber >= 0
                    then do
                        printf stockCell nameWithAsterisk (show currentStock) (show requiredStockLevel) (show restockNumber)
                    else
                        printf stockCell nameWithAsterisk (show currentStock) (show requiredStockLevel) "N/A"
            putStrLn ("+" ++ replicate 32 '-' ++ "+" ++ replicate 24 '-' ++ "+" ++ replicate 22 '-' ++ "+" ++ replicate 17 '-' ++ "+")

            forM_ csvData $ \record -> do
                let currentStock = stock record
                    nameWithAsterisk
                        | expDate record < currentDay = name record ++ "*"
                        | otherwise = name record
                putStr "| INFO: "
                putStr nameWithAsterisk
                if currentStock < requiredStockLevel
                    then putStrLn " needs to be restocked."
                    else putStrLn " does not need to be restocked."

---------------------- NON CRUD OPERATION FUNCTIONS ---------------------------------------------
getCurrentDate :: IO Day
getCurrentDate = utctDay <$> getCurrentTime

printMenuHeader :: String -> IO()
printMenuHeader header = do
    let topBotLine = putStrLn ('+' : replicate 98 '-' ++ "+")
    let spacerLine = putStrLn ('|' : replicate 98 ' '  ++  "|")
    case header of
        "Add" -> do
                topBotLine
                spacerLine
                putStrLn "|\t\t\t\t\tAdd Food Record\t\t\t\t\t\t   |"
                spacerLine
                topBotLine
        "Edit" -> do
                topBotLine
                spacerLine
                putStrLn "|\t\t\t\t\tEdit Food Record\t\t\t\t\t   |"
                spacerLine
                topBotLine
        "Display" -> do
                topBotLine
                spacerLine
                putStrLn "|\t\t\t\t\tDisplay Food Record\t\t\t\t\t   |"
                spacerLine
                topBotLine
        "Delete" -> do
                topBotLine
                spacerLine
                putStrLn "|\t\t\t\t\tDelete Food Record\t\t\t\t\t   |"
                spacerLine
                topBotLine
        "Food Info" -> do
                topBotLine
                spacerLine
                putStrLn "|\t\t\t\t\tFood Information\t\t\t\t\t   |"
                spacerLine
                topBotLine
        "Main" -> do
                topBotLine
                spacerLine
                putStrLn "|\t\t\t\t\tHaskell Food Storage\t\t\t\t\t   |"
                spacerLine
                topBotLine
        "Spoiled" -> do
                topBotLine
                spacerLine
                putStrLn "|\t\t\t\t\tRemove Spoiled Items\t\t\t\t\t   |"
                spacerLine
                topBotLine
        "Stock" -> do
                topBotLine
                spacerLine
                putStrLn "|\t\t\t\t\tDiplay Stock Levels\t\t\t\t\t   |"
                spacerLine
                topBotLine
        _ -> error "| ERROR: Invalid argument!"


selectMenuChoice :: String -> [Int] ->  IO Int
selectMenuChoice prompt list = do
    choice <- getInput prompt

    let maybeChoice = readMaybe choice :: Maybe Int

    case maybeChoice of
        Just choiceInt -> if choiceInt `elem` list
            then return choiceInt
            else putStrLn "| ERROR: Invalid input. Please input an integer." >> selectMenuChoice prompt list
        Nothing -> putStrLn "| ERROR: Invalid input. Please input an integer." >> selectMenuChoice prompt list

-- Function to prompt user for input, (removes buffer)
getInput :: String -> IO String
getInput prompt = do
    putStr prompt
    hFlush stdout
    getLine

getCategory :: IO FoodCategory
getCategory = do
    putStrLn ('+' : replicate 98 '-' ++ "+")
    putStrLn ("| [Food Categories]" ++ replicate 80 ' ' ++ "|")
    putStrLn ("| [1] Dairy" ++ replicate 88 ' ' ++ "|")
    putStrLn ("| [2] Meat" ++ replicate 89 ' ' ++ "|")
    putStrLn ("| [3] Vegetable" ++ replicate 84 ' ' ++ "|")
    putStrLn ("| [4] Fruit" ++ replicate 88 ' ' ++ "|")
    putStrLn ('+' : replicate 98 '-' ++ "+")
    categoryInput <- selectMenuChoice "| Select Food Category: " [1, 2, 3, 4]

    return $ toCategoryInt categoryInput
    where
        -- maps the integer input from the choice to the Category datatype
        toCategoryInt :: Int -> FoodCategory
        toCategoryInt int = case int of
            1 -> Dairy
            2 -> Meat
            3 -> Vegetable
            4 -> Fruit
            _ -> error "| ERROR: Invalid choice!"


getStock :: IO Int
getStock = do
    putStrLn ('+' : replicate 98 '-' ++ "+")
    stock <- getInput "| Please input stock of food: "

    if all isDigit stock && read stock >= (0 :: Int)
        then return (read stock)
        else putStrLn "| ERROR: Invalid input. Please enter a valid non-negative number for stock." >> getStock

-- Function to prompt user for the name of food item
getName :: IO String
getName = do
    putStrLn ('+' : replicate 98 '-' ++ "+")
    getInput "| Please input name of food: "

getID :: IO Integer
getID = do
    csvData <- readCsvFile csvFile
    let recordIds = map foodID csvData
    let maxID = if null recordIds
        then 0
        else maximum recordIds
    return $ maxID + 1


getExpDate :: IO Day
getExpDate = do
    putStrLn ('+' : replicate 98 '-' ++ "+")
    date <- getInput "| Please input expiration date (YYYY-MM-DD):"
    currentDay <- getCurrentDate

    case parseTimeM True defaultTimeLocale "%Y-%m-%d" date of
        Nothing -> putStrLn "| ERROR: Invalid Input. Enter a valid date." >> getExpDate
        Just day ->
            if day < currentDay
                then putStrLn "| ERROR: Expiration date cannot be earlier than today." >> getExpDate
                else return day


mainMenu :: IO ()
mainMenu = do
    printMenuHeader "Main"

    putStrLn "| [1] Add New Food Record \t\t\t\t\t\t\t\t\t   |"
    putStrLn "| [2] Edit Food Record \t\t\t\t\t\t\t\t\t\t   |"
    putStrLn "| [3] Delete Food Record \t\t\t\t\t\t\t\t\t   |"
    putStrLn "| [4] Display All Records \t\t\t\t\t\t\t\t\t   |"
    putStrLn "| [5] Remove Spoiled Items \t\t\t\t\t\t\t\t\t   |"
    putStrLn "| [6] Display Stock Levels \t\t\t\t\t\t\t\t\t   |"
    putStrLn "| [7] Exit Program \t\t\t\t\t\t\t\t\t\t   |"

    putStrLn ('+' : replicate 98 '-' ++ "+")
    option <- selectMenuChoice "Select an option: " [1, 2, 3, 4, 5, 6, 7]
    case option of
        1 -> addRecord >> mainMenu
        2 -> editRecord >> mainMenu
        3 -> deleteRecord >> mainMenu
        4 -> displayRecords >> mainMenu
        5 -> removeSpoiledItems >> mainMenu
        6 -> displayStockLevels >> mainMenu
        7 -> exitSuccess
        _   -> putStrLn "Invalid option. Please select again." >> mainMenu


main :: IO ()
main = do
    createCsvFile csvFile
    mainMenu
