# The Haskell CRUD App Tutorial I Wish I Had

## Rationale


For our Design and Implementation of Programming Language (DIPROGLANG) course, we had to pick one language to work with for the entire semester. I was intrigued by declarative programming languages and decided to challenge myself by choosing Haskell. Unfortunately, the lack of documentation made it difficult to learn. As a student with less than a week to learn the language, its tooling, and complete a final project, it was a nightmare. Hence, I created a tutorial to help others who may find themselves in a similar situation. 

Disclaimer: Please note that this tutorial is not exhaustive, but rather a reference for those who may need it.

## Installation.  
Follow the [instructions](https://www.haskell.org/downloads/) and recommendations on how to install Haskell on your device.

## Prerequisites

I suggest that you check out Philipp Hagenlocher's playlist called ["Haskell for Imperative Programmers"](https://www.youtube.com/watch?v=Vgu82wiiZ90&list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV). It provides a comprehensive guide to learning Haskell's syntax. Although most Haskell tutorials use Cabal to manage projects, I find Stack to be more user-friendly. Therefore, in this tutorial, we will be using Stack. You should ensure that Stack was installed when you installed Haskell. However, one disadvantage of Stack is that some packages are not available in its repository. But don't worry, we will discuss a workaround for this later.

## Setting the Project Environment

Before starting the project, we need to create a project directory. This will make installing and managing dependencies easier in the long run. Create a dedicated folder for the project. In my case, I’ll call it food-storage-tutorial. Enter the `stack new <directory-name>` command in your terminal and this should create and initialize in the directory name you provided.

```  
stack new food-storage-tutorial  
```

Congrats! You now have a project directory. I know that it can be a little overwhelming since there are a lot of files to look at right now, but some of those are just boilerplate. To build or run the program, one would usually use the `ghc Main.hs` command. However, this is very tedious as you must run this command every time you want to build the executable. 

Thankfully, Stack provides us with the `stack install --file-watch` command which builds the executable every time we save the file. This is a time saver for us since the Haskell extension does not provide us with a debugger (as far as I know).

This will also give us warning and error messages while building our executable which is helpful to debug our program. It’s good to **read the error messages** provided by Stack because they may contain solutions, particularly when it comes to managing dependencies.

## Managing Dependencies

While doing this program, you will eventually install libraries and add them as dependencies. As I have mentioned earlier, some of the files that were generated when entering the ` stack new food-storage-tutorial` are just boilerplate. However, three (3) important files in there that are used for managing dependencies, namely: `package.yaml`, `<your-directory-name>.cabal` and `stack.yaml`.  
  
Here’s mine:

```  
package.yaml
food-storage-tutorial.cabal
stack.yaml
```

Every time you import a library or an external dependency in your code, you might need to run the `stack install <name-of-lib>` to install that library. However, that doesn’t stop there since you need to add them under the `dependencies` section of your `package.yaml` file. Yours might look like this:

```  
-- boilerplate ….
dependencies:
- base >= 4.7 && < 5
-- boilerplate again….
```

This goes for every library that is available on Stackage – the repository of libraries where stack pulls from. As I have mentioned earlier, not every dependency is present on Stackage. So, if you used a dependency that is not available on Stackage, then you must add that under the `extra-deps` in the `stack.yaml` file. For now, it is commented out since our project does not have dependencies yet.

Here’s mine for now (the acme-missiles library is just an example; you can delete it later):

```
# boilerplate…  
# extra-deps:
# - acme-missiles-0.3
# - git: https://github.com/commercialhaskell/stack.git
#  commit: e7b331f14bcffb8367cd58fbfc8b40ec7642100a
# boilerplate again…  
```

## Coding

Finally, we reached the coding part. As I have mentioned, this will not be an in-depth tutorial wherein I teach every nook and cranny of code. Instead, I am just showcasing how CRUD operations can be implemented in Haskell. With that said let’s proceed with the coding.

In this project, we will be using the following libraries:

```  
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
```

If you run the `stack install --file-watch` command, it will show an error message either saying that you haven’t installed the library (in which you should) or you haven’t added them to your `dependencies` section in your package.yaml.

In my case, I already installed these libraries, so the only error that occurs on my end is that I haven’t added them under the `dependencies` section of my package,yaml

Here are my error messages:

```  
app\Main.hs:6:1: error:
Could not find module `Data.List.Split'
Use -v (or `:set -v` in ghci) to see a list of the files searched for.
|
6 | import Data.List.Split (splitOn)
| ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

app\Main.hs:7:1: error:
Could not load module `Data.Time.Calendar.OrdinalDate'
It is a member of the hidden package `time-1.12.2'.
Perhaps you need to add `time' to the build-depends in your .cabal file.
Use -v (or `:set -v` in ghci) to see a list of the files searched for.
|
7 | import Data.Time.Calendar.OrdinalDate ( Day )
| ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

app\Main.hs:8:1: error:
Could not load module `Data.Time'
It is a member of the hidden package `time-1.12.2'.
Perhaps you need to add `time' to the build-depends in your .cabal file.
Use -v (or `:set -v` in ghci) to see a list of the files searched for.
|
8 | import Data.Time
| ^^^^^^^^^^^^^^^^...

app\Main.hs:12:1: error:
Could not find module `System.IO.Strict'
Perhaps you meant
System.OsString (needs flag -package-id filepath-1.4.200.1)
Use -v (or `:set -v` in ghci) to see a list of the files searched for.
|
12 | import qualified System.IO.Strict (readFile)
| ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

app\Main.hs:14:1: error:
Could not find module `System.Console.ANSI'
Use -v (or `:set -v` in ghci) to see a list of the files searched for.
|
14 | import System.Console.ANSI ( clearScreen, setCursorPosition )
| ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

app\Main.hs:16:1: error:
Could not load module `System.Directory'
It is a member of the hidden package `directory-1.3.8.1'.
Perhaps you need to add `directory' to the build-depends in your .cabal file.
Use -v (or `:set -v` in ghci) to see a list of the files searched for.
|
16 | import System.Directory (doesFileExist)
| ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  
```

Remember when I said that running `stack install --filewatch` gives us some solutions in the error messages? As you can see, it suggests that we add the `directory` dependency to the `build-depends` section in our `.cabal` file. However, before adding it to the `.cabal` file, you should first try to add it in the `dependencies` section of your `package.yaml`.  
  
Here is my `dependencies` section after adding all the dependencies:  
```  
dependencies:
- base >= 4.7 && < 5
- time
- directory
- ansi-terminal
- strict
- split  
```

### Data types
Let's start with the data types that we will be using throughout the program:
```
data  FoodRecord = FoodRecord
	{
		foodID :: Integer,
		category :: FoodCategory,
		name :: String,
		stock :: Int,
		expDate :: Day
	} deriving (Show, Eq)

data  FoodCategory = Dairy
	| Meat
	| Vegetable
	| Fruit
	deriving (Show, Eq)
```
The `FoodRecord` data type essentially holds all of the information that a food will have. In a way, it exhibits some of the concepts of object-oriented programming. This is also true for the `FoodCategory` data type.

### Non-CRUD Operation Functions
Now, let's proceed with the non-CRUD operation functions or other helper functions:
```
getCurrentDate :: IO  Day
getCurrentDate = utctDay <$> getCurrentTime

printMenuHeader :: String -> IO()
printMenuHeader header = do
	let topBotLine = putStrLn ('+' : replicate 98  '-' ++ "+")
	let spacerLine = putStrLn ('|' : replicate 98  ' ' ++ "|")
	case header of
		"Add" -> do
				topBotLine
				spacerLine
				putStrLn "|\t\t\t\t\tAdd Food Record\t\t\t\t\t\t |"
				spacerLine
				topBotLine
		"Edit" -> do
				topBotLine
				spacerLine
				putStrLn "|\t\t\t\t\tEdit Food Record\t\t\t\t\t |"
				spacerLine
				topBotLine
		"Display" -> do
				topBotLine
				spacerLine
				putStrLn "|\t\t\t\t\tDisplay Food Record\t\t\t\t\t |"
				spacerLine
				topBotLine
		"Delete" -> do
				topBotLine
				spacerLine
				putStrLn "|\t\t\t\t\tDelete Food Record\t\t\t\t\t |"
				spacerLine
				topBotLine
		"Food Info" -> do
				topBotLine
				spacerLine
				putStrLn "|\t\t\t\t\tFood Information\t\t\t\t\t |"
				spacerLine
				topBotLine
		"Main" -> do
				topBotLine
				spacerLine
				putStrLn "|\t\t\t\t\tHaskell Food Storage\t\t\t\t\t |"
				spacerLine
				topBotLine
		"Spoiled" -> do
				topBotLine
				spacerLine
				putStrLn "|\t\t\t\t\tRemove Spoiled Items\t\t\t\t\t |"
				spacerLine
				topBotLine
		"Stock" -> do
				topBotLine
				spacerLine
				putStrLn "|\t\t\t\t\tDiplay Stock Levels\t\t\t\t\t |"
				spacerLine
				topBotLine
		_ -> error "| ERROR: Invalid argument!"

  
selectMenuChoice :: String -> [Int] -> IO  Int
selectMenuChoice prompt list = do
	choice <- getInput prompt
	let maybeChoice = readMaybe choice :: Maybe  Int
	
	case maybeChoice of
		Just choiceInt -> if choiceInt `elem` list
			then return choiceInt
			else putStrLn "| ERROR: Invalid input. Please input an integer." >> selectMenuChoice prompt list
		Nothing -> putStrLn "| ERROR: Invalid input. Please input an integer." >> selectMenuChoice prompt list

  

-- Function to prompt user for input, (removes buffer)
getInput :: String -> IO  String
getInput prompt = do
	putStr prompt
	hFlush stdout
	getLine

getCategory :: IO  FoodCategory
getCategory = do
	putStrLn ('+' : replicate 98  '-' ++ "+")
	putStrLn ("| [Food Categories]" ++ replicate 80  ' ' ++ "|")
	putStrLn ("| [1] Dairy" ++ replicate 88  ' ' ++ "|")
	putStrLn ("| [2] Meat" ++ replicate 89  ' ' ++ "|")
	putStrLn ("| [3] Vegetable" ++ replicate 84  ' ' ++ "|")
	putStrLn ("| [4] Fruit" ++ replicate 88  ' ' ++ "|")
	putStrLn ('+' : replicate 98  '-' ++ "+")
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


getStock :: IO  Int
getStock = do
	putStrLn ('+' : replicate 98  '-' ++ "+")
	stock <- getInput "| Please input stock of food: "
	
	if all isDigit stock && read stock >= (0 :: Int)
		then return (read stock)
		else putStrLn "| ERROR: Invalid input. Please enter a valid non-negative number for stock." >> getStock


-- Function to prompt user for the name of food item
getName :: IO  String
getName = do
	putStrLn ('+' : replicate 98  '-' ++ "+")
	getInput "| Please input name of food: "
	
  
getID :: IO  Integer
getID = do
	csvData <- readCsvFile csvFile
	let recordIds = map foodID csvData
	let maxID = if null recordIds
		then  0
		else maximum recordIds
	return $ maxID + 1

getExpDate :: IO  Day
getExpDate = do
	putStrLn ('+' : replicate 98  '-' ++ "+")
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
	putStrLn "| [1] Add New Food Record \t\t\t\t\t\t\t\t\t |"
	putStrLn "| [2] Edit Food Record \t\t\t\t\t\t\t\t\t\t |"
	putStrLn "| [3] Delete Food Record \t\t\t\t\t\t\t\t\t |"
	putStrLn "| [4] Display All Records \t\t\t\t\t\t\t\t\t |"
	putStrLn "| [5] Remove Spoiled Items \t\t\t\t\t\t\t\t\t |"
	putStrLn "| [6] Display Stock Levels \t\t\t\t\t\t\t\t\t |"
	putStrLn "| [7] Exit Program \t\t\t\t\t\t\t\t\t\t |"
	putStrLn ('+' : replicate 98  '-' ++ "+")
	option <- selectMenuChoice "Select an option: " [1, 2, 3, 4, 5, 6, 7]
	
	case option of
		1 -> addRecord >> mainMenu
		2 -> editRecord >> mainMenu
		3 -> deleteRecord >> mainMenu
		4 -> displayRecords >> mainMenu
		5 -> removeSpoiledItems >> mainMenu
		6 -> displayStockLevels >> mainMenu
		7 -> exitSuccess
		_ -> putStrLn "Invalid option. Please select again." >> mainMenu
```

All of the functions starting with get (e.g., `getInput`, `getID `, `getCategory `, `getName`, `getStock`, and `getExpDate`) retrieves the input with the respective field.

`selectMenuChoice` on the other hand streamlines the process of receiving input from a menu.

`printMenuHeader` also streamlines the process of printing each header

Lastly,  the `mainMenu` function acts as the main menu of the program and is the first one to show up once you open the program.

### CRUD Helper Functions
Now, let's create the CRUD helper functions:

```  
csvFile :: String
csvFile = "inventory.csv"

loadFileStrict :: FilePath -> IO  String
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
		-- maps a String to a FoodCategory
		toCategoryStr :: String -> FoodCategory
		toCategoryStr str = case str of
			"Dairy" -> Dairy
			"Meat" -> Meat
			"Vegetable" -> Vegetable
			"Fruit" -> Fruit
			_ -> error "| ERROR: Invalid choice!"

formatRecord :: FoodRecord -> String
-- converts a `FoodRecord` to a `String` and adds commas to each field.
formatRecord food = concat [show (foodID food), ",", show (category food), ",", name food, ",", show (stock food), ",", show (expDate food)]

parseToRecord :: String -> FoodRecord
parseToRecord line =
	let (foodID, category, name, stock, expDate) = extractFields line
	in  FoodRecord foodID category name stock expDate
	
createCsvFile :: FilePath -> IO ()
createCsvFile file = do
	-- checks if the file exists
	fileExists <- doesFileExist file
	if not fileExists
		-- if file does not exist, create one
		then writeFile file ""
		-- else, print that it already exists
		else putStrLn "| INFO: CSV file already exists"
```

The `loadFileStrict` is a helper function that reads the file eagerly rather than lazily. This avoids any problem related to Haskell’s lazy evaluation in reading operations. For more information, click [here.](https://stackoverflow.com/questions/5053135/resource-busy-file-is-locked-error-in-haskell)

The `parseToRecord` function, as its name implies, parses a `String` data type into a `FoodRecord` data type. It extracts the foodID, category, name, stock, and expDate from the String using the `extractFields` function and returns a `FoodRecord` with the values of said fields.

The `extractFields` function, on the other hand, extracts the fields in a given `String`. First, it splits the string using the `,` as a delimiter to specify each field and creates a list. It then uses the `!!` symbol to access the elements in the list and assign them to each field similar to `[]` in imperative languages. Finally, it returns a tuple of the data types that constitute a `FoodRecord`.

The `formatRecord` function takes a `FoodRecord` data type and converts it to a `String`. It uses the `concat` function to concatenate the elements in the list thus creating a `String`.

The `csvFile` is a `String` which is the name of the CSV file you want to create. On the other hand, the `createCsvFile` function, as its name suggests, creates a CSV file if one does not exist yet.

### Create Operation
Let's now proceed with the CRUD operations. Let's start with the Create operation which is the easiest to implement from the CRUD operations:

```  
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
	setCursorPosition 0  0
	printMenuHeader "Add"
	confirm <- getInput "| Do you want to add a food record? (y/n): "
	if confirm == "y" || confirm == "Y"
		then  do
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
	else  if confirm == "n" || confirm == "N"
		then  do
			mainMenu
	else putStrLn "| Invalid choice!" >> addRecord
```

The `appendRecord` function takes a `FoodRecord` as an argument and applies the `formatRecord` function to convert it to `String` so that it can append it in the CSV file. It also appends a `\n` to each record so that they are separated into different lines.

The `addRecord` function is the menu shown when adding a food record. It calls the other functions that get the fields of a record.

### Read Operation
Let’s now proceed with the Read operation:

```  
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
```

The `readCsvFile` reads the CSV file and catches any read errors using the `handleReadError` function which just shows the exception that happened. It returns a list of `FoodRecord` since the `parseToRecord` function is applied to each element in the list generated by the `(lines csvData)`.

### Display Operation
The display operation is not a part of the CRUD acronym but it is essential for any CRUD app. It is also somewhat an extension of the read operation since you have to read the database (the CSV file in our case) to display all the records inside it.

Here is the display operation:
```
tableCell :: String
tableCell = "| %-6s | %-9s | %-49s | %-10s | %-10s |"

displayRecords :: IO ()
displayRecords = do
	clearScreen
	setCursorPosition 0  0
	printMenuHeader "Display"
	csvData <- readCsvFile csvFile
	if null csvData
		then putStrLn "| INFO: There are no records in the database."
		else  do
			let table_header = printf tableCell "FoodID"  "Category"  "Name"  "Stock"  "Exp Date"
			putStrLn table_header
			putStrLn ("+" ++ replicate 8  '-' ++ "+" ++ replicate 11  '-' ++ "+" ++ replicate 51  '-' ++ "+" ++ replicate 12  '-' ++ "+" ++ replicate 12  '-' ++ "+")
			recordRows <- mapM printRecordRow csvData
			putStr $ unlines recordRows
			putStrLn ("+" ++ replicate 8  '-' ++ "+" ++ replicate 11  '-' ++ "+" ++ replicate 51  '-' ++ "+" ++ replicate 12  '-' ++ "+" ++ replicate 12  '-' ++ "+")

printRecordRow :: FoodRecord -> IO  String
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
```
The `tableCell` is a `String` that contains the spacing and format of the table.

The `displayRecords` function displays the records if there are any, otherwise it just prints that the database has no records yet.

The `printRowRecord` function takes the fields of a `FoodRecord` as an argument and prints them. It adds an asterisk `*` when the food is already spoiled.

### Update Operation
The update operation is probably the hardest to implement in my opinion. But here is how I implemented mine:

```
searchRecordID :: Integer -> IO (Maybe  FoodRecord)
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
	-- This line writes the updated records back to the CSV file.
	-- It uses the `map` function to apply the update function to each record in the list.
	let updatedCsvData = map (\r -> if r == record then updatedRecord else r) csvData
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
	putStrLn ('+' : replicate 98  '-' ++ "+")
	putStrLn ("| [1]. Category" ++ replicate 84  ' ' ++ "|")
	putStrLn ("| [2]. Name" ++ replicate 88  ' ' ++ "|")
	putStrLn ("| [3]. Stock" ++ replicate 87  ' ' ++ "|")
	putStrLn ("| [4]. Expiration Date" ++ replicate 77  ' ' ++ "|")
	putStrLn ('+' : replicate 98  '-' ++ "+")
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
	setCursorPosition 0  0
	printMenuHeader "Edit"
	
	csvData <- readCsvFile csvFile
	if null csvData
		then putStrLn "| INFO: There are no records in the database. Please add a record first."
		else  do
			confirm <- getInput "| Do you want to edit a food record? (y/n): "
			if confirm == "y" || confirm == "Y"
				then  do
					displayRecords
					idToEdit <- getInput "| Enter the ID of the record you want to edit: "
					if all isDigit idToEdit
						then  do
							let realID = read idToEdit
							maybeRecord <- searchRecordID realID
							case maybeRecord of
								Just record -> editRecordDetails record
								Nothing -> putStrLn "| INFO: Record not found."
						else putStrLn "| ERROR: Invalid ID. Please enter a valid integer ID." >> editRecord
			else  if confirm == "n" || confirm == "N"
				then  do
					mainMenu
			else putStrLn "| ERROR: Invalid input!" >> editRecord
```
The `searchRecordID` function takes an integer as an argument which should be the ID that the user wants to search. It then searches the CSV file for the record that matches that ID and returns it. The IDs in the CSV file are always incremented by 1 (via the `getID` function), and as such they are all unique.

The `updateRecord` function takes a function, the new value, a `FoodRecord`, and a `String` (for messages) as parameters. The function that it takes as an argument replaces the old field with a new one. Then, it updates the contents of the `csvData` by applying the lambda function to each record. Technically, it just updates the matching record and makes no changes to the other records, but since we want the display function to not be affected (i.e., displaying records with the ID sorted in ascending order), we update the whole contents of the `csvData` and write them again into the file.

All the functions that update the fields (e.g., `updateCategory`) just replace the old field with a new one.

The `editRecordDetails` function allows the user to choose what field to update and applies the `updateRecord` function that we discussed earlier.

`1 -> getCategory >>= \newCategory -> updateRecord updateCategory newCategory record "Category"`

This line might be unreadable or even intimidating, but let me explain it. The output that is returned by the `getCategory` function is passed to a lambda function (`\` denotes that a function is a lambda function) called `\newCategory`. The only use of this function is to hold the value given by the `getCategory`.

In simpler terms, this whole expression is saying: "Get a new category using the `getCategory` function, then use this new category as an argument in the `updateRecord` function to update the category of the record". I hope that helped ;).

Lastly, the `editRecord` function acts as the interface for the user. It applies all the edit-related functions.

### Delete Operation
The delete operation is similar to the the update operation, but instead of editing the record you delete it.

Here is how I implemented it:
```
deleteRecordFromFile :: Integer -> IO ()
deleteRecordFromFile idToDelete = do
	csvData <- readCsvFile csvFile
	let updatedRecords = filter (\r -> foodID r /= idToDelete) csvData
	writeRecordsToFile csvFile updatedRecords
	putStrLn "| INFO: Record deleted successfully."

deleteRecord :: IO ()
deleteRecord = do
	clearScreen
	setCursorPosition 0  0
	displayRecords
	printMenuHeader "Delete"
	
	csvData <- readCsvFile csvFile
	
	if null csvData
		then putStrLn "| INFO: There are no records in the database. Please add a record first."
		else  do
			-- Allows the user to go back
			confirm <- getInput "| Do you want to delete a food record? (y/n): "

			if confirm == "y" || confirm == "Y"
			then  do
			displayRecords
			idToDelete <- getInput "| Enter the ID of the record you want to delete:"

			-- checks if idToDelete is a number
			if all isDigit idToDelete
				then  do
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
			else  if confirm == "n" || confirm == "N"
				then  do
					mainMenu
			else putStrLn "| ERROR: Invalid choice!" >> deleteRecord
```
The `deleteRecordFromFile` function is the inverse of the `updateFunction` earlier. It updates all the contents of the `csvData` but instead of updating the matching record, it doesn't include the record with the matching ID in the updated `csvData` and as such, it is not written into the file, thus deleting it.

The `deleteRecord` function acts as the interface that the user interacts with. 

### Miscellaneous Operation
These are functions that are not a part of the original CRUD operations, but similar to the Display operation, they extend the CRUD operations. Furthermore, they were required in the making of the final project requirement.

```
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
	setCursorPosition 0  0
	printMenuHeader "Spoiled"
	
	csvData <- readCsvFile csvFile
	
	if null csvData
		then putStrLn "| INFO: There are no records in the database. Please add a record first."
		else  do
			removeItem <- getInput "| Do you want to spoiled food records? (y/n): "

			if removeItem == "Y" || removeItem == "y"
				then  do
					csvData <- readCsvFile csvFile
					-- filter fresh (expDate >= current date) in the record
					freshItems <- filterFresh csvData

					-- overwrite the file with only the fresh items thus removing the `spoilt` items
					writeRecordsToFile csvFile freshItems
					displayRecords
			else  if removeItem == "N" || removeItem == "n"
				then  do
					mainMenu
			else putStrLn "| ERROR: Invalid choice!" >> deleteRecord


stockCell :: String
stockCell = "| %-30s | %-22s | %-20s | %-15s |\n"

displayStockLevels :: IO ()
displayStockLevels = do
	clearScreen
	setCursorPosition 0  0
	printMenuHeader "Stock"

	csvData <- readCsvFile csvFile
	if null csvData
		then putStrLn "| INFO: There are no records in the database. Please add a record first."
		else  do
			currentDay <- getCurrentDate
			let requiredStockLevel = 100

			putStrLn ("+" ++ replicate 32  '-' ++ "+" ++ replicate 24  '-' ++ "+" ++ replicate 22  '-' ++ "+" ++ replicate 17  '-' ++ "+")
			printf stockCell "Name"  "Current Stock Level"  "Required Stock Level"  "Restock Number"
			putStrLn ("+" ++ replicate 32  '-' ++ "+" ++ replicate 24  '-' ++ "+" ++ replicate 22  '-' ++ "+" ++ replicate 17  '-' ++ "+")

			 
			forM_ csvData $ \record -> do
				let currentStock = stock record
				restockNumber = requiredStockLevel - currentStock
				nameWithAsterisk
					| expDate record < currentDay = name record ++ "*"
					| otherwise = name record

				if restockNumber >= 0
					then  do
						printf stockCell nameWithAsterisk (show currentStock) (show requiredStockLevel) (show restockNumber)
					else
						printf stockCell nameWithAsterisk (show currentStock) (show requiredStockLevel) "N/A"
				putStrLn ("+" ++ replicate 32  '-' ++ "+" ++ replicate 24  '-' ++ "+" ++ replicate 22  '-' ++ "+" ++ replicate 17  '-' ++ "+")

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
```
The `filterFresh` function takes a list of `FoodRecord` as an argument and checks their expiration date against the current date. Then, it filters the dates in the list that are greater than or equal to the current date.

It is used in tandem with the `removeSpoiledItems` function which is an extension of the delete operation. But instead of deleting a record, it deletes all spoiled items. It does so by filtering only the fresh items using the `filterFresh` function and overwrites the CSV file with only the fresh items.

The `displayStockLevels` function, on the other hand, just displays the current stock of each food record and calculates the amount of stock it needs to reach the required stock level (restock number). The required stock level is just an arbitrary number we set to 100. If the restock number is negative, then it prints out `N/A` instead of showing a negative number.

## Conclusion

I hope this tutorial/guide/reference helped you accomplish your Haskell code. While cramming the final project requirement, I was trying to find a reference like what I did since I find declarative code and Haskell's documentation hard to read.

It got to the point where I asked our upperclassmen if they had done something similar, but it seemed that they too struggled. In the end, I found no references or help, and I had to dig through Haskell's documentation and countless StackOverflow forums. That was truly a nightmare and I wouldn't wish it on anyone. As such, I promised myself that after I finished the final project requirement, I would create a tutorial or guide that I wish I had during those times.

If this short and crude tutorial somewhat helped you in any way, leaving a star on the GitHub repository would mean a lot.

That's all, thank you!

Love, Crumbz
