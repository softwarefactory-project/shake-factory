-- | Shake functions for dhall project
module ShakeFactory.Dhall
  ( -- * Dhall commands
    dhallFormat,
    dhallFreeze,
    dhallFormatFreeze,

    -- * Actions
    dhallTopLevelPackageAction,
    dhallPackageAction,
    dhallReadmeAction,
    dhallDefaultAction,

    -- * Helpers
    dhallDocsRules,
    mkDhallPackage,

    -- * Re-Export
    needDhall,
  )
where

import Data.Bifunctor (second)
import Data.List (isPrefixOf, isSuffixOf, sortOn)
import Data.Text (Text, pack, unpack)
import Development.Shake
import Development.Shake.Dhall (needDhall)
import Development.Shake.FilePath
import Dhall.Core
import Dhall.Map (fromList, toList)
import Dhall.Parser (exprFromText)
import Dhall.Pretty (prettyExpr)

-- | Convert a dhall type record to a value record using None value for Optional attributes
-- | >>> getDefaults "./Job/Type.dhall" "{ name : Text, become : Optional Bool, task : Optional ./Task.dhall }"
-- | "{ become = None Bool, task = None ./Task.dhall }"
getDefaults :: FilePath -> Text -> Text
getDefaults fp content = decode
  where
    decode :: Text
    decode = case exprFromText fp content of
      Left err -> error $ show err
      Right expr -> case Dhall.Core.denote expr of
        Record record -> pack $ (show $ prettyExpr $ process record) <> "\n"
        _ -> error $ fp <> ": is not a record type"
    process record =
      RecordLit
        $ fromList
        $ sortOn fst
        $ map (second makeRecordField)
        $ go
        $ map (second recordFieldValue)
        $ toList record
    -- Process every record element
    go :: [(Text, Expr s Import)] -> [(Text, Expr s Import)]
    go [] = []
    go ((n, App Optional v) : xs) = (n, App None v) : go xs
    go (_ : xs) = go xs

isRecord :: FilePath -> Text -> Bool
isRecord fn content = case exprFromText fn content of
  Left _ -> False
  Right expr -> case Dhall.Core.denote expr of
    Record _ -> True
    _ -> False

dhallDefaultAction :: FilePath -> Action ()
dhallDefaultAction fp =
  do
    fileContent <- readFile' src
    if isRecord fp (pack fileContent)
      then do
        putVerbose $ fp <> ": created from " <> src
        writeFile' fp $ unpack (getDefaults src $ pack fileContent)
      else do
        putVerbose $ fp <> ": can't be generated from Type.dhall"
        pure ()
  where
    src = ren "Type.dhall" fp
    ren fn2 fn = joinPath (dropLast (splitPath fn) <> [fn2])
    dropLast = reverse . drop 1 . reverse

-- | A shake action to run dhall
dhallCommand ::
  -- | The cwd
  FilePath ->
  -- | The dhall args
  [String] ->
  -- | The command input
  String ->
  Action String
dhallCommand cwd args input = do
  Stdout out <- command [Cwd cwd, Stdin input] "dhall" args
  pure out

-- | A shake action to run dhall format
dhallFormat ::
  -- | The dhall expression
  String ->
  Action String
dhallFormat = dhallCommand "." ["format", "--ascii"]

-- | A shake action to run dhall freeze
dhallFreeze ::
  -- | The cwd
  String ->
  -- | The dhall expression
  String ->
  Action String
dhallFreeze cwd = dhallCommand cwd ["freeze", "--all"]

-- | A shake action to format and freeze an expression
dhallFormatFreeze ::
  -- | The cwd
  FilePath ->
  -- | The dhall expression
  String ->
  Action String
dhallFormatFreeze cwd expr = dhallFormat expr >>= dhallFreeze cwd

-- | A shake action to create the top-level package.dhall
dhallTopLevelPackageAction ::
  -- | Path of the real package
  FilePath ->
  -- | Name of the top level file
  FilePath ->
  Action ()
dhallTopLevelPackageAction src dst = do
  need [src]
  dhallFormat src >>= writeFile' dst

-- | Render the documentation of a dhall project
dhallDocsRules ::
  -- | The name of the dhall package
  String ->
  Rules ()
dhallDocsRules name =
  do
    phony "docs" dhallDocsNeed
    phony "docs-publish" dhallDocsPublish
    "build/docs/index.html" %> const dhallDocsAction
  where
    dhallDocsNeed = need ["build/docs/index.html"]
    dhallDocsAction = do
      cmd_ "mkdir -p build"
      cmd_ "dhall-docs --input . --package-name" name "--output-link build/result --ascii"
      cmd_ "rsync --delete -r build/result/ build/docs/"
      cmd_ "find build/docs/ -type d -exec chmod 0755 {} +"
      cmd_ "find build/docs/ -type f -exec chmod 0644 {} +"
    dhallDocsDest = "pagesuser@www.softwarefactory-project.io:/var/www/pages/docs.softwarefactory-project.io/" <> name <> "/"
    dhallDocsPublish = do
      dhallDocsNeed
      cmd_ "rsync --delete -avi ./build/docs/" dhallDocsDest

-- | Create a dhall package from a list of files:
--
--   >>> mkDhallPackage ["Type.dhall", "Project/package.dhall"]
--   "{ Type = ./Type.dhall, Project = ./Project/package.dhall }"
mkDhallPackage :: [FilePath] -> String
mkDhallPackage = mkRecord . map mkAttribute
  where
    mkRecord :: [String] -> String
    mkRecord attrs = "{ " <> drop 2 (concat attrs) <> " }"
    mkAttribute :: FilePath -> String
    mkAttribute f = ", " <> getName f <> " = ./" <> f
    getName :: FilePath -> String
    getName fp =
      let fc = getFirstComponent fp
       in if hasExtension fc
            then dropExtensions fc
            else takeDirectory fc
    getFirstComponent :: FilePath -> FilePath
    getFirstComponent = head . splitPath

-- | Create a dhall package at file at location from ["*.dhall", */package.dhall]
--   For example, if the location directory contains a Type and default file, the package will contains:
--   { Type = ./Type.dhall, default = ./default.dhall }
dhallPackageAction :: FilePath -> Action ()
dhallPackageAction target =
  do
    -- Get the directory files excluding "package.dhall" and "Prelude.dhall"
    files <- filter (`notElem` ["package.dhall", "Prelude.dhall"]) <$> getDirectoryFiles targetDir ["*.dhall", "*/package.dhall"]
    putVerbose $ "Creating package with: " <> show files
    -- Generate the package.dhall
    let packageText = mkDhallPackage files
    putVerbose $ target <> ": need " <> show files <> " (from " <> targetDir <> ") -> \n" <> packageText
    -- Indicate that this package needs all of it's file import
    need (map (targetDir </>) files)
    -- Format and freeze the package
    dhallFormat packageText >>= writeFile' target
  where
    targetDir = takeDirectory target

-- | Ensure the readme contains the latest content and result
--   This function replace every code-block with the file content
--   and the command output.
dhallReadmeAction :: FilePath -> Action ()
dhallReadmeAction fp =
  do
    fpLines <- liftIO $ lines <$> readFile fp
    let fpIncludes = foldr getInclude [] fpLines
    putVerbose $ fp <> ": need " <> show fpIncludes
    needDhall fpIncludes
    newDoc <- sequence (updateDoc fpLines [])
    writeFileChanged fp (unlines newDoc)
  where
    -- Return the list of included file
    getInclude :: String -> [FilePath] -> [FilePath]
    getInclude fpLine acc = case isInclude fpLine of
      Just include -> include : acc
      Nothing -> acc
    -- The main function that replaces code-block with their content
    updateDoc :: [String] -> [Action String] -> [Action String]
    updateDoc [] acc = reverse acc
    updateDoc allLines@(x : xs) acc = case isInclude x of
      Just include -> updateDoc (dropUntilBlockEnd xs) (readFile' include : pure x : acc)
      Nothing -> case isCommand allLines of
        Just (header, commandString) -> updateDoc (dropUntilBlockEnd xs) (runCommand commandString : pure header : acc)
        Nothing -> updateDoc xs (pure x : acc)
    -- Return the include file if it begins with `-- ./`
    isInclude :: String -> Maybe FilePath
    isInclude line =
      if isPrefixOf "-- ./" line && isSuffixOf ".dhall" line
        then Just (drop 3 line)
        else Nothing
    -- Returns the header and command if the list begins with a command code-block
    isCommand :: [String] -> Maybe (String, String)
    isCommand (header : line : _) =
      if isPrefixOf "```" header && isPrefixOf "# dhall" line
        then Just (header <> "\n" <> line, drop 2 line)
        else Nothing
    isCommand _ = Nothing
    -- Execute a command and return its output
    runCommand :: String -> Action String
    runCommand commandString = do
      putInfo $ "Running: " <> commandString
      Stdout out <- command [] "bash" ["-c", commandString]
      -- `pure` puts the String back into Action to be sequenced later
      pure out
    -- A helper to drop generated code block
    dropUntilBlockEnd :: [String] -> [String]
    dropUntilBlockEnd [] = error "Unexpected end of file"
    dropUntilBlockEnd res@("```" : _) = res
    dropUntilBlockEnd (_ : xs) = dropUntilBlockEnd xs
