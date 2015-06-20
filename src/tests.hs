{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as Text

import Common                   ( Entity(..), Image(..), Tag(..), Transaction
                                , (<$$>), fromEntity )
import Control.Monad            ( when )
import Control.Monad.Reader     ( runReaderT, ask )
import Control.Monad.Trans      ( lift )
import Data.Functor             ( (<$>) )
import Data.Maybe               ( isJust )
import Data.Text                ( splitOn )
import Data.Time.Clock          ( UTCTime(..) )
import Data.Time.Extended       ( fromSeconds )
import Database.SQLite.Simple   ( Query(..), execute_, query_, withConnection )
import DataSource.SQLite
import Test.HUnit               ( Test(..), Assertion, (@=?), runTestTT
                                , assertFailure )

main = runTestTT $ TestList
    [ insertImageTest
    , selectImageTest
    , selectImagesTest
    , deleteImageTest
    , updateImageTest
    , attachTagTest
    , attachTagsTest
    , selectTagsTest
    , selectTagsByImageTest
    , cleanTagsTest
    , clearTagsTest ]

------------------------------------------------------------------- Image Tests

-- | Tests the deleteImage function.
deleteImageTest :: Test
deleteImageTest = testDatabase $ do
    id1 <- insertImage (Image "t1" False "h1" "e1" 2 4 (time 1) (time 4) 8 [])
    id2 <- insertImage (Image "t2" True  "h2" "e2" 1 3 (time 2) (time 5) 5 [])
    id3 <- insertImage (Image "t3" False "h3" "e3" 9 7 (time 3) (time 6) 6 [])
    
    numResults1 <- selectImageCount
    
    deleteImage id2
    numResults2 <- selectImageCount
    
    deleteImage 999
    numResults3 <- selectImageCount
    
    image1 <- selectImage id1
    image2 <- selectImage id2
    image3 <- selectImage id3
    
    lift $ do
        numResults1   @=? 3
        numResults2   @=? 2
        numResults3   @=? 2
        isJust image1 @=? True
        isJust image2 @=? False
        isJust image3 @=? True

-- | Tests the insertImage function.
insertImageTest :: Test
insertImageTest = testDatabase $ do
    let image1 = Image "t1" False "h1" "e1" 2 4 (time 1) (time 2) 8 []
    let image2 = Image "t2" True  "h2" "e2" 1 3 (time 3) (time 4) 5 []
    
    id1 <- insertImage image1
    id2 <- insertImage image2
    
    [entity1, entity2] <- selectImages
    numResults         <- selectImageCount
    
    lift $ do
        id1               @/? id2
        numResults        @=? 2
        Entity id1 image1 @=? entity1
        Entity id2 image2 @=? entity2

-- | Tests the selectImage function.
selectImageTest :: Test
selectImageTest = testDatabase $ do
    let image1 = Image "t1" False "h1" "e1" 2 4 (time 1) (time 2) 8 []
        image2 = Image "t2" True  "h2" "e2" 1 3 (time 3) (time 4) 5 []
        image3 = Image "t3" False "h3" "e3" 9 7 (time 5) (time 6) 6 []
    
    id1 <- insertImage image1
    id2 <- insertImage image2
    id3 <- insertImage image3
    
    (Just (Entity id4 image4)) <- selectImage id1
    (Just (Entity id5 image5)) <- selectImage id2
    (Just (Entity id6 image6)) <- selectImage id3
    noResults                  <- selectImage 999
    
    lift $ do
        noResults @=? Nothing
        id1       @=? id4
        id2       @=? id5
        id3       @=? id6
        image1    @=? image4
        image2    @=? image5
        image3    @=? image6

-- | Tests the selectImages function.
selectImagesTest :: Test
selectImagesTest = testDatabase $ do
    noResults <- selectImages

    id1 <- insertImage (Image "t1" False "h1" "e1" 2 4 (time 1) (time 2) 8 [])
    id2 <- insertImage (Image "t2" True  "h2" "e2" 1 3 (time 3) (time 4) 5 [])
    id3 <- insertImage (Image "t3" False "h3" "e3" 9 7 (time 5) (time 6) 6 [])
    
    (Just image1) <- selectImage id1
    (Just image2) <- selectImage id2
    (Just image3) <- selectImage id3
    allImages     <- selectImages
    
    lift $ do
        []                       @=? noResults
        [image1, image2, image3] @=? allImages

-- | Tests the updateImage function.
updateImageTest :: Test
updateImageTest = testDatabase $ do
    let image1 = Image "t1" False "h1" "e1" 2 4 (time 1) (time 2) 8 []
        image2 = Image "t2" True  "h2" "e2" 3 5 (time 3) (time 4) 9 []
        image3 = Image "t3" True  "h3" "e3" 1 2 (time 5) (time 6) 4 []
        
    id <- insertImage image1
    
    updateImage (Entity id image2)
    
    (Just (Entity _ image4)) <- selectImage id
    
    -- Only the title, isFavourite, and modified fields should change.
    lift $ do
        imageTitle       image2 @=? imageTitle       image4
        imageIsFavourite image2 @=? imageIsFavourite image4
        imageModified    image2 @=? imageModified    image4
        imageHash        image2 @/? imageHash        image4
        imageExtension   image2 @/? imageExtension   image4
        imageWidth       image2 @/? imageWidth       image4
        imageHeight      image2 @/? imageHeight      image4
        imageCreated     image2 @/? imageCreated     image4
        imageFileSize    image2 @/? imageFileSize    image4
        image2                  @/? image3

--------------------------------------------------------------------- Tag Tests

-- Tests the selectTags function.
selectTagsTest :: Test
selectTagsTest = testDatabase $ do
    id1 <- insertImage (Image "t1" False "h1" "e1" 1 2 (time 1) (time 2) 5 [])
    id2 <- insertImage (Image "t2" True  "h2" "e2" 3 4 (time 3) (time 4) 6 [])
    
    attachTags id1 ["test", "hello", "goodbye"]
    attachTags id2 ["another", "couple", "test"]
    
    results @ [(Entity _ (Tag name1)),
               (Entity _ (Tag name2)),
               (Entity _ (Tag name3)),
               (Entity _ (Tag name4)),
               (Entity _ (Tag name5))] <- selectTags
    
    lift $ do
        length results @=? 5
        name1          @=? "another"
        name2          @=? "couple"
        name3          @=? "goodbye"
        name4          @=? "hello"
        name5          @=? "test"
    
-- Tests the selectTagsByImage function.
selectTagsByImageTest :: Test
selectTagsByImageTest = testDatabase $ do
    id1 <- insertImage (Image "t1" False "h1" "e1" 1 2 (time 1) (time 2) 5 [])
    id2 <- insertImage (Image "t2" True  "h2" "e2" 3 4 (time 3) (time 4) 6 [])
    
    attachTags id1 ["test", "hello", "goodbye"]
    attachTags id2 ["another", "couple", "test"]
    
    results @ [(Entity _ (Tag name1)),
               (Entity _ (Tag name2)),
               (Entity _ (Tag name3))] <- selectTagsByImage id1
    
    lift $ do
        length results @=? 3
        name1          @=? "goodbye"
        name2          @=? "hello"
        name3          @=? "test"

-- Tests the cleanTags function.
cleanTagsTest :: Test
cleanTagsTest = testDatabase $ do
    id1 <- insertImage (Image "t1" False "h1" "e1" 1 2 (time 1) (time 2) 5 [])
    id2 <- insertImage (Image "t2" True  "h2" "e2" 3 4 (time 3) (time 4) 6 [])
    
    attachTags id1 ["test", "hello", "goodbye"]
    attachTags id2 ["another", "couple", "test"]
    
    deleteImage id1

    results1 <- selectTags
    
    cleanTags
    
    results2 @ [(Entity _ (Tag name1)),
                (Entity _ (Tag name2)),
                (Entity _ (Tag name3))] <- selectTags
    
    lift $ do
        length results1 @=? 5
        length results2 @=? 3
        name1           @=? "another"
        name2           @=? "couple"
        name3           @=? "test"

------------------------------------------------------------ Relationship Tests

-- Tests the attachTag function.
attachTagTest :: Test
attachTagTest = testDatabase $ do
    id1 <- insertImage (Image "t1" False "h1" "e1" 1 2 (time 1) (time 2) 5 [])
    id2 <- insertImage (Image "t2" True  "h2" "e2" 3 4 (time 3) (time 4) 6 [])
    
    attachTag id1 "test1"
    attachTag 999 "test2" -- nothing should happen
    
    [(Entity _ (Tag name))] <- selectTagsByImage id1
    noResults1              <- selectTagsByImage id2
    noResults2              <- selectTagsByImage 999
    
    lift $ do
        name            @=? "test1"
        null noResults1 @=? True
        null noResults2 @=? True

-- Tests the attachTags function.
attachTagsTest :: Test
attachTagsTest = testDatabase $ do
    id1 <- insertImage (Image "t1" False "h1" "e1" 1 2 (time 1) (time 2) 5 [])
    id2 <- insertImage (Image "t2" True  "h2" "e2" 3 4 (time 3) (time 4) 6 [])
    
    attachTags id1 ["test1", "test2", "test3"]
    
    [(Entity _ (Tag name1)),
     (Entity _ (Tag name2)),
     (Entity _ (Tag name3))] <- selectTagsByImage id1
    noResults                <- selectTagsByImage id2
    
    lift $ do
        name1          @=? "test1"
        name2          @=? "test2"
        name3          @=? "test3"
        null noResults @=? True

-- | Tests the clearTags function.
clearTagsTest :: Test
clearTagsTest = testDatabase $ do
    id1 <- insertImage $ Image "" True "" "" 1 1 (time 1) (time 1) 1 []
    id2 <- insertImage $ Image "" True "" "" 1 1 (time 1) (time 1) 1 []
    
    attachTags id1 ["test1", "test2", "test3"]
    attachTags id2 ["test1", "test2", "test3"]
    
    clearTags id2
    clearTags 999 -- nothing should happen
    
    results1 <- selectTagsByImage id1
    results2 <- selectTagsByImage id2
    
    lift $ do 
        length results1 @=? 3
        length results2 @=? 0

----------------------------------------------------------------------- Utility

-- | Asserts that the specified actual value is not equal to the expected value 
-- | (with the expected value on the left-hand side).
(@/?) :: (Show a, Eq a) => a -> a -> Assertion
(@/?) a b = when (a == b) (assertFailure (message ++ show a))
    where message = "Expected two different values but got: "

-- | Selects the number of rows in the image table.
selectImageCount :: Transaction Int
selectImageCount = do
    conn <- ask
    lift $ head <$> query_ conn "SELECT COUNT(*) FROM image"

-- | Runs a test with an empty database.
testDatabase :: Transaction () -> Test
testDatabase f = TestCase $ withConnection ":memory:" $ \conn -> do
    commands <- getDatabaseMigration
    mapM_ (execute_ conn . Query) commands
    execute_ conn "PRAGMA foreign_keys = ON;"
    runReaderT f conn
    where getDatabaseMigration = filter (/="\n") 
                                 <$> splitOn ";" 
                                 <$> Text.readFile "../schema.sql"

-- | Returns the UTC time that is the given number of seconds after the epoch.
time :: Integer -> UTCTime
time = fromSeconds
