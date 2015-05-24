{-# LANGUAGE OverloadedStrings #-}

import Common                           
import Data.Text                        (splitOn)
import Data.Functor                     ((<$>))
import Data.Maybe                       (fromJust, isJust)
import Database.SQLite.Entities         
import Database.SQLite.Simple           
import Database.SQLite.Simple.FromField (FromField)
import Test.HUnit                       (Test(..), Assertion, (@=?), runTestTT)
import qualified Data.Text.IO as Text   (readFile)

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

------------------------------------------------------------------------ Images

-- | Tests the deleteImage function.
deleteImageTest :: Test
deleteImageTest = testDatabase $ \conn -> do
    id1 <- insertImage conn (Image "title1" False "hash1" "ext1" 2 4 "" "" 8)
    id2 <- insertImage conn (Image "title2" True  "hash2" "ext2" 1 3 "" "" 5)
    id3 <- insertImage conn (Image "title3" True  "hash3" "ext3" 9 7 "" "" 6)
    
    numResults1 <- length <$> selectImages conn
    deleteImage conn id2
    numResults2 <- length <$> selectImages conn
    deleteImage conn 0
    numResults3 <- length <$> selectImages conn
    
    image1 <- selectImage conn id1
    image2 <- selectImage conn id2
    image3 <- selectImage conn id3
    
    3     @=? numResults1
    2     @=? numResults2
    2     @=? numResults3
    True  @=? isJust image1
    False @=? isJust image2
    True  @=? isJust image3

-- | Tests the insertImage function.
insertImageTest :: Test
insertImageTest = testDatabase $ \conn -> do
    let image1 = Image "title1" False "hash1" "ext1" 2 4 "" "" 8
    let image2 = Image "title2" True  "hash2" "ext2" 1 3 "" "" 5

    id1          <- insertImage conn image1
    id2          <- insertImage conn image2
    [numResults] <- query_ conn command1 :: IO [Int]

    2   @=? numResults
    id1 @/? id2
    
    assertImage conn head image1 id1
    assertImage conn last image2 id2
    
    where
        assertImage conn pos image id = do
            (id', title', isFavourite', hash', extension', width', height', 
             created', modified', fileSize') <- pos <$> query_ conn command2
            
            id'          @=? id
            title'       @=? imageTitle       image
            isFavourite' @=? imageIsFavourite image
            hash'        @=? imageHash        image
            extension'   @=? imageExtension   image
            width'       @=? imageWidth       image
            height'      @=? imageHeight      image
            created'     @/? imageCreated     image
            modified'    @/? imageModified    image
            fileSize'    @=? imageFileSize    image
                    
        command1 = "SELECT COUNT(*) FROM image"
    
        command2 = "SELECT i.id, p.title, p.is_favourite, i.hash, \
                   \i.extension, i.width, i.height, p.created, p.modified, \
                   \i.file_size \
                   \FROM post p \
                   \JOIN image i ON i.post_id = p.id;"

-- | Tests the selectImage function.
selectImageTest :: Test
selectImageTest = testDatabase $ \conn -> do
    noResults <- selectImage conn 1
    
    Nothing @=? noResults

    let image1 = Image "title1" False "hash1" "ext1" 2 4 "" "" 8
        image2 = Image "title2" True  "hash2" "ext2" 1 3 "" "" 5
        image3 = Image "title3" True  "hash3" "ext3" 9 7 "" "" 6
    
    id1 <- insertImage conn image1
    id2 <- insertImage conn image2
    id3 <- insertImage conn image3
    
    (Entity id4 image4) <- fromJust <$> selectImage conn id2
    
    -- Ignore the date fields when comparing.
    let image2' = image2 { imageCreated = "", imageModified = "" }
        image4' = image4 { imageCreated = "", imageModified = "" }
    
    id2     @=? id4
    image2' @=? image4'

-- | Tests the selectImages function.
selectImagesTest :: Test
selectImagesTest = testDatabase $ \conn -> do
    noResults <- selectImages conn
    
    [] @=? noResults

    id1 <- insertImage conn (Image "title1" False "hash1" "ext1" 2 4 "" "" 8)
    id2 <- insertImage conn (Image "title2" True  "hash2" "ext2" 1 3 "" "" 5)
    id3 <- insertImage conn (Image "title3" True  "hash3" "ext3" 9 7 "" "" 6)
    
    image1 <- fromJust <$> selectImage conn id1
    image2 <- fromJust <$> selectImage conn id2
    image3 <- fromJust <$> selectImage conn id3
    images <- selectImages conn
    
    [image1, image2, image3] @=? images

-- | Tests the updateImage function.
updateImageTest :: Test
updateImageTest = testDatabase $ \conn -> do
    let image1 = Image "title1" False "hash1" "ext1" 2 4 ""  ""  8
        image2 = Image "title2" True  "hash2" "ext2" 3 5 "a" "b" 9
        
    id <- insertImage conn image1
    
    updateImage conn (Entity id image2)
    
    image3 <- fromEntity <$> fromJust <$> selectImage conn id
    
    -- Only the title, isFavourite, and modified fields should change.
    imageTitle       image2 @=? imageTitle       image3
    imageIsFavourite image2 @=? imageIsFavourite image3
    imageModified    image2 @=? imageModified    image3
    imageHash        image2 @/? imageHash        image3
    imageExtension   image2 @/? imageExtension   image3
    imageWidth       image2 @/? imageWidth       image3
    imageHeight      image2 @/? imageHeight      image3
    imageCreated     image2 @/? imageCreated     image3
    imageFileSize    image2 @/? imageFileSize    image3

-------------------------------------------------------------------------- Tags

-- Tests the selectTags function.
selectTagsTest :: Test
selectTagsTest = testDatabase $ \conn -> do
    id1 <- insertImage conn (Image "title1" False "hash1" "ext1" 2 4 ""  ""  8)
    id2 <- insertImage conn (Image "title2" True  "hash2" "ext2" 2 4 ""  ""  8)
    
    attachTags conn id1 ["test", "hello", "goodbye"]
    attachTags conn id2 ["another", "couple", "test"]
    
    results@[(Entity _ (Tag name1)),
             (Entity _ (Tag name2)),
             (Entity _ (Tag name3)),
             (Entity _ (Tag name4)),
             (Entity _ (Tag name5))] <- selectTags conn
    
    5         @=? length results
    "another" @=? name1
    "couple"  @=? name2
    "goodbye" @=? name3
    "hello"   @=? name4
    "test"    @=? name5
    
-- Tests the selectTagsByImage function.
selectTagsByImageTest :: Test
selectTagsByImageTest = testDatabase $ \conn -> do
    id1 <- insertImage conn (Image "title1" False "hash1" "ext1" 2 4 ""  ""  8)
    id2 <- insertImage conn (Image "title2" True  "hash2" "ext2" 2 4 ""  ""  8)
    
    attachTags conn id1 ["test", "hello", "goodbye"]
    attachTags conn id2 ["another", "couple", "test"]
    
    results@[(Entity _ (Tag name1)),
             (Entity _ (Tag name2)),
             (Entity _ (Tag name3))] <- selectTagsByImage conn id1
    
    3         @=? length results
    "goodbye" @=? name1
    "hello"   @=? name2
    "test"    @=? name3

-- Tests the cleanTags function.
cleanTagsTest :: Test
cleanTagsTest = testDatabase $ \conn -> do
    id1 <- insertImage conn (Image "title1" False "hash1" "ext1" 2 4 ""  ""  8)
    id2 <- insertImage conn (Image "title2" True  "hash2" "ext2" 2 4 ""  ""  8)
    
    attachTags conn id1 ["test", "hello", "goodbye"]
    attachTags conn id2 ["another", "couple", "test"]
    
    deleteImage conn id1

    results1 <- selectTags conn
    
    cleanTags conn
    
    results2@[(Entity _ (Tag name1)),
              (Entity _ (Tag name2)),
              (Entity _ (Tag name3))] <- selectTags conn
    
    5         @=? length results1
    3         @=? length results2
    "another" @=? name1
    "couple"  @=? name2
    "test"    @=? name3

----------------------------------------------------------------- Relationships
    
-- Tests the attachTag function.
attachTagTest :: Test
attachTagTest = testDatabase $ \conn -> do
    id1 <- insertImage conn (Image "title1" False "hash1" "ext1" 2 4 ""  ""  8)
    id2 <- insertImage conn (Image "title2" True  "hash2" "ext2" 2 4 ""  ""  8)
    
    attachTag conn id1 "test"
    attachTag conn 999 "test" -- nothing should happen
    
    [(Tag name)] <- fromEntity <$$> selectTagsByImage conn id1
    noResults    <- selectTagsByImage conn id2
    
    "test" @=? name
    True   @=? null noResults

-- Tests the attachTags function.
attachTagsTest :: Test
attachTagsTest = testDatabase $ \conn -> do
    id1 <- insertImage conn (Image "title1" False "hash1" "ext1" 2 4 ""  ""  8)
    id2 <- insertImage conn (Image "title2" True  "hash2" "ext2" 2 4 ""  ""  8)
    
    attachTags conn id1 ["test1", "test2", "test3"]
    
    [(Tag name1),
     (Tag name2),
     (Tag name3)] <- fromEntity <$$> selectTagsByImage conn id1
    noResults     <- selectTagsByImage conn id2
    
    "test1" @=? name1
    "test2" @=? name2
    "test3" @=? name3
    True    @=? null noResults

-- Tests the clearTags function.
clearTagsTest :: Test
clearTagsTest = testDatabase $ \conn -> do
    id1 <- insertImage conn (Image "title1" False "hash1" "ext1" 2 4 ""  ""  8)
    id2 <- insertImage conn (Image "title2" True  "hash2" "ext2" 2 4 ""  ""  8)
    
    attachTags conn id1 ["test1", "test2", "test3"]
    attachTags conn id2 ["test1", "test2", "test3"]
    
    clearTags conn id2
    clearTags conn 999  -- nothing should happen
    
    results1 <- selectTagsByImage conn id1
    results2 <- selectTagsByImage conn id2 
    
    3 @=? length results1
    0 @=? length results2

----------------------------------------------------------------------- Utility

-- | Asserts that the specified actual value is not equal to the expected value 
-- | (with the expected value on the left-hand side). 
(@/?) :: (Eq a) => a -> a -> Assertion
(@/?) a b = False @=? a == b

-- | Runs a test with an empty database.
testDatabase :: (Connection -> IO ()) -> Test
testDatabase f = TestCase $ withConnection ":memory:" $ \conn -> do
    commands <- getDatabaseMigration
    mapM (execute_ conn . Query) commands
    execute_ conn "PRAGMA foreign_keys = ON"
    f conn
    where
        getDatabaseMigration = filter (/="\n") 
                               <$> splitOn ";" 
                               <$> Text.readFile "../schema.sql"
