{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

import App.Core.Types         ( Album(..), Image(..), Page(..), Scope(..), Tag(..) )
import App.Database
import App.Expression         ( parse )
import Control.Monad          ( when, replicateM )
import Control.Monad.Reader   ( runReaderT, ask, lift )
import Data.Functor           ( (<$>) )
import Data.List              ( nub )
import Data.Maybe             ( fromJust, isJust )
import Data.DateTime          ( DateTime, fromSeconds )
import Database.Engine        ( Entity(..), Transaction, fromEntity )
import Database.SQLite.Simple ( Query(..), execute_, query_, withConnection )
import Test.HUnit             ( Test(..), Assertion, (@=?), runTestTT, assertFailure )

main = runTestTT $ TestList
    [ insertImageTest
    , selectImageTest
    , selectNextImageTest
    , selectPreviousImageTest
    , selectRandomImagesTest
    , selectImagesTest
    , selectImagesCountTest
    , deleteImageTest
    , updateImageTest
    , selectHashExistsTest
    , deleteAlbumTest
    , insertAlbumTest
    , updateAlbumTest
    , selectAlbumsTest
    , selectAlbumsCountTest
    , deleteScopeTest
    , insertScopeTest
    , selectScopeTest
    , updateScopeTest
    , attachTagsTest
    , detachTagsTest
    , selectTagsTest
    , cleanTagsTest ]

------------------------------------------------------------------- Image Tests

-- | Tests the deleteImage function.
deleteImageTest :: Test
deleteImageTest = testDatabase $ do
    id1 <- insertImage (Image "t1" False "h1" "e1" 2 4 (time 1) (time 4) 8 [])
    id2 <- insertImage (Image "t2" True  "h2" "e2" 1 3 (time 2) (time 5) 5 [])
    id3 <- insertImage (Image "t3" False "h3" "e3" 9 7 (time 3) (time 6) 6 [])

    numResults1 <- selectImagesCount []

    deletePost id2
    numResults2 <- selectImagesCount []

    deletePost 999
    numResults3 <- selectImagesCount []

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
        image2 = Image "t2" True  "h2" "e2" 1 3 (time 3) (time 4) 5 []

    id1 <- insertImage image1
    id2 <- insertImage image2

    [entity2, entity1] <- selectImages [] 0 50
    numResults         <- selectImagesCount []

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

-- | Tests the selectNextImage function.
selectNextImageTest :: Test
selectNextImageTest = testDatabase $ do
    let image1 = Image "t1" False "h1" "e1" 2 4 (time 1) (time 2) 8 []
        image2 = Image "t2" True  "h2" "e2" 1 3 (time 3) (time 4) 5 []
        image3 = Image "t3" False "h3" "e3" 9 7 (time 5) (time 6) 6 []

    id1 <- insertImage image1
    id2 <- insertImage image2
    id3 <- insertImage image3

    (Just (Entity id2' image2')) <- selectNextImage id3 []
    (Just (Entity id3' image3')) <- selectNextImage id1 []
    noResults                    <- selectNextImage 999 []

    lift $ do
        noResults @=? Nothing
        id2'      @=? id2
        image2'   @=? image2
        id3'      @=? id3
        image3'   @=? image3

-- | Tests the selectPreviousImage function.
selectPreviousImageTest :: Test
selectPreviousImageTest = testDatabase $ do
    let image1 = Image "t1" False "h1" "e1" 2 4 (time 1) (time 2) 8 []
        image2 = Image "t2" True  "h2" "e2" 1 3 (time 3) (time 4) 5 []
        image3 = Image "t3" False "h3" "e3" 9 7 (time 5) (time 6) 6 []

    id1 <- insertImage image1
    id2 <- insertImage image2
    id3 <- insertImage image3

    (Just (Entity id2' image2')) <- selectPreviousImage id1 []
    (Just (Entity id1' image1')) <- selectPreviousImage id3 []
    noResults                    <- selectPreviousImage 999 []

    lift $ do
        noResults @=? Nothing
        id1'      @=? id1
        image1'   @=? image1
        id2'      @=? id2
        image2'   @=? image2

-- | Tests the selectRandomImage and selectRandomImages functions.
selectRandomImagesTest :: Test
selectRandomImagesTest = testDatabase $ do
    let image1 = Image "t1" True  "h1" "e1" 1 2 (time 1) (time 2) 3 []
        image2 = Image "t2" True  "h2" "e2" 1 3 (time 3) (time 4) 5 []
        image3 = Image "t3" False "h3" "e3" 9 7 (time 5) (time 6) 6 []
        image4 = Image "t4" False "h4" "e4" 4 2 (time 7) (time 8) 9 []

        tags1 = ["goodbye", "hello",  "test"]
        tags2 = ["another", "couple", "test"]
        tags3 = ["blahblah", "hello", "test"]

    id1 <- insertImage image1
    id2 <- insertImage image2
    id3 <- insertImage image3
    id4 <- insertImage image4

    attachTags tags1 id1
    attachTags tags2 id2
    attachTags tags3 id3

    (Just (Entity id image)) <- selectRandomImage  (parse "another")
    allImages                <- selectRandomImages (parse "") 10
    noImage                  <- selectRandomImage  (parse "asdasda")
    images                   <- replicateM 100 (selectRandomImage [])

    lift $ do
        image2 { imageTagNames = tags2 } @=? image
        2                                @=? id
        Nothing                          @=? noImage
        4                                @=? length (nub images)

    lift $ do
        length allImages @=? 4
        True @=? Entity id1 image1 { imageTagNames = tags1 } `elem` allImages
        True @=? Entity id2 image2 { imageTagNames = tags2 } `elem` allImages
        True @=? Entity id3 image3 { imageTagNames = tags3 } `elem` allImages
        True @=? Entity id4 image4 { imageTagNames = []    } `elem` allImages

-- | Tests the selectImages function.
selectImagesTest :: Test
selectImagesTest = testDatabase $ do
    noResults <- selectImages [] 0 50

    id1 <- insertImage (Image "t1" False "h1" "e1" 2 4 (time 1) (time 2) 8 [])
    id2 <- insertImage (Image "t2" True  "h2" "e2" 1 3 (time 3) (time 4) 5 [])
    id3 <- insertImage (Image "t3" False "h3" "e3" 9 7 (time 5) (time 6) 6 [])
    id4 <- insertImage (Image "t4" False "h4" "e4" 4 2 (time 7) (time 8) 9 [])

    attachTags ["test", "hello", "goodbye"]  id1
    attachTags ["another", "couple", "test"] id2
    attachTags ["hello", "blahblah", "test"] id3

    (Just image1) <- selectImage id1
    (Just image2) <- selectImage id2
    (Just image3) <- selectImage id3
    (Just image4) <- selectImage id4

    allImages   <- selectImages [] 0 50
    firstImage  <- selectImages [] 0 1
    secondImage <- selectImages [] 1 1
    thirdImage  <- selectImages [] 2 1
    twoImages   <- selectImages [] 2 2

    lift $ do
        noResults   @=? []
        allImages   @=? [image4, image3, image2, image1]
        firstImage  @=? [image4]
        secondImage @=? [image3]
        thirdImage  @=? [image2]
        twoImages   @=? [image2, image1]

    results1 <- selectImages (parse "test")         0 50
    results2 <- selectImages (parse "hello")        0 50
    results3 <- selectImages (parse "goodbye")      0 50
    results4 <- selectImages (parse "test, hello")  0 50
    results5 <- selectImages (parse "-test")        0 50
    results6 <- selectImages (parse "test, -hello") 0 50
    results7 <- selectImages (parse "random")       0 50
    results8 <- selectImages []                     0 50

    lift $ do
        results1    @=? [image3, image2, image1]
        results2    @=? [image3, image1]
        results3    @=? [image1]
        results4    @=? [image3, image1]
        results5    @=? [image4]
        results6    @=? [image2]
        results7    @=? []
        results8    @=? [image4, image3, image2, image1]

-- | Tests the selectImagesCount function.
selectImagesCountTest :: Test
selectImagesCountTest = testDatabase $ do
    zero <- selectImagesCount []

    id1 <- insertImage (Image "t1" True  "h1" "e1" 1 2 (time 1) (time 2) 3 [])
    id2 <- insertImage (Image "t2" True  "h2" "e2" 1 3 (time 3) (time 4) 5 [])
    id3 <- insertImage (Image "t3" False "h3" "e3" 9 7 (time 5) (time 6) 6 [])
    id4 <- insertImage (Image "t4" False "h4" "e4" 4 2 (time 7) (time 8) 9 [])

    attachTags ["test", "hello", "goodbye"]  id1
    attachTags ["another", "couple", "test"] id2
    attachTags ["hello", "blahblah", "test"] id3

    two  <- selectImagesCount (parse "test, -blahblah")
    four <- selectImagesCount []

    lift $ do
        zero @=? 0
        two  @=? 2
        four @=? 4

-- | Tests the selectHashExists function.
selectHashExistsTest :: Test
selectHashExistsTest = testDatabase $ do
    result1 <- selectHashExists "h1"

    insertImage (Image "t1" True  "h1" "e1" 1 2 (time 1) (time 2) 3 [])
    insertImage (Image "t2" False "h2" "e2" 4 5 (time 3) (time 4) 6 [])

    result2 <- selectHashExists "h0"
    result3 <- selectHashExists "h1"
    result4 <- selectHashExists "h2"

    lift $ do
        result1 @=? False
        result2 @=? False
        result3 @=? True
        result4 @=? True

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

------------------------------------------------------------------- Album Tests

-- | Tests the deleteAlbum function.
deleteAlbumTest :: Test
deleteAlbumTest = testDatabase $ do
    let pages1 = [Page 1 "p1" "e1"]
        pages2 = [Page 2 "p2" "e2", Page 3 "p3" "e3"]

    id1 <- insertAlbum (Album "t1" False (time 1) (time 2) 8 pages1 [])
    id2 <- insertAlbum (Album "t2" True  (time 3) (time 4) 5 pages2 [])
    id3 <- insertAlbum (Album "t3" True  (time 6) (time 7) 9 []     [])

    numResults1 <- selectAlbumsCount []

    deletePost id2
    numResults2 <- selectAlbumsCount []

    deletePost 999
    numResults3 <- selectAlbumsCount []

    album1 <- selectAlbum id1
    album2 <- selectAlbum id2
    album3 <- selectAlbum id3
    [page] <- selectPages

    lift $ do
        numResults1   @=? 3
        numResults2   @=? 2
        numResults3   @=? 2
        isJust album1 @=? True
        isJust album2 @=? False
        isJust album3 @=? True
        page          @=? Entity 1 (Page 1 "p1" "e1")

-- | Tests the insertAlbum function.
insertAlbumTest :: Test
insertAlbumTest = testDatabase $ do
    let pages1 = [Page 1 "p1" "e1"]
        pages2 = [Page 2 "p2" "e2", Page 3 "p3" "e3"]
        album1 = Album "t1" False (time 1) (time 2) 8 pages1 []
        album2 = Album "t2" True  (time 3) (time 4) 5 pages2 []

    id1 <- insertAlbum album1
    id2 <- insertAlbum album2

    [entity2, entity1] <- selectAlbums [] 0 50
    numResults         <- selectAlbumsCount []

    lift $ do
        id1               @/? id2
        numResults        @=? 2
        Entity id1 album1 @=? entity1
        Entity id2 album2 @=? entity2

-- | Tests the selectAlbum function.
selectAlbumTest :: Test
selectAlbumTest = testDatabase $ do
    let pages1 = [Page 1 "p1" "e1"]
        pages2 = [Page 2 "p2" "e2", Page 3 "p3" "e3"]
        album1 = Album "t1" False (time 1) (time 2) 8 pages1 []
        album2 = Album "t2" True  (time 3) (time 4) 5 pages2 []
        album3 = Album "t3" True  (time 6) (time 7) 9 []     []

    id1 <- insertAlbum album1
    id2 <- insertAlbum album2
    id3 <- insertAlbum album3

    (Just (Entity id4 album4)) <- selectAlbum id1
    (Just (Entity id5 album5)) <- selectAlbum id2
    (Just (Entity id6 album6)) <- selectAlbum id3
    noResults                  <- selectAlbum 999

    lift $ do
        noResults @=? Nothing
        id1       @=? id4
        id2       @=? id5
        id3       @=? id6
        album1    @=? album4
        album2    @=? album5
        album3    @=? album6

-- | Tests the updateAlbum function
updateAlbumTest :: Test
updateAlbumTest = testDatabase $ do
    let pages1 = [Page 1 "p1" "e1"]
        pages2 = [Page 2 "p2" "e2", Page 3 "p3" "e3"]
        album1 = Album "t1" False (time 1) (time 2) 8 pages1 []
        album2 = Album "t2" True  (time 3) (time 4) 5 pages2 []
        album3 = album1
            { albumTitle = "t3"
            , albumIsFavourite = True
            , albumModified = time 10 }

    id1 <- insertAlbum album1
    id2 <- insertAlbum album2

    updateAlbum (Entity id1 album3)

    (Just (Entity _ album4)) <- selectAlbum id1
    (Just (Entity _ album5)) <- selectAlbum id2

    lift $ do
        album3 @=? album4
        album5 @=? album2

-- | Tests the selectAlbums function.
selectAlbumsTest :: Test
selectAlbumsTest = testDatabase $ do
    noResults <- selectImages [] 0 50

    let pages1 = [Page 1 "p1" "e1"]
        pages2 = [Page 2 "p2" "e2", Page 3 "p3" "e3"]

    id1 <- insertAlbum (Album "t1" False (time 10) (time 20) 30 pages1 [])
    id2 <- insertAlbum (Album "t2" True  (time 11) (time 21) 31 pages2 [])
    id3 <- insertAlbum (Album "t3" False (time 12) (time 22) 32 []     [])
    id4 <- insertAlbum (Album "t4" True  (time 13) (time 23) 33 []     [])

    attachTags ["test", "hello", "goodbye"]  id1
    attachTags ["another", "couple", "test"] id2
    attachTags ["hello", "blahblah", "test"] id3

    (Just album1) <- selectAlbum id1
    (Just album2) <- selectAlbum id2
    (Just album3) <- selectAlbum id3
    (Just album4) <- selectAlbum id4

    allAlbums   <- selectAlbums [] 0 50
    firstAlbum  <- selectAlbums [] 0 1
    secondAlbum <- selectAlbums [] 1 1
    thirdAlbum  <- selectAlbums [] 2 1
    twoAlbums   <- selectAlbums [] 2 2

    lift $ do
        noResults   @=? []
        allAlbums   @=? [album4, album3, album2, album1]
        firstAlbum  @=? [album4]
        secondAlbum @=? [album3]
        thirdAlbum  @=? [album2]
        twoAlbums   @=? [album2, album1]

    results1 <- selectAlbums (parse "test")         0 50
    results2 <- selectAlbums (parse "hello")        0 50
    results3 <- selectAlbums (parse "goodbye")      0 50
    results4 <- selectAlbums (parse "test, hello")  0 50
    results5 <- selectAlbums (parse "-test")        0 50
    results6 <- selectAlbums (parse "test, -hello") 0 50
    results7 <- selectAlbums (parse "random")       0 50
    results8 <- selectAlbums []                     0 50

    lift $ do
        results1    @=? [album3, album2, album1]
        results2    @=? [album3, album1]
        results3    @=? [album1]
        results4    @=? [album3, album1]
        results5    @=? [album4]
        results6    @=? [album2]
        results7    @=? []
        results8    @=? [album4, album3, album2, album1]

-- | Tests the selectAlbumsCount function.
selectAlbumsCountTest :: Test
selectAlbumsCountTest = testDatabase $ do
    zero <- selectAlbumsCount []

    let pages1 = [Page 1 "p1" "e1"]
        pages2 = [Page 2 "p2" "e2", Page 3 "p3" "e3"]

    id1 <- insertAlbum (Album "t1" False (time 10) (time 20) 30 pages1 [])
    id2 <- insertAlbum (Album "t2" True  (time 11) (time 21) 31 pages2 [])
    id3 <- insertAlbum (Album "t3" False (time 12) (time 22) 32 []     [])
    id4 <- insertAlbum (Album "t4" True  (time 13) (time 23) 33 []     [])

    attachTags ["test", "hello", "goodbye"]  id1
    attachTags ["another", "couple", "test"] id2
    attachTags ["hello", "blahblah", "test"] id3

    two  <- selectAlbumsCount (parse "test, -blahblah")
    four <- selectAlbumsCount []

    lift $ do
        zero @=? 0
        two  @=? 2
        four @=? 4

------------------------------------------------------------------- Scope Tests

-- | Tests the deleteScope function.
deleteScopeTest :: Test
deleteScopeTest = testDatabase $ do
    insertScope (Scope "n1" "e1")
    insertScope (Scope "n2" "e2")
    insertScope (Scope "n3" "e3")
    deleteScope "n2"

    result1 <- selectScope "n1"
    result2 <- selectScope "n2"
    result3 <- selectScope "n3"

    lift $ do
        Just (Entity 1 (Scope "n1" "e1")) @=? result1
        Nothing                           @=? result2
        Just (Entity 3 (Scope "n3" "e3")) @=? result3

-- | Tests the insertScope function.
insertScopeTest :: Test
insertScopeTest = testDatabase $ do
    insertScope (Scope "n1" "e1")
    insertScope (Scope "n2" "e2")

    (Just (Entity _ result1)) <- selectScope "n1"
    (Just (Entity _ result2)) <- selectScope "n2"

    lift $ do
        Scope "n1" "e1" @=? result1
        Scope "n2" "e2" @=? result2

-- | Tests the updateScope function
updateScopeTest :: Test
updateScopeTest = testDatabase $ do
    insertScope (Scope "n1" "e1")
    insertScope (Scope "n2" "e2")
    updateScope (Entity 1 (Scope "n3" "e3"))

    result1 <- selectScope "n1"
    result2 <- selectScope "n2"
    result3 <- selectScope "n3"

    lift $ do
        Nothing                           @=? result1
        Just (Entity 2 (Scope "n2" "e2")) @=? result2
        Just (Entity 1 (Scope "n3" "e3")) @=? result3

-- | Tests the selectScopeTest function.
selectScopeTest :: Test
selectScopeTest = testDatabase $ do
    insertScope (Scope "n1" "e1")
    insertScope (Scope "n2" "e2")
    insertScope (Scope "n3" "e3")

    result1 <- selectScope "n1"
    result2 <- selectScope "n4"

    lift $ do
        Just (Entity 1 (Scope "n1" "e1")) @=? result1
        Nothing                           @=? result2

--------------------------------------------------------------------- Tag Tests

-- Tests the selectTags function.
selectTagsTest :: Test
selectTagsTest = testDatabase $ do
    id1 <- insertImage (Image "t1" False "h1" "e1" 1 2 (time 1) (time 2) 5 [])
    id2 <- insertImage (Image "t2" True  "h2" "e2" 3 4 (time 3) (time 4) 6 [])

    attachTags ["test", "hello", "goodbye"]  id1
    attachTags ["another", "couple", "test"] id2

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

-- Tests the cleanTags function.
cleanTagsTest :: Test
cleanTagsTest = testDatabase $ do
    id1 <- insertImage (Image "t1" False "h1" "e1" 1 2 (time 1) (time 2) 5 [])
    id2 <- insertImage (Image "t2" True  "h2" "e2" 3 4 (time 3) (time 4) 6 [])
    id3 <- insertImage (Image "t3" False "h3" "e3" 7 8 (time 9) (time 0) 2 [])

    attachTags ["test", "hello", "goodbye"]  id1
    attachTags ["another", "couple", "test"] id2
    attachTags ["another", "hello", "test"]  id3

    results1 <- selectTags

    deletePost id1
    cleanTags

    results2 @ [(Entity _ (Tag name1)),
                (Entity _ (Tag name2)),
                (Entity _ (Tag name3)),
                (Entity _ (Tag name4))] <- selectTags

    lift $ do
        length results1 @=? 5
        length results2 @=? 4
        name1           @=? "another"
        name2           @=? "couple"
        name3           @=? "hello"
        name4           @=? "test"

------------------------------------------------------------ Relationship Tests

-- Tests the attachTags function.
attachTagsTest :: Test
attachTagsTest = testDatabase $ do
    id1 <- insertImage (Image "t1" False "h1" "e1" 1 2 (time 1) (time 2) 5 [])
    id2 <- insertImage (Image "t2" True  "h2" "e2" 3 4 (time 3) (time 4) 6 [])

    attachTags ["test1", "test2", "test3"] id1

    results1 <- selectImage id1
    results2 <- selectImage id2

    let tags      = imageTagNames $ fromEntity $ fromJust $ results1
        noResults = imageTagNames $ fromEntity $ fromJust $ results2

    lift $ do
        tags !! 0      @=? "test1"
        tags !! 1      @=? "test2"
        tags !! 2      @=? "test3"
        null noResults @=? True

-- Tests the detachTags function.
detachTagsTest :: Test
detachTagsTest = testDatabase $ do
    id1 <- insertImage (Image "t1" False "h1" "e1" 1 2 (time 1) (time 2) 5 [])
    id2 <- insertImage (Image "t2" True  "h2" "e2" 3 4 (time 3) (time 4) 6 [])

    attachTags ["t1", "t2", "t3"] id1
    attachTags ["t3", "t4", "t5"] id2
    detachTags ["t2", "t3"]      id1

    tags1 <- imageTagNames . fromEntity . fromJust <$> selectImage id1
    tags2 <- imageTagNames . fromEntity . fromJust <$> selectImage id2

    lift $ do
        ["t1"]             @=? tags1
        ["t3", "t4", "t5"] @=? tags2

----------------------------------------------------------------------- Utility

-- | Asserts that the specified actual value is not equal to the expected value
-- | (with the expected value on the left-hand side).
(@/?) :: (Show a, Eq a) => a -> a -> Assertion
(@/?) a b = when (a == b) (assertFailure (message ++ show a))
    where message = "Expected two different values but got: "

-- | Runs a test with an empty database.
testDatabase :: Transaction () -> Test
testDatabase f = TestCase $ withConnection ":memory:" $ \conn -> do
    commands <- getDatabaseMigration
    mapM_ (execute_ conn . Query) commands
    execute_ conn "PRAGMA foreign_keys = ON;"
    runReaderT f conn
    where getDatabaseMigration = filter (/="\n")
                                 <$> Text.splitOn ";"
                                 <$> Text.readFile "schema.sql"

-- | Returns the date time that is the given number of seconds after the epoch.
time :: Integer -> DateTime
time = fromSeconds

-- | Returns all the pages from the database.
selectPages :: Transaction [Entity Page]
selectPages = do
    conn <- ask
    lift $ query_ conn "SELECT id, number, title, extension FROM page"
