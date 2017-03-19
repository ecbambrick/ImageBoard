import qualified App.Core.Album      as Album
import qualified App.Core.Everything as Everything
import qualified App.Core.Image      as Image
import qualified App.Core.Post       as Post
import qualified App.Core.Scope      as Scope
import qualified App.Core.Tag        as Tag

import Test.Hspec

main :: IO ()
main = hspec $ do

    describe "albums" $ do
        describe "count" $ do
            it "counts 0 when there are no albums" pending
            it "counts 0 when there are no matching albums" pending
            it "counts every album when given an empty expression" pending
            it "counts matching albums when given a non-empty expression" pending
        describe "delete" $ do
            it "does nothing when no album with the given ID exists" pending
            it "deletes the album with the given ID" pending
            it "marks the album with the given ID as deleted" pending
        describe "query" $ do
            it "returns nothing when there are no albums" pending
            it "returns nothing when there are no matching albums" pending
            it "returns every album when given an empty expression" pending
            it "returns matching albums when given a non-empty expression" pending
        describe "querySingle" $ do
            it "returns nothing when no album with the given ID exists" pending
            it "returns the album with the given ID" pending
        describe "update" $ do
            it "does not update anything when there is no album with the given ID" pending
            it "does not update anything given an invalid title or tags" pending
            it "updates the title and tags of the album with the given ID" pending

    describe "images" $ do
        describe "count" $ do
            it "counts 0 when there are no images" pending
            it "counts 0 when there are no matching images" pending
            it "counts every image when given an empty expression" pending
            it "counts matching images when given a non-empty expression" pending
        describe "delete" $ do
            it "does nothing when no image with the given ID exists" pending
            it "deletes the image with the given ID" pending
            it "marks the image with the given ID as deleted" pending
        describe "query" $ do
            it "returns nothing when there are no images" pending
            it "returns nothing when there are no matching images" pending
            it "returns every image when given an empty expression" pending
            it "returns matching images when given a non-empty expression" pending
        describe "querySingle" $ do
            it "returns nothing when no image with the given ID exists" pending
            it "returns the image with the given ID" pending
        describe "queryTriple" $ do
            it "returns the image with the given ID and its neighbours" pending
            it "returns the image with the given ID three times when it has no neighbours" pending
            it "returns nothing when no image with the given ID exists" pending
        describe "update" $ do
            it "does not update anything when there is no image with the given ID" pending
            it "does not update anything given an invalid title or tags" pending
            it "updates the title and tags of the image with the given ID" pending

    describe "posts" $ do
        describe "insert" $ do
            it "does not insert a zip file with an invalid title or tags" pending
            it "does not insert an image file with an invalid title or tags" pending
            it "does not insert a video file with an invalid title or tags" pending
            it "does not insert an invalid file format" pending
            it "inserts a zip file with a valid title and tags" pending
            it "inserts an image file with a valid title and tags" pending
            it "inserts a video file with a valid title and tags" pending

    describe "everything" $ do
        describe "delete" $ do
            it "does nothing when the database does not exist" pending
            it "deletes everything in the database" pending
        describe "initialize" $ do
            it "fails when the database's directory does not exist" pending
            it "does nothing when a database already exists" pending
            it "initializes an empty database when one does not already exist" pending

    describe "scopes" $ do
        describe "delete" $ do
            it "does nothing when no scope with the given name exists" pending
            it "deletes the scope with the given name" pending
        describe "insertOrUpdate" $ do
            it "does not insert or update anything when the given scope name is invalid" pending
            it "inserts a new scope when no scope with the same name exists" pending
            it "updates the expression of the scope with the given name" pending
        describe "querySingle" $ do
            it "returns nothing when no scope with the given name exists" pending
            it "returns the scope with the given name" pending

    describe "tags" $ do
        describe "query" $ do
            it "returns nothing when there are no tags" pending
            it "returns nothing when there are no matching tags" pending
            it "returns every tag when given an empty expression" pending
            it "returns matching tags when given a non-empty expression" pending
        describe "categorize" $ do
            it "does not categorize a non-existant tag" pending
            it "does not categorize a non-existant category" pending
            it "associates the given tag with each given category" pending
