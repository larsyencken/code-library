//--------------------------------------------------------------------------//
// imageDatabase.hpp
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 sts=4 et tw=78:
// Wed Feb  7 11:22:12 2007
//
// A modified version of the imgSeek database implementation by Ricardo
// Niederberger Cabral <nieder|at|mail.ru>.
//--------------------------------------------------------------------------//

#ifndef IMAGEDATABASE_HPP
#define IMAGEDATABASE_HPP

#include "haar.hpp"
#include "signature.hpp"
#include "listLong.hpp"

//--------------------------------------------------------------------------//

// Whether the image is a normal image, or just a sketch. 
enum ImageType { ImageType__Normal = 0, ImageType__Sketch = 1 };

//--------------------------------------------------------------------------//

const int NUM_CHANNELS = 3;
const int NUM_POLARITIES = 2;

//--------------------------------------------------------------------------//

/**
 * A database of images. When an image is added, its signature is calculated
 * and stored in the database. Querying is the available by id, filename or
 * signature similarilty.
 */
class ImageDatabase
{
    public:
        // *** CONSTRUCTORS *** //

        /**
         * Build a new empty database.
         */
        ImageDatabase();

        /**
         * Load a database from a filename.
         */
        ImageDatabase(const char* filename);

        // *** ACCESSORS *** //

        /**
         * Save to the given filename.
         */
        bool saveToFile(const char* filename);

        /**
         * Search for the image using a query image.
         *
         * @param sig1 The first channel of length NUM_CEOFS.
         * @param sig2 The second channel.
         * @param sig3 The third channel.
         * @param avgl The average luminance.
         * @param maxNumResults The maximum number of results to return.
         * @param imageType The type of the image used in the query.
         */
        void queryImageData(Index* sig1, Index* sig2, Index* sig3,
                double* avgl, int maxNumResults, ImageType imageType);

        /**
         * Finds all matches which are greater than a given similarity
         * threshold. Note that every search result is removed from tsigs
         * (right now this function is only used by clusterSim). 
         *
         * @param sig1 The first channel array of length NUM_COEFS.
         * @param avgl The average luminance.
         * @param thresd The limit similarity threshold. Only images with
         *      score > thresd will be a result.
         * @param imageType Which set of weights to use.
         * @param tsigs The source to query on (map of signatures).
         */
        ListLong queryImageDataForThres(
                SignatureMap* tsigs,
                Index* sig1, Index* sig2, Index* sig3,
                double* avgl, float thresd, ImageType imageType
            );

        /**
         * XXX Similar to queryImageDataForThres() ?
         */
        ListLong queryImageDataForThresFast(
                SignatureMap* tsigs,
                double* avgl, float thresd, ImageType imageType
            );

        /*
         * Query for images similar to the one that has this id.
         *
         * @param id The id of the image used as a query.
         * @param maxNumResults The maximum number of results.
         */
        void queryImageId(long int id, int maxNumResults);

        /*
         * Query for images similar to the one on filename.
         *
         * @param filename The location of the image to use as a query. 
         * @param maxNumResults The maximum number of results.
         * @param imageType Whether this image is a drawing.
         * @return true on success, false otherwise.
         */
        bool queryImageFile(const char* filename, int maxNumResults,
                ImageType imageType);

        /**
         * Use it to tell the content-based difference between two images.
         */
        double calcDiff(long int id1, long int id2);

        /**
         * Return the average luminance difference between the two images.
         */
        double calcAvglDiff(long int id1, long int id2);

        /**
         * Cluster by similarity. Returns list of list of long ints (img ids).
         */
        ListListLong clusterSim(float thresd, bool fast=false);

        // *** MANIPULATORS *** //
        
        /**
         * Load from the given filename.
         */
        bool loadFromFile(const char* filename);

        /**
         * Add this image to the database, and optionally generate a
         * thumbnail for it at the same time.
         *
         * @param id A unique image identifier.
         * @param filename The image location.
         * @param thname The thumbnail location for this image.
         * @param doThumb Set to 1 if you want to save the thumbnail on thname.
         * @param ignDim Images with a dimension smaller than ignDim are
         *      ignored.
         * @return 0 on error, 1 on success and 2 if skipped.
         */
        int addImage(long int id, const char* filename, const char* thname,
                bool doThumb, int ignDim=0);

        /**
         * Remove the given image signature from the database. Prints an
         * error message and returns if the given id does not exist.
         *
         * @param id The id of the image to remove.
         */
        void remove(long int id);

        /**
         * Reset the database, clearing all existing signatures.
         *
         * @return true on success, false otherwise.
         */
        void reset();

        /**
         * Reset the database, clearing all existing signatures.
         *
         * @return true on success, false otherwise.
         */
        void close();

        // *** DESTRUCTOR *** //

        /**
         * Destructor, frees memory associated with this database.
         */
        ~ImageDatabase();

    private:
        // *** MANIPULATORS *** //

        /**
         * Setup initial fixed weights that each coefficient represents.
         */
        void initializeBins();

        /**
         * Empties the signature map, freeing the associated memory.
         */
        void freeSignatures();

        // *** ATTRIBUTES *** //

        /**
         * The mapping for which attributes go into which bin. Effectively
         * constant once initialized.
         */
        unsigned char m_imgBin[NUM_PIXELS_SQUARED];

        /**
         * The database mapping image id to signature. 
         */
        SignatureMap m_sigs;

        /**
         * Results priority queue; largest at the top.
         */
        SignatureQueue m_pqResults;

        /**
         * Current result waiting to be returned.
         */
        Signature m_curResult;

        /*
         * Number of results found in a query.
         */
        int m_numResults;

        /**
         * Lists of picture ids, indexed by [color-channel][sign][position],
         * i.e., R=0/G=1/B=2, pos=0/neg=1, (i*NUM_PIXELS+j)
         */
        ListLong m_imageBuckets[NUM_CHANNELS][NUM_POLARITIES][NUM_PIXELS_SQUARED];

};

//--------------------------------------------------------------------------//

/**
 * Export Python interface for the ImageDatabase class.
 */
void export_ImageDatabase();

//--------------------------------------------------------------------------//

#endif

