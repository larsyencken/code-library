//--------------------------------------------------------------------------//
// imageDatabase.cpp
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 sts=4 expandtab:
// Wed Feb  7 11:23:03 JST 2007
//--------------------------------------------------------------------------//

#include "imageDatabase.hpp"

#include <fstream>
#include <iostream>
#include <boost/python.hpp>
#include <math.h>
#include <stdio.h>
#include <qimage.h>

using namespace boost::python;

//--------------------------------------------------------------------------//
// MACROS
//--------------------------------------------------------------------------//

/**
 * Convenient max() function which is unit invariant.
 */
#define max__inline(a, b)  (((a) > (b)) ? (a) : (b))

/**
 * Convenient min() function which is unit invariant.
 */
#define min__inline(a, b)  (((a) > (b)) ? (b) : (a))

//--------------------------------------------------------------------------//
// GLOBALS
//--------------------------------------------------------------------------//

// Weights for the Haar coefficients.
// Straight from the referenced paper:
const float g_weights[2][6][3] = {
    // For scanned picture (sketch=0):
    //    Y      I      Q       idx total occurs
    {{5.00, 19.21, 34.37},      // 0   58.58      1 (`DC' component)
     {0.83, 1.26, 0.36},        // 1    2.45      3
     {1.01, 0.44, 0.45},        // 2    1.90      5
     {0.52, 0.53, 0.14},        // 3    1.19      7
     {0.47, 0.28, 0.18},        // 4    0.93      9
     {0.30, 0.14, 0.27}},       // 5    0.71      16384-25=16359

    // For handdrawn/painted sketch (sketch=1):
    //    Y      I      Q
    {{4.04, 15.14, 22.62},
     {0.78, 0.92, 0.40},
     {0.46, 0.53, 0.63},
     {0.42, 0.26, 0.25},
     {0.41, 0.14, 0.15},
     {0.32, 0.07, 0.38}}
};

//--------------------------------------------------------------------------//
// METHOD CODE
//--------------------------------------------------------------------------//

ImageDatabase::ImageDatabase()
{
    initializeBins();
    return;
}

//--------------------------------------------------------------------------//

ImageDatabase::ImageDatabase(const char* filename)
{
    bool success = loadFromFile(filename);

    if (not success) {
        std::cerr << "Error loading from the given file -- exitting" <<
            std::endl;
        exit(1);
    }
    return;
}

//--------------------------------------------------------------------------//

bool
ImageDatabase::loadFromFile(const char* filename)
{
    std::ifstream f(filename, std::ios::binary);
    if (!f.is_open()) {
        return false;
    }

    int sz;
    long int id;

    // read buckets
    for (int c = 0; c < 3; c++) {
        for (int pn = 0; pn < 2; pn++) {
            for (int i = 0; i < NUM_PIXELS_SQUARED; i++) {
                f.read((char *) &(sz), sizeof(int));
                for (int k = 0; k < sz; k++) {
                    f.read((char *) &(id), sizeof(long int));
                    m_imageBuckets[c][pn][i].push_back(id);
                }
            }
        }
    }

    // read sigs
    f.read((char *) &(sz), sizeof(int));
    for (int k = 0; k < sz; k++) {
        f.read((char *) &(id), sizeof(long int));
        m_sigs[id] = new Signature();
        f.read((char *) m_sigs[id], sizeof(Signature));
    }
    f.close();
    return true;
}

//--------------------------------------------------------------------------//

bool
ImageDatabase::saveToFile(const char* filename)
{
    /*
     * Serialization order: for each color {0,1,2}: for
     * {positive,negative}: for each 128x128 coefficient {0-16384}: [int]
     * bucket size (size of list of ids) for each id: [long int] image id
     * [int] number of images (signatures) for each image: [long id] image 
     * id for each sig coef {0-39}: (the NUM_COEFS greatest coefs) for
     * each color {0,1,2}: [int] coef index (signed) for each color
     * {0,1,2}: [double] average luminance [int] image width [int] image
     * height
     * 
     */
    std::ofstream f(filename, std::ios::binary);
    if (!f.is_open()) {
        return false;
    }

    int sz;
    long int id;

    // save buckets
    for (int c = 0; c < 3; c++) {
        for (int pn = 0; pn < 2; pn++) {
            for (int i = 0; i < NUM_PIXELS_SQUARED; i++) {
                sz = m_imageBuckets[c][pn][i].size();
                f.write((char *) &(sz), sizeof(int));
                ListLongIterator end = m_imageBuckets[c][pn][i].end();
                for (ListLongIterator it =
                     m_imageBuckets[c][pn][i].begin(); it != end; it++) {
                    f.write((char *) &((*it)), sizeof(long int));
                }
            }
        }
    }

    // save sigs
    sz = m_sigs.size();
    f.write((char *) &(sz), sizeof(int));
    for (sigIterator it = m_sigs.begin(); it != m_sigs.end(); it++) {
        id = (*it).first;
        f.write((char *) &(id), sizeof(long int));
        f.write((char *) (it->second), sizeof(Signature));
    }
    f.close();
    return true;
}

//--------------------------------------------------------------------------//

void
ImageDatabase::reset()
{
    for (int c = 0; c < NUM_CHANNELS; c++) {
        for (int pn = 0; pn < NUM_POLARITIES; pn++) {
            for (int i = 0; i < NUM_PIXELS_SQUARED; i++) {
                m_imageBuckets[c][pn][i].clear();
            }
        }
    }
    // delete sigs
    freeSignatures();
    return;
}

//--------------------------------------------------------------------------//

void
ImageDatabase::freeSignatures()
{
    // Free each signature, clearing the memory.
    for (sigIterator it = m_sigs.begin(); it != m_sigs.end(); it++)
        delete(*it).second;

    // Clear the map as well, so that no dangling pointers are left. 
    m_sigs.clear();
    return;
}

//--------------------------------------------------------------------------//

void
ImageDatabase::close()
{
    freeSignatures();
    return;
}

//--------------------------------------------------------------------------//

void
ImageDatabase::initializeBins()
{
    int i, j;

    /*
     *   0 1 2 3 4 5 6 i
     * 0 0 1 2 3 4 5 5
     * 1 1 1 2 3 4 5 5
     * 2 2 2 2 3 4 5 5
     * 3 3 3 3 3 4 5 5
     * 4 4 4 4 4 4 5 5
     * 5 5 5 5 5 5 5 5
     * 5 5 5 5 5 5 5 5
     * j 
     */

    /*
     * Every position has value 5, 
     */
    memset(m_imgBin, 5, NUM_PIXELS_SQUARED);

    /*
     * Except for the 5 by 5 upper-left quadrant: 
     */
    for (i = 0; i < 5; i++)
        for (j = 0; j < 5; j++)
            m_imgBin[i * NUM_PIXELS + j] = max__inline(i, j);

    // Note: imgBin[0] == 0
    return;
}

//--------------------------------------------------------------------------//

double
ImageDatabase::calcAvglDiff(long int id1, long int id2)
{
    if (!m_sigs.count(id1))
        return 0;

    if (!m_sigs.count(id2))
        return 0;

    return fabs(m_sigs[id1]->avgl[0] - m_sigs[id2]->avgl[0])
        + fabs(m_sigs[id1]->avgl[1] - m_sigs[id2]->avgl[1])
        + fabs(m_sigs[id1]->avgl[2] - m_sigs[id2]->avgl[2]);
}

//--------------------------------------------------------------------------//

double
ImageDatabase::calcDiff(long int id1, long int id2)
{
    double diff = calcAvglDiff(id1, id2);
    Index *sig1[3] =
        { m_sigs[id1]->sig1, m_sigs[id1]->sig2, m_sigs[id1]->sig3 };
    Index *sig2[3] =
        { m_sigs[id2]->sig1, m_sigs[id2]->sig2, m_sigs[id2]->sig3 };

    for (int b = 0; b < NUM_COEFS; b++) {
        for (int c = 0; c < NUM_CHANNELS; c++) {
            for (int b2 = 0; b2 < NUM_COEFS; b2++) {
                if (sig2[c][b2] == sig1[c][b]) {
                    diff -= g_weights[0][m_imgBin[abs(sig1[c][b])]][c];
                }
            }
        }
    }

    return diff;
}

//--------------------------------------------------------------------------//

int
ImageDatabase::addImage(long int id, const char* filename, const char* thname,
    bool doThumb, int ignDim)
{
    int cn;
    // Made static for speed; only used locally
    static Unit cdata1[NUM_PIXELS_SQUARED];
    static Unit cdata2[NUM_PIXELS_SQUARED];
    static Unit cdata3[NUM_PIXELS_SQUARED];
    int i;
    int width, height;

#ifdef ImMagick
    // #TODO2: speed things up. Reading pixel by pixel is definetely not
    // needed but imageMagick's writePixels didn't work. 
    // (there is some sort of bit packing problem on my part or theirs)
    Image image;
    try {
        image.read(filename);

        width = image.baseColumns();
        height = image.baseRows();
        if (ignDim && (width <= ignDim || height <= ignDim))
            return 2;

        if (doThumb) {
            Image im2(image);
            im2.scale("128x128");
            im2.write(thname);
        }
        image.sample("128x128!");   // force 128x128 dim
        unsigned char rchan[NUM_PIXELS_SQUARED];
        unsigned char gchan[NUM_PIXELS_SQUARED];
        unsigned char bchan[NUM_PIXELS_SQUARED];

        for (int idx = 0; idx < NUM_PIXELS_SQUARED; idx++) {
            rchan[idx] = pixel_cache->red;
            gchan[idx] = pixel_cache->green;
            bchan[idx] = pixel_cache->blue;
            pixel_cache++;
        }
        transformChar(rchan, gchan, bchan, cdata1, cdata2, cdata3);

    } catch(Exception & error_) {
        std::cout << "While adding image, caught exception: " << error_.
            what() << endl;
        return 0;
    }

#else                           // use QT
    QImage image = QImage();
    QString format = QImageIO::imageFormat(filename);

    if (!image.load(filename))
        return 0;

    width = image.width();
    height = image.height();
    if (ignDim && (width <= ignDim || height <= ignDim)) {
        return 2;
    }

    if (doThumb)
        image.smoothScale(NUM_PIXELS, NUM_PIXELS, QImage::ScaleMin).save(thname, "PNG");

    image = image.scale(NUM_PIXELS, NUM_PIXELS);

    for (i = 0, cn = 0; i < NUM_PIXELS; i++) {
        // Get a scanline:
        QRgb *line = (QRgb *) image.scanLine(i);

        for (int j = 0; j < NUM_PIXELS; j++) {
            QRgb pixel = line[j];

            cdata1[cn] = qRed(pixel);
            cdata2[cn] = qGreen(pixel);
            cdata3[cn] = qBlue(pixel);
            cn++;
        }
    }
    transform(cdata1, cdata2, cdata3);
#endif

    Signature *nsig = new Signature();
    nsig->id = id;
    nsig->width = width;
    nsig->height = height;

    if (m_sigs.count(id)) {
        printf("ID already in DB: %ld\n", id);
        delete m_sigs[id];
        m_sigs.erase(id);
    }
    m_sigs[id] = nsig;

    calcHaar(cdata1, cdata2, cdata3,
             nsig->sig1, nsig->sig2, nsig->sig3, nsig->avgl);

    for (i = 0; i < NUM_COEFS; i++) {   // populate buckets
        int x, t;

        // sig[i] never 0

        x = nsig->sig1[i];
        t = (x < 0);            /* t = 1 if x neg else 1 */
        /*
         * x - 0 ^ 0 = x; i - 1 ^ 0b111..1111 = 2-compl(x) = -x 
         */
        x = (x - t) ^ -t;
        m_imageBuckets[0][t][x].push_back(id);

        x = nsig->sig2[i];
        t = (x < 0);
        x = (x - t) ^ -t;
        m_imageBuckets[1][t][x].push_back(id);

        x = nsig->sig3[i];
        t = (x < 0);
        x = (x - t) ^ -t;
        m_imageBuckets[2][t][x].push_back(id);
    }
    return 1;
}

//--------------------------------------------------------------------------//

void
ImageDatabase::queryImageData(Index* sig1, Index* sig2, Index* sig3,
             double *avgl, int maxNumResults, ImageType sketch)
{
    int idx, c;
    int pn;
    Index *sig[3] = { sig1, sig2, sig3 };

    for (sigIterator sit = m_sigs.begin(); sit != m_sigs.end(); sit++) {
        // #TODO3: do I really need to score every single sig on db?
        (*sit).second->score = 0;
        for (c = 0; c < 3; c++) {
            (*sit).second->score += g_weights[sketch][0][c]
                * fabs((*sit).second->avgl[c] - avgl[c]);
        }
    }
    for (int b = 0; b < NUM_COEFS; b++) {   // for every coef on a sig
        for (c = 0; c < 3; c++) {
            pn = sig[c][b] <= 0;
            idx = (sig[c][b] - pn) ^ -pn;

            // update the score of every image which has this coef
            ListLongIterator end = m_imageBuckets[c][pn][idx].end();
            for (ListLongIterator uit = m_imageBuckets[c][pn][idx].begin();
                 uit != end; uit++) {
                m_sigs[(*uit)]->score -= g_weights[sketch][m_imgBin[idx]][c];
            }
        }
    }
    // make sure m_pqResults is empty.
    // TODO: any faster way to empty it ? didn't find any on STL refs.
    while (!m_pqResults.empty()) {
        m_pqResults.pop();
    }

    sigIterator sit = m_sigs.begin();

    // Fill up the maxNumResults-bounded priority queue (largest at top):
    for (int count = 0; count < maxNumResults; count++) {
        if (sit == m_sigs.end())
            // No more images; cannot get requested maxNumResults, alas.
            return;
        m_pqResults.push(*(*sit).second);
        sit++;
    }

    for (; sit != m_sigs.end(); sit++) {
        if ((*sit).second->score < m_pqResults.top().score) {
            // Make room by dropping largest entry:
            m_pqResults.pop();
            // Insert new entry:
            m_pqResults.push(*(*sit).second);
        }
    }
    return;
}

//--------------------------------------------------------------------------//

ListLong
ImageDatabase::queryImageDataForThres(SignatureMap * tsigs,
                     Index * sig1, Index * sig2, Index * sig3,
                     double *avgl, float thresd, ImageType sketch)
{
    int idx, c;
    int pn;
    ListLong res;
    Index *sig[3] = { sig1, sig2, sig3 };

    for (sigIterator sit = (*tsigs).begin(); sit != (*tsigs).end(); sit++) {
        // TODO: do I really need to score every single sig on db?
        (*sit).second->score = 0;
        for (c = 0; c < 3; c++)
            (*sit).second->score += g_weights[sketch][0][c]
                * fabs((*sit).second->avgl[c] - avgl[c]);
    }
    for (int b = 0; b < NUM_COEFS; b++) {   // for every coef on a sig
        for (c = 0; c < 3; c++) {
            pn = sig[c][b] <= 0;
            idx = (sig[c][b] - pn) ^ -pn;

            // update the score of every image which has this coef
            ListLongIterator end = m_imageBuckets[c][pn][idx].end();
            for (ListLongIterator uit = m_imageBuckets[c][pn][idx].begin();
                 uit != end; uit++) {
                if ((*tsigs).count((*uit)))
                    // this is an ugly line 
                    (*tsigs)[(*uit)]->score -=
                        g_weights[sketch][m_imgBin[idx]][c];
            }
        }
    }
    for (sigIterator sit = (*tsigs).begin(); sit != (*tsigs).end(); sit++) {
        if ((*sit).second->score < thresd) {
            res.push_back((*sit).second->id);
            (*tsigs).erase((*sit).second->id);
        }
    }
    return res;
}

//--------------------------------------------------------------------------//

ListLong
ImageDatabase::queryImageDataForThresFast(
        SignatureMap* tsigs, double *avgl,
        float thresd, ImageType sketch
    )
{
    // will only look for average luminance
    ListLong res;

    for (sigIterator sit = (*tsigs).begin(); sit != (*tsigs).end(); sit++) {
        (*sit).second->score = 0;
        for (int c = 0; c < 3; c++)
            (*sit).second->score += g_weights[sketch][0][c]
                * fabs((*sit).second->avgl[c] - avgl[c]);
        if ((*sit).second->score < thresd) {
            res.push_back((*sit).second->id);
            (*tsigs).erase((*sit).second->id);
        }
    }
    return res;
}

//--------------------------------------------------------------------------//

ListListLong
ImageDatabase::clusterSim(float thresd, bool fast)
{
    ListListLong res;           // will hold a list of lists. ie. a list
    // of clusters
    
    /* Temporary map of sigs. As soon as an image becomes part of a cluster,
     * it's removed from this map. */
    SignatureMap wSigs(m_sigs);

    /* Temporary map of sigs. As soon as an image becomes part of a cluster,
     * it's removed from this map. */
    SignatureMap wSigsTrack(m_sigs);

    // For every image in the database.
    for (sigIterator sit = wSigs.begin(); sit != wSigs.end(); sit++) {
        ListLong res2;

        if (fast) {
            res2 = queryImageDataForThresFast(&wSigs, (*sit).second->avgl,
                                         thresd, ImageType__Sketch);
        } else {
            res2 =
                queryImageDataForThres(&wSigs, (*sit).second->sig1,
                                     (*sit).second->sig2,
                                     (*sit).second->sig3,
                                     (*sit).second->avgl, thresd,
                                     ImageType__Sketch);
        }

        // Remove the "head" signature from the map of those neeeding to be
        // clustered.
        long int hid = (*sit).second->id;
        wSigs.erase(hid);

        if (res2.size() <= 1) {
            if (wSigs.size() <= 1) {
                // Everything has already been added to a cluster sim.
                // Bail out immediately, otherwise the next iteration
                // will segfault when incrementing sit.
                break;
            }
            continue;
        }
        res2.push_front(hid);
        res.push_back(res2);
        if (wSigs.size() <= 1) {
            break;
        }
        // sigIterator sit2 = wSigs.end();
        // sigIterator sit3 = sit++;
    }
    return res;
}

//--------------------------------------------------------------------------//

void
ImageDatabase::remove(long int id)
{
    if (!m_sigs.count(id)) {
        // Don't remove something which isn't even in the db.
        std::cout << "Attempt to remove invalid id: " << id << std::endl;
        return;
    }

    delete m_sigs[id];
    m_sigs.erase(id);

    // Remove id from each bucket it could be in.
    for (int c = 0; c < 3; c++) {
        for (int pn = 0; pn < 2; pn++) {
            for (int i = 0; i < NUM_PIXELS_SQUARED; i++) {
                m_imageBuckets[c][pn][i].remove(id);
            }
        }
    }

    return;
}

//--------------------------------------------------------------------------//

void
ImageDatabase::queryImageId(long int id, int maxNumResults)
{
    while (!m_pqResults.empty())
        m_pqResults.pop();

    if (!m_sigs.count(id)) {
        printf("ID not found.\n");
        return;
    }
    queryImageData(m_sigs[id]->sig1, m_sigs[id]->sig2, m_sigs[id]->sig3,
                 m_sigs[id]->avgl, maxNumResults, ImageType__Normal);
}

//--------------------------------------------------------------------------//

bool
ImageDatabase::queryImageFile(const char* filename, int maxNumResults,
        ImageType sketch)
{
    while (!m_pqResults.empty())
        m_pqResults.pop();

    double avgl[3];
    Index sig1[NUM_COEFS];
    Index sig2[NUM_COEFS];
    Index sig3[NUM_COEFS];
    int cn = 0;
    Unit cdata1[NUM_PIXELS_SQUARED];
    Unit cdata2[NUM_PIXELS_SQUARED];
    Unit cdata3[NUM_PIXELS_SQUARED];

#ifdef ImMagick
    Image image;
    try {
        image.read(filename);
        image.sample("128x128!");
        unsigned char rchan[NUM_PIXELS_SQUARED];
        unsigned char gchan[NUM_PIXELS_SQUARED];
        unsigned char bchan[NUM_PIXELS_SQUARED];
        Pixels view(image);
        PixelPacket *pixel_cache = view.get(0, 0, NUM_PIXELS, NUM_PIXELS);
        int idx = 0;
        // TODO: is this order inverted? check later
        for (int i = 0; i < NUM_PIXELS; i++)
            for (int j = 0; j < NUM_PIXELS; j++) {
                rchan[idx] = pixel_cache->red;
                gchan[idx] = pixel_cache->green;
                bchan[idx] = pixel_cache->blue;
                pixel_cache++;
                idx++;
            }
        transformChar(rchan, gchan, bchan, cdata1, cdata2, cdata3);
    }
    catch(Exception & error_) {
        std::cout << "While reading image, caught exception: " << error_.
            what() << endl;
        return false;
    }

#else                           // QT

    QImage image = QImage();
    if (!image.load(filename))
        return false;

    if (image.width() != NUM_PIXELS || image.height() != NUM_PIXELS)
        image = image.scale(NUM_PIXELS, NUM_PIXELS);

    for (int i = 0; i < NUM_PIXELS; i++) {
        // Get a scanline:
        QRgb *line = (QRgb *) image.scanLine(i);

        for (int j = 0; j < NUM_PIXELS; j++) {
            QRgb pixel = line[j];

            cdata1[cn] = qRed(pixel);
            cdata2[cn] = qGreen(pixel);
            cdata3[cn] = qBlue(pixel);
            cn++;
        }
    }
    transform(cdata1, cdata2, cdata3);

#endif

    calcHaar(cdata1, cdata2, cdata3, sig1, sig2, sig3, avgl);
    queryImageData(sig1, sig2, sig3, avgl, maxNumResults, sketch);

    return true;
}

//--------------------------------------------------------------------------//

ImageDatabase::~ImageDatabase()
{
    close();
}

//--------------------------------------------------------------------------//
//--------------------------------------------------------------------------//

void
export_ImageDatabase()
{
    class_<ImageDatabase>("ImageDatabase")
        .def("addImage", &ImageDatabase::addImage)
        .def("remove", &ImageDatabase::remove)
        .def("loadFromFile", &ImageDatabase::loadFromFile)
        .def("saveToFile", &ImageDatabase::saveToFile)
        .def("reset", &ImageDatabase::reset)
        .def("calcDiff", &ImageDatabase::calcDiff)
        .def("calcAvglDiff", &ImageDatabase::calcAvglDiff)
        .def("queryImageFile", &ImageDatabase::queryImageFile)
        .def("queryImageId", &ImageDatabase::queryImageId)
        .def("clusterSim", &ImageDatabase::clusterSim)
        ;

    enum_<ImageType>("ImageType")
        .value("Normal", ImageType__Normal)
        .value("Sketch", ImageType__Sketch)
        ;

    return;
}

//--------------------------------------------------------------------------//
