/**
 * @file cell.cpp
 * @brief A benchmark and generator for cellular (Worley-like) textures.
 *
 * This program generates a grayscale cellular texture using a set of randomly
 * distributed feature points. It implements and benchmarks multiple algorithms
 * for finding the two nearest points to generate the texture.
 */

#define _CRT_SECURE_NO_WARNINGS

#include <algorithm>
#include <array>
#include <cassert>
#include <chrono>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <limits>
#include <memory>
#include <random>
#include <string>
#include <system_error>
#include <vector>

#include <emmintrin.h> // SSE2 intrinsics

using std::max;
using std::min;

static std::mt19937 gRng(std::random_device{}());
static std::uniform_real_distribution<float> gUniform01(0.0f, 1.0f);

/**
 * @brief Clamps a value to a specified range.
 * @param x The value to clamp.
 * @param lowerBound The lower bound of the range.
 * @param upperBound The upper bound of the range.
 * @return The clamped value.
 */
static inline float clamp(float x, float lowerBound, float upperBound)
{
  return std::min(std::max(x, lowerBound), upperBound);
}

/**
 * @brief Clamps a value to the range [0.0, 1.0].
 * @param x The value to saturate.
 * @return The saturated value.
 */
static inline float saturate(float x) { return clamp(x, 0.0f, 1.0f); }

/**
 * @brief Squares a value.
 * @param x The value to square.
 * @return The squared value.
 */
static inline float square(float x) { return x * x; }

/**
 * @brief Computes the fractional part of a number.
 * @param x The input number.
 * @return The fractional part of x.
 */
static inline float frac(float x) { return x - std::floor(x); }

/**
 * @brief Swaps two values.
 * @tparam T The type of the values.
 * @param a The first value.
 * @param b The second value.
 */
template <typename T>
static void swap(T &a, T &b)
{
  T t = a;
  a = b;
  b = t;
}

typedef float Intens;

/**
 * @class Image
 * @brief A lightweight container for a grayscale image.
 *
 * This class stores image data as a flat vector of floating-point intensities
 * in row-major order.
 */
class Image
{
  std::vector<Intens> Pixels;

public:
  int SizeX, SizeY;

  /**
   * @brief Constructs an Image with the given dimensions.
   * @param sx The width of the image.
   * @param sy The height of the image.
   */
  Image(int sx, int sy) : Pixels(static_cast<size_t>(sx) * sy), SizeX(sx), SizeY(sy) {}

  /**
   * @brief Gets a pointer to the beginning of a row.
   * @param y The row index.
   * @return A pointer to the first pixel in the row.
   */
  Intens *Row(int y) { return Pixels.data() + static_cast<size_t>(y) * SizeX; }

  /**
   * @brief Gets a const pointer to the beginning of a row.
   * @param y The row index.
   * @return A const pointer to the first pixel in the row.
   */
  const Intens *Row(int y) const
  {
    return Pixels.data() + static_cast<size_t>(y) * SizeX;
  }
};

/**
 * @brief Saves an image as an 8-bit grayscale TGA file.
 * @param filename The path to save the image to.
 * @param img The image to save.
 * @return True if the image was saved successfully, false otherwise.
 */
static bool saveImageTGA(const std::filesystem::path &filename, const Image &img)
{
  unsigned char header[18] = {
      0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 32,
  };

  header[0x0c] = img.SizeX & 0xff;
  header[0x0d] = (img.SizeX >> 8) & 0xff;
  header[0x0e] = img.SizeY & 0xff;
  header[0x0f] = (img.SizeY >> 8) & 0xff;

  namespace fs = std::filesystem;

  if (!filename.empty())
  {
    const fs::path parentDir = filename.parent_path();
    if (!parentDir.empty())
    {
      std::error_code ec;
      fs::create_directories(parentDir, ec);
      if (ec)
        return false;
    }
  }

  const std::string filePath = filename.string();
  std::unique_ptr<FILE, decltype(&std::fclose)> file(std::fopen(filePath.c_str(), "wb"), &std::fclose);
  if (!file)
    return false;

  bool ok = std::fwrite(header, sizeof(header), 1, file.get()) == 1;

  std::vector<unsigned char> lineBuf(static_cast<size_t>(img.SizeX));
  for (int y = 0; y < img.SizeY; y++)
  {
    const Intens *src = img.Row(y);
    for (int x = 0; x < img.SizeX; x++)
      lineBuf[x] = (unsigned char)(saturate(src[x]) * 255.0f);

    ok &= std::fwrite(lineBuf.data(), static_cast<size_t>(img.SizeX), 1, file.get()) == 1;
  }

  return ok;
}

/**
 * @struct Point
 * @brief Represents a 2D point with floating-point coordinates.
 */
struct Point
{
  float x, y;

  Point() : x(0.0f), y(0.0f) {}
  Point(float _x, float _y) : x(_x), y(_y) {}
};

/**
 * @brief Calculates the wrapped distance between two points on a toroidal axis.
 * @param a The first point.
 * @param b The second point.
 * @return The wrapped distance.
 */
static inline float wrapDist(float a, float b)
{
  float d = std::fabs(b - a);
  return min(d, 1.0f - d);
}

/**
 * @brief Calculates the distance from a point to the boundary of a region.
 * @param x The coordinate of the point.
 * @param mid The midpoint of the region.
 * @param r The radius of the region.
 * @return The distance to the boundary.
 */
static float distanceBound(float x, float mid, float r)
{
  return max(wrapDist(x, mid) - r, 0.0f);
}

/**
 * @brief Calculates the squared wrapped distance between two 2D points.
 * @param a The first point.
 * @param b The second point.
 * @return The squared wrapped distance.
 */
static inline float wrapDistSq(const Point &a, const Point &b)
{
  return square(wrapDist(a.x, b.x)) + square(wrapDist(a.y, b.y));
}

/**
 * @brief Calculates the wrapped distance between two 2D points.
 * @param a The first point.
 * @param b The second point.
 * @return The wrapped distance.
 */
static inline float wrapDist(const Point &a, const Point &b)
{
  return std::sqrt(wrapDistSq(a, b));
}

/**
 * @brief Generates random points with a minimum separation distance.
 * @param pts An array to store the generated points.
 * @param count The number of points to generate.
 * @param minDist The minimum distance between points.
 * @return The number of points successfully generated.
 */
static int genRandomPoints(Point pts[], int count, float minDist)
{
  static const int maxTries = 10;
  float minDistSq = square(minDist);

  for (int i = 0; i < count; i++)
  {
    int attempt = 0, j;

    do
    {
      if (++attempt > maxTries)
        return i;

      pts[i].x = gUniform01(gRng);
      pts[i].y = gUniform01(gRng);

      for (j = 0; j < i; j++)
        if (wrapDistSq(pts[i], pts[j]) < minDistSq)
          break;
    } while (j < i);
  }

  return count;
}

/**
 * @brief Calculates the cellular intensity based on the two nearest distances.
 * @param dist The distance to the nearest point.
 * @param dist2 The distance to the second nearest point.
 * @return The calculated intensity.
 */
static inline float cellIntensity(float dist, float dist2)
{
  return 2.0f * dist / (dist2 + dist);
}

/**
 * @brief Calculates the cellular intensity using SSE intrinsics.
 * @param dist An __m128 vector of distances to the nearest points.
 * @param dist2 An __m128 vector of distances to the second nearest points.
 * @return An __m128 vector of calculated intensities.
 */
static inline __m128 cellIntensity(__m128 dist, __m128 dist2)
{
  __m128 sum = _mm_add_ps(dist2, dist);
  __m128 distx2 = _mm_add_ps(dist, dist);
  __m128 intens = _mm_div_ps(distx2, sum);

  return intens;
}

/**
 * @struct KeyedPoint
 * @brief A point with an associated key for sorting and pruning.
 */
struct KeyedPoint
{
  Point pt;
  float key;
  float temp;
};

/**
 * @brief Converts an array of Points to a vector of KeyedPoints.
 * @param pts The input array of Points.
 * @param count The number of points.
 * @return A vector of KeyedPoints.
 */
static std::vector<KeyedPoint> makeKeyed(const Point pts[], int count)
{
  std::vector<KeyedPoint> out(static_cast<size_t>(count));
  for (int i = 0; i < count; i++)
  {
    out[static_cast<size_t>(i)].pt = pts[i];
    out[static_cast<size_t>(i)].key = 0.0f;
    out[static_cast<size_t>(i)].temp = 0.0f;
  }

  return out;
}

/**
 * @brief Inserts a KeyedPoint into a sorted array.
 * @param pts The array of KeyedPoints.
 * @param count The number of points in the array.
 * @param pt The Point to insert.
 * @param key The key for sorting.
 * @param temp An auxiliary temporary value.
 */
static void insertKeyedPoint(KeyedPoint pts[], int count, Point pt, float key,
                             float temp = 0.0f)
{
  int j;
  for (j = count; j > 0 && key < pts[j - 1].key; j--)
    pts[j] = pts[j - 1];

  pts[j].pt = pt;
  pts[j].key = key;
  pts[j].temp = temp;
}

/**
 * @brief Generates a cellular texture using a brute-force algorithm.
 * @param out The output image.
 * @param pts The array of feature points.
 * @param count The number of feature points.
 */
static void cellularTexBruteForce(Image &out, const Point pts[], int count)
{
  for (int y = 0; y < out.SizeY; y++)
  {
    Intens *dest = out.Row(y);
    Point cur(0.0f, float(y) / out.SizeY);

    for (int x = 0; x < out.SizeX; x++)
    {
      cur.x = float(x) / out.SizeX;

      float best = 1.0f, best2 = 1.0f;

      for (int i = 0; i < count; i++)
      {
        float d = wrapDistSq(cur, pts[i]);
        if (d < best2)
        {
          if (d < best)
            best2 = best, best = d;
          else
            best2 = d;
        }
      }

      dest[x] = cellIntensity(std::sqrt(best), std::sqrt(best2));
    }
  }
}

/**
 * @brief Generates a cellular texture using a Y-sorted pruning algorithm.
 * @param out The output image.
 * @param ptIn The array of feature points.
 * @param count The number of feature points.
 */
static void cellularTexSortY(Image &out, const Point ptIn[], int count)
{
  auto ptsBuf = makeKeyed(ptIn, count);
  KeyedPoint *pts = ptsBuf.data();

  for (int y = 0; y < out.SizeY; y++)
  {
    Intens *dest = out.Row(y);
    float curY = float(y) / out.SizeY;

    for (int i = 0; i < count; i++)
      insertKeyedPoint(pts, i, pts[i].pt, square(wrapDist(pts[i].pt.y, curY)));

    for (int x = 0; x < out.SizeX; x++)
    {
      float curX = float(x) / out.SizeX;

      float best = 1.0f, best2 = 1.0f;

      for (int i = 0; i < count && best2 > pts[i].key; i++)
      {
        float d = square(wrapDist(pts[i].pt.x, curX)) + pts[i].key;
        if (d < best2)
        {
          if (d < best)
            best2 = best, best = d;
          else
            best2 = d;
        }
      }

      dest[x] = cellIntensity(std::sqrt(best), std::sqrt(best2));
    }
  }

}

/**
 * @brief Generates a cellular texture using a Y-sorted pruning algorithm with SSE.
 * @param out The output image.
 * @param ptIn The array of feature points.
 * @param count The number of feature points.
 */
static void cellularTexSortY_SSE(Image &out, const Point ptIn[], int count)
{
  assert(out.SizeX % 4 == 0);
  float stepX = 1.0f / out.SizeX;

  auto ptsBuf = makeKeyed(ptIn, count);
  KeyedPoint *pts = ptsBuf.data();

  for (int y = 0; y < out.SizeY; y++)
  {
    Intens *dest = out.Row(y);
    float curY = float(y) / out.SizeY;

    for (int i = 0; i < count; i++)
      insertKeyedPoint(pts, i, pts[i].pt, square(wrapDist(pts[i].pt.y, curY)));

    static const __declspec(align(16)) unsigned int clearSignC[4] = {
        0x7fffffff, 0x7fffffff, 0x7fffffff, 0x7fffffff};

    __m128 curX =
        _mm_setr_ps(stepX * 0.0f, stepX * 1.0f, stepX * 2.0f, stepX * 3.0f);
    __m128 stepX4 = _mm_set1_ps(stepX * 4.0f);
    __m128 _one = _mm_set1_ps(1.0f);
    __m128 clearSign = _mm_load_ps((const float *)clearSignC);

    for (int x = 0; x < out.SizeX; x += 4)
    {
      __m128 best = _one, best2 = _one;

      for (int i = 0; i < count; i++)
      {
        __m128 pt = _mm_loadu_ps(&pts[i].pt.x);
        __m128 ptKey = _mm_shuffle_ps(pt, pt, 0xaa);
        __m128 cmp = _mm_cmpgt_ps(best2, ptKey);
        if (!_mm_movemask_ps(cmp))
          break;

        __m128 ptX = _mm_shuffle_ps(pt, pt, 0x00);
        __m128 dx = _mm_sub_ps(ptX, curX);
        __m128 absdx = _mm_and_ps(dx, clearSign);
        __m128 omad = _mm_sub_ps(_one, absdx);
        __m128 distx = _mm_min_ps(absdx, omad);
        __m128 distx2 = _mm_mul_ps(distx, distx);
        __m128 dist = _mm_add_ps(distx2, ptKey);

        __m128 cmpb2 = _mm_cmpgt_ps(best2, dist);
        __m128 cmpb = _mm_cmpgt_ps(best, dist);

        __m128 xor1 = _mm_xor_ps(dist, best);
        __m128 xorM = _mm_and_ps(xor1, cmpb);
        dist = _mm_xor_ps(dist, xorM);
        best = _mm_xor_ps(best, xorM);

        dist = _mm_and_ps(dist, cmpb2);
        best2 = _mm_andnot_ps(cmpb2, best2);
        best2 = _mm_or_ps(best2, dist);
      }

      __m128 intens = cellIntensity(_mm_sqrt_ps(best), _mm_sqrt_ps(best2));

      _mm_storeu_ps(dest + x, intens);
      curX = _mm_add_ps(curX, stepX4);
    }
  }
}

typedef void (*CellularFunc)(Image &out, const Point pts[], int count);

/**
 * @brief Measures the execution time of a cellular texture generation function.
 * @param func The function to measure.
 * @param name The name of the function for printing.
 * @param out The output image.
 * @param pts The array of feature points.
 * @param count The number of feature points.
 */
static void measure(CellularFunc func, const char *name, Image &out,
                    const Point pts[], int count)
{
  using namespace std::chrono;

  printf("%20s: ", name);
  fflush(stdout);

  long long minTime = std::numeric_limits<long long>::max();

  for (int i = 0; i < 3; i++)
  {
    const auto timeStart = steady_clock::now();
    func(out, pts, count);
    const auto elapsed = duration_cast<microseconds>(steady_clock::now() - timeStart);
    minTime = std::min<long long>(minTime, elapsed.count());
  }

  printf("%9lld microseconds\n", minTime);
}

/**
 * @brief The main entry point for the program.
 * @return 0 on success.
 */
int main()
{
  static const int maxPts = 2048;
  static const int numPts[] = {64, 128, 256, 512, 1024};

  Image img(1024, 1024);
  std::vector<Point> pts(maxPts);
  gRng.seed(1337u);

  for (int j = 0; j < sizeof(numPts) / sizeof(*numPts); j++)
  {
    int count = genRandomPoints(pts.data(), numPts[j], 0.01f);
    printf("%d points.\n", count);

    static const struct MeasureTarget
    {
      CellularFunc Target;
      const char *Name;
    } targets[] = {
        {cellularTexBruteForce, "brute force"},
        {cellularTexSortY, "sort by y"},
        {cellularTexSortY_SSE, "sort by y, SSE"},
    };

    for (int i = 0; i < sizeof(targets) / sizeof(*targets); i++)
      measure(targets[i].Target, targets[i].Name, img, pts.data(), count);

    printf("\n");
  }

  namespace fs = std::filesystem;
  const fs::path outputPath = fs::path("challenges") / "Emulation" / "CellularTextures" / "test.tga";

  saveImageTGA(outputPath, img);

  return 0;
}
