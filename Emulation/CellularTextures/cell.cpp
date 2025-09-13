#define _CRT_SECURE_NO_WARNINGS

#include <assert.h>
#include <malloc.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include <emmintrin.h>

template <typename T> static inline T min(T a, T b) { return a < b ? a : b; }

template <typename T> static inline T max(T a, T b) { return a > b ? a : b; }

static inline float clamp(float x, float lowerBound, float upperBound) {
  return min(max(x, lowerBound), upperBound);
}

static inline float saturate(float x) { return clamp(x, 0.0f, 1.0f); }

static inline float square(float x) { return x * x; }

static inline float frac(float x) { return x - floor(x); }

template <typename T> static void swap(T &a, T &b) {
  T t = a;
  a = b;
  b = t;
}

typedef float Intens;

class Image {
  Intens *Pixels;

public:
  int SizeX, SizeY;

  Image(int sx, int sy) : SizeX(sx), SizeY(sy), Pixels(new Intens[sx * sy]) {}

  ~Image() { delete[] Pixels; }

  Intens *Row(int y) { return Pixels + y * SizeX; }
  const Intens *Row(int y) const { return Pixels + y * SizeX; }
};

static bool saveImageTGA(const char *filename, const Image &img) {
  unsigned char header[18] = {
      0,              // 0x00: no ident struct
      0,              // 0x01: no color map
      3,              // 0x02: grayscale
      0,  0, 0, 0, 0, // 0x03: color map descriptor filled with zeroes
      0,  0,          // 0x08: x origin
      0,  0,          // 0x0a: y origin
      0,  0,          // 0x0c: put width here
      0,  0,          // 0x0e: put height here
      8,              // 0x10: 8 bits/pixels
      32,             // 0x11: descriptor bits (top-down tga)
  };

  header[0x0c] = img.SizeX & 0xff;
  header[0x0d] = (img.SizeX >> 8) & 0xff;
  header[0x0e] = img.SizeY & 0xff;
  header[0x0f] = (img.SizeY >> 8) & 0xff;

  FILE *f = fopen(filename, "wb");
  if (!f)
    return false;

  bool ok = true;
  ok &= fwrite(header, sizeof(header), 1, f) == 1;

  unsigned char *lineBuf = (unsigned char *)alloca(img.SizeX);
  for (int y = 0; y < img.SizeY; y++) {
    const Intens *src = img.Row(y);
    for (int x = 0; x < img.SizeX; x++)
      lineBuf[x] = (unsigned char)(saturate(src[x]) * 255.0f);

    ok &= fwrite(lineBuf, img.SizeX, 1, f) == 1;
  }

  fclose(f);
  return ok;
}

struct Point {
  float x, y;

  Point() : x(0.0f), y(0.0f) {}
  Point(float _x, float _y) : x(_x), y(_y) {}
};

static inline float wrapDist(float a, float b) {
  float d = fabs(b - a);
  return min(d, 1.0f - d);
}

static float distanceBound(float x, float mid, float r) {
  return max(wrapDist(x, mid) - r, 0.0f);
}

static inline float wrapDistSq(const Point &a, const Point &b) {
  return square(wrapDist(a.x, b.x)) + square(wrapDist(a.y, b.y));
}

static inline float wrapDist(const Point &a, const Point &b) {
  return sqrtf(wrapDistSq(a, b));
}

static int genRandomPoints(Point pts[], int count, float minDist) {
  static const int maxTries = 10;
  float minDistSq = square(minDist);

  for (int i = 0; i < count; i++) {
    int attempt = 0, j;

    do {
      if (++attempt > maxTries)
        return i;

      pts[i].x = float(rand()) / RAND_MAX;
      pts[i].y = float(rand()) / RAND_MAX;

      for (j = 0; j < i; j++)
        if (wrapDistSq(pts[i], pts[j]) < minDistSq)
          break;
    } while (j < i);
  }

  return count;
}

static inline float cellIntensity(float dist, float dist2) {
  return 2.0f * dist / (dist2 + dist);
}

static inline __m128 cellIntensity(__m128 dist, __m128 dist2) {
  __m128 sum = _mm_add_ps(dist2, dist);
  __m128 distx2 = _mm_add_ps(dist, dist);
  __m128 intens = _mm_div_ps(distx2, sum);

  return intens;
}

struct KeyedPoint {
  Point pt;
  float key;
  float temp;
};

static KeyedPoint *makeKeyed(const Point pts[], int count) {
  KeyedPoint *out = new KeyedPoint[count];
  for (int i = 0; i < count; i++) {
    out[i].pt = pts[i];
    out[i].key = out[i].temp = 0.0f;
  }

  return out;
}

static void insertKeyedPoint(KeyedPoint pts[], int count, Point pt, float key,
                             float temp = 0.0f) {
  int j;
  for (j = count; j > 0 && key < pts[j - 1].key; j--)
    pts[j] = pts[j - 1];

  pts[j].pt = pt;
  pts[j].key = key;
  pts[j].temp = temp;
}

static void cellularTexBruteForce(Image &out, const Point pts[], int count) {
  for (int y = 0; y < out.SizeY; y++) {
    Intens *dest = out.Row(y);
    Point cur(0.0f, float(y) / out.SizeY);

    for (int x = 0; x < out.SizeX; x++) {
      cur.x = float(x) / out.SizeX;

      float best = 1.0f, best2 = 1.0f;

      for (int i = 0; i < count; i++) {
        float d = wrapDistSq(cur, pts[i]);
        if (d < best2) {
          if (d < best)
            best2 = best, best = d;
          else
            best2 = d;
        }
      }

      dest[x] = cellIntensity(sqrtf(best), sqrtf(best2));
    }
  }
}

static void cellularTexSortY(Image &out, const Point ptIn[], int count) {
  KeyedPoint *pts = makeKeyed(ptIn, count);

  for (int y = 0; y < out.SizeY; y++) {
    Intens *dest = out.Row(y);
    float curY = float(y) / out.SizeY;

    for (int i = 0; i < count; i++)
      insertKeyedPoint(pts, i, pts[i].pt, square(wrapDist(pts[i].pt.y, curY)));

    for (int x = 0; x < out.SizeX; x++) {
      float curX = float(x) / out.SizeX;

      float best = 1.0f, best2 = 1.0f;

      for (int i = 0; i < count && best2 > pts[i].key; i++) {
        float d = square(wrapDist(pts[i].pt.x, curX)) + pts[i].key;
        if (d < best2) {
          if (d < best)
            best2 = best, best = d;
          else
            best2 = d;
        }
      }

      dest[x] = cellIntensity(sqrtf(best), sqrtf(best2));
    }
  }

  delete[] pts;
}

static void cellularTexSortY_SSE(Image &out, const Point ptIn[], int count) {
  assert(out.SizeX % 4 == 0);
  float stepX = 1.0f / out.SizeX;

  KeyedPoint *pts = makeKeyed(ptIn, count);

  for (int y = 0; y < out.SizeY; y++) {
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

    for (int x = 0; x < out.SizeX; x += 4) {
      __m128 best = _one, best2 = _one;

      for (int i = 0; i < count; i++) {
        __m128 pt = _mm_loadu_ps(&pts[i].pt.x);
        __m128 ptKey = _mm_shuffle_ps(pt, pt, 0xaa); // splat key
        __m128 cmp =
            _mm_cmpgt_ps(best2, ptKey); // not done for any of the pixels?
        if (!_mm_movemask_ps(cmp))      // if none are greater, we can stop
          break;

        __m128 ptX = _mm_shuffle_ps(pt, pt, 0x00); // splat x
        __m128 dx = _mm_sub_ps(ptX, curX);
        __m128 absdx = _mm_and_ps(dx, clearSign);
        __m128 omad = _mm_sub_ps(_one, absdx);  // 1-abs(dx)
        __m128 distx = _mm_min_ps(absdx, omad); // min(abs(dx),1-abs(dx))
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

  delete[] pts;
}

static void cellularTexTilesRect(Image &out, int x0, int y0, int x1, int y1,
                                 KeyedPoint pts[], int count) {
  static int tileSize = 32;

  for (int ty = y0; ty < y1; ty += tileSize) {
    int endY = min(ty + tileSize, y1);
    float y0 = float(ty) / out.SizeY;
    float radY = 0.5f * float(endY - ty - 1) / out.SizeY;
    float midY = y0 + radY;

    for (int i = 0; i < count; i++)
      pts[i].temp = square(distanceBound(pts[i].pt.y, midY, radY));

    for (int tx = x0; tx < x1; tx += tileSize) {
      int endX = min(tx + tileSize, x1);
      float x0 = float(tx) / out.SizeX;
      float radX = 0.5f * float(endX - tx - 1) / out.SizeX;
      float midX = x0 + radX;

      for (int i = 0; i < count; i++)
        insertKeyedPoint(pts, i, pts[i].pt,
                         pts[i].temp +
                             square(distanceBound(pts[i].pt.x, midX, radX)),
                         pts[i].temp);

      for (int y = ty; y < endY; y++) {
        Intens *dest = out.Row(y);
        Point cur(0.0f, float(y) / out.SizeY);

        for (int x = tx; x < endX; x++) {
          cur.x = float(x) / out.SizeX;

          float best = 1.0f, best2 = 1.0f;
          for (int i = 0; i < count && best2 > pts[i].key; i++) {
            float d = wrapDistSq(cur, pts[i].pt);
            if (d < best2) {
              if (d < best)
                best2 = best, best = d;
              else
                best2 = d;
            }
          }

          dest[x] = cellIntensity(sqrtf(best), sqrtf(best2));
        }
      }
    }
  }
}

static void cellularTexTiles(Image &out, const Point ptIn[], int count) {
  KeyedPoint *pts = makeKeyed(ptIn, count);
  cellularTexTilesRect(out, 0, 0, out.SizeX, out.SizeY, pts, count);
  delete[] pts;
}

static void cellularTexTilesRect_SSE(Image &out, int x0, int y0, int x1, int y1,
                                     KeyedPoint pts[], int count) {
  static int tileSize = 32;

  float stepX = 1.0f / out.SizeX;
  float stepY = 1.0f / out.SizeY;

  for (int ty = y0; ty < y1; ty += tileSize) {
    int endY = min(ty + tileSize, y1);
    float y0 = float(ty) * stepY;
    float radY = 0.5f * float(endY - ty - 1) * stepY;
    float midY = y0 + radY;

    for (int i = 0; i < count; i++)
      pts[i].temp = square(distanceBound(pts[i].pt.y, midY, radY));

    for (int tx = x0; tx < x1; tx += tileSize) {
      int endX = min(tx + tileSize, x1);
      float x0 = float(tx) * stepX;
      float radX = 0.5f * float(endX - tx - 1) * stepX;
      float midX = x0 + radX;

      for (int i = 0; i < count; i++)
        insertKeyedPoint(pts, i, pts[i].pt,
                         pts[i].temp +
                             square(distanceBound(pts[i].pt.x, midX, radX)),
                         pts[i].temp);

      assert((endX - tx) % 4 == 0);

      static const __declspec(align(16)) unsigned int clearSignC[4] = {
          0x7fffffff, 0x7fffffff, 0x7fffffff, 0x7fffffff};

      __m128 stepX4 = _mm_set1_ps(stepX * 4.0f);
      __m128 _one = _mm_set1_ps(1.0f);
      __m128 clearSign = _mm_load_ps((const float *)clearSignC);
      __m128 curY = _mm_set1_ps(y0);
      __m128 stepYv = _mm_set1_ps(stepY);

      for (int y = ty; y < endY; y++) {
        Intens *dest = out.Row(y);
        __m128 curX = _mm_setr_ps(x0 + stepX * 0.0f, x0 + stepX * 1.0f,
                                  x0 + stepX * 2.0f, x0 + stepX * 3.0f);

        for (int x = tx; x < endX; x += 4) {

          __m128 best = _one, best2 = _one;

          for (int i = 0; i < count; i++) {
            __m128 pt = _mm_loadu_ps(&pts[i].pt.x);
            __m128 ptKey = _mm_shuffle_ps(pt, pt, 0xaa); // splat key
            __m128 cmp =
                _mm_cmpgt_ps(best2, ptKey); // not done for any of the pixels?
            if (!_mm_movemask_ps(cmp))      // if none are greater, we can stop
              break;

            __m128 ptX = _mm_shuffle_ps(pt, pt, 0x00); // splat x
            __m128 ptY = _mm_shuffle_ps(pt, pt, 0x55); // splat y
            __m128 dx = _mm_sub_ps(ptX, curX);
            __m128 dy = _mm_sub_ps(ptY, curY);
            __m128 absdx = _mm_and_ps(dx, clearSign);
            __m128 absdy = _mm_and_ps(dy, clearSign);
            __m128 omadx = _mm_sub_ps(_one, absdx);  // 1-abs(dx)
            __m128 omady = _mm_sub_ps(_one, absdy);  // 1-abs(dy)
            __m128 distx = _mm_min_ps(absdx, omadx); // min(abs(dx),1-abs(dx))
            __m128 disty = _mm_min_ps(absdy, omady); // min(abs(dy),1-abs(dy))
            __m128 distx2 = _mm_mul_ps(distx, distx);
            __m128 disty2 = _mm_mul_ps(disty, disty);
            __m128 dist = _mm_add_ps(distx2, disty2);

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

        curY = _mm_add_ps(curY, stepYv);
      }
    }
  }
}

static void cellularTexTiles_SSE(Image &out, const Point ptIn[], int count) {
  KeyedPoint *pts = makeKeyed(ptIn, count);
  cellularTexTilesRect_SSE(out, 0, 0, out.SizeX, out.SizeY, pts, count);
  delete[] pts;
}

struct TreeNode {
  int point2;       // second support point (1st is implicit)
  float radius;     // bounding sphere radius
  TreeNode *kid[2]; // child nodes
};

class Tree {
  Point *points;
  TreeNode *nodes;

public:
  Tree(const Point pts[], int nPoints);
  ~Tree();

  void findNearestTwo(const Point &pt, float &best, float &best2);

private:
  void calcBoundsR(TreeNode *root, int point1);
  void calcBoundsR2(TreeNode *target, const Point &center, TreeNode *current);

  void findNearestTwoR(const Point &pt, float &best, float &best2, int point1,
                       float dist1, TreeNode *cur);
};

Tree::Tree(const Point pts[], int nPoints) {
  assert(nPoints >= 2);

  points = new Point[nPoints];
  for (int i = 0; i < nPoints; i++)
    points[i] = pts[i];

  nodes = new TreeNode[nPoints - 1];

  for (int i = 1; i < nPoints; i++) {
    TreeNode *newNode = nodes + (i - 1);
    newNode->point2 = i;
    newNode->kid[0] = newNode->kid[1] = 0;

    if (i > 1) {
      TreeNode *cur = nodes;
      float p1DistSq = wrapDistSq(points[i], points[0]);

      for (;;) {
        float p2DistSq = wrapDistSq(points[i], points[cur->point2]);
        int idx = 0;

        if (p2DistSq < p1DistSq) // right child is nearer
        {
          p1DistSq = p2DistSq; // right anchor point becomes new anchor point
          idx = 1;
        }

        if (cur->kid[idx])
          cur = cur->kid[idx];
        else {
          cur->kid[idx] = newNode;
          break;
        }
      }
    }
  }

  calcBoundsR(nodes, 0);
}

Tree::~Tree() {
  delete[] points;
  delete[] nodes;
}

void Tree::calcBoundsR(TreeNode *root, int point1) {
  int point2 = root->point2;
  if (root->kid[0])
    calcBoundsR(root->kid[0], point1);
  if (root->kid[1])
    calcBoundsR(root->kid[1], point2);

  root->radius = root->kid[0] ? square(root->kid[0]->radius) : 0.0f;

  const Point &pt1 = points[point1];
  root->radius = max(root->radius, wrapDistSq(pt1, points[point2]));

  if (root->kid[1])
    calcBoundsR2(root, pt1, root->kid[1]);

  root->radius = sqrtf(root->radius);
}

void Tree::findNearestTwo(const Point &pt, float &best, float &best2) {
  float dist1 = wrapDist(pt, points[0]);
  findNearestTwoR(pt, best, best2, 0, dist1, nodes);
}

void Tree::calcBoundsR2(TreeNode *target, const Point &center, TreeNode *cur) {
  target->radius = max(target->radius, wrapDistSq(center, points[cur->point2]));

  if (cur->kid[0])
    calcBoundsR2(target, center, cur->kid[0]);
  if (cur->kid[1])
    calcBoundsR2(target, center, cur->kid[1]);
}

void Tree::findNearestTwoR(const Point &pt, float &best, float &best2,
                           int point1, float dist1, TreeNode *cur) {
  if (cur) // inner node
  {
    int point2 = cur->point2;
    float dist2 = wrapDist(pt, points[point2]);
    if (dist1 < dist2) // first point is closer
    {
      findNearestTwoR(pt, best, best2, point1, dist1, cur->kid[0]);

      float rad2 = cur->kid[1] ? cur->kid[1]->radius : 0.0f;
      if (dist2 < best2 + rad2)
        findNearestTwoR(pt, best, best2, point2, dist2, cur->kid[1]);
    } else // second point is closer
    {
      findNearestTwoR(pt, best, best2, point2, dist2, cur->kid[1]);

      float rad1 = cur->kid[0] ? cur->kid[0]->radius : 0.0f;
      if (dist1 < best2 + rad1)
        findNearestTwoR(pt, best, best2, point1, dist1, cur->kid[0]);
    }
  } else // leaf node
  {
    if (dist1 < best2) {
      if (dist1 < best) // new best point!
        best2 = best, best = dist1;
      else // new second best
        best2 = dist1;
    }
  }
}

static void cellularTexTree(Image &out, const Point pts[], int count) {
  Tree tree(pts, count);
  int besti = -1, besti2 = -1;

  for (int y = 0; y < out.SizeY; y++) {
    Intens *dest = out.Row(y);
    Point cur(0.0f, float(y) / out.SizeY);

    for (int x = 0; x < out.SizeX; x++) {
      cur.x = float(x) / out.SizeX;

      float best = 1.0f, best2 = 1.0f;
      tree.findNearestTwo(cur, best, best2);

      dest[x] = cellIntensity(best, best2);
    }
  }
}

static void cellularTexSpatialSubdR(Image &out, int x0, int y0, int x1, int y1,
                                    float radius, KeyedPoint pts[], int count) {
  static const int minSize = 16;

  int hx = (x1 - x0) / 2; // diameter in x/y of subdivided rectangles
  int hy = (y1 - y0) / 2;
  float subRad =
      0.5f * radius; // "radius" (half-extent) of subdivided rectangles

  for (int i = 0; i < 4; i++) {
    int sx0 = x0 + ((i & 1) ? hx : 0);
    int sy0 = y0 + ((i & 2) ? hy : 0);

    float midX = float(sx0) / out.SizeX + subRad;
    float midY = float(sy0) / out.SizeY + subRad;

    int outCount = 0, realIn = 0;
    for (int j = 0; j < count; j++) {
      Point pt = pts[j].pt;
      float bx = distanceBound(pt.x, midX, subRad);
      float by = distanceBound(pt.y, midY, subRad);

      if (bx < radius && by < radius) {
        if (bx == 0.0f && by == 0.0f) // point inside subrect
          realIn++;

        swap(pts[outCount++], pts[j]);
      }
    }

    if (realIn >= 2 && hx >= minSize && hy >= minSize)
      cellularTexSpatialSubdR(out, sx0, sy0, sx0 + hx, sy0 + hy, subRad, pts,
                              outCount);
    else
      cellularTexTilesRect_SSE(out, sx0, sy0, sx0 + hx, sy0 + hy, pts, count);
  }
}

static void cellularTexSpatialSubd(Image &out, const Point ptIn[], int count) {
  KeyedPoint *pts = makeKeyed(ptIn, count);
  cellularTexSpatialSubdR(out, 0, 0, out.SizeX, out.SizeY, 0.5f, pts, count);
  delete[] pts;
}

typedef void (*CellularFunc)(Image &out, const Point pts[], int count);

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

static LARGE_INTEGER timeFreq;

static void initTimeUS() {
  QueryPerformanceFrequency(&timeFreq);
  timeFreq.QuadPart /= 1000000;
}

static int getTimeUS() {
  LARGE_INTEGER now;
  QueryPerformanceCounter(&now);
  return (int)(now.QuadPart / timeFreq.QuadPart);
}

static void measure(CellularFunc func, const char *name, Image &out,
                    const Point pts[], int count) {
  printf("%20s: ", name);
  fflush(stdout);

  int minTime = 0x7fffffff;

  for (int i = 0; i < 3; i++) {
    int timeStart = getTimeUS();
    func(out, pts, count);
    minTime = min(minTime, getTimeUS() - timeStart);
  }

  printf("%9d microseconds\n", minTime);
}

int main() {
  static const int maxPts = 2048;
  static const int numPts[] = {64, 128, 256, 512, 1024};

  initTimeUS();

  Image img(1024, 1024);
  Point pts[maxPts];

  for (int j = 0; j < sizeof(numPts) / sizeof(*numPts); j++) {
    int count = genRandomPoints(pts, numPts[j], 0.01f);
    printf("%d points.\n", count);

    static const struct MeasureTarget {
      CellularFunc Target;
      const char *Name;
    } targets[] = {
        cellularTexBruteForce,  "brute force",
        cellularTexTree,        "tree",
        cellularTexSortY,       "sort by y",
        cellularTexSortY_SSE,   "sort by y, SSE",
        cellularTexTiles,       "tiles",
        cellularTexTiles_SSE,   "tiles, SSE",
        cellularTexSpatialSubd, "spatial subd",
    };

    for (int i = 0; i < sizeof(targets) / sizeof(*targets); i++)
      measure(targets[i].Target, targets[i].Name, img, pts, count);

    printf("\n");
  }

  saveImageTGA("Emulation\\Easy\\CellularTextures\\test.tga", img);

  return 0;
}