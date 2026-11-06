#define NOMINMAX
#include <glad/glad.h>
#include <GLFW/glfw3.h>
#include <vector>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstdint>
#include <cstring>
#include <algorithm>
#include <random>
using namespace std;

static const float PI = 3.14159265f;
static const float TILE = 3.f;          // размер клетки лабиринта (м)
static const float LV = 2.0f;           // высота одного этажа (м)
static const int   RES = 8;             // шагов сетки пещеры на клетку
static const float HS = TILE / RES;     // шаг сетки пещеры (м)
static const int   STEPS = 10;          // ступеней в лестнице (высота ступени 0.2 м)
static mt19937 rng(random_device{}());
static float rndf() { return uniform_real_distribution<float>(0, 1)(rng); }
static int rndi(int n) { return (int)(rng() % (unsigned)n); }
static float cl01(float v) { return v < 0 ? 0 : (v > 1 ? 1 : v); }

struct V3 { float x, y, z; };
static V3 operator+(V3 a, V3 b) { return { a.x + b.x, a.y + b.y, a.z + b.z }; }
static V3 operator-(V3 a, V3 b) { return { a.x - b.x, a.y - b.y, a.z - b.z }; }
static V3 operator*(V3 a, float s) { return { a.x * s, a.y * s, a.z * s }; }

// =====================================================================
//  „ј—“№ 1. Ўум и процедурные текстуры (цвет + карта нормалей с высотой в альфе)
// =====================================================================
static unsigned hash32(unsigned x) { x ^= x >> 16; x *= 0x7feb352dU; x ^= x >> 15; x *= 0x846ca68bU; x ^= x >> 16; return x; }
static float hrand(int x, int y, int seed) {
    return (hash32((unsigned)x * 374761393u + (unsigned)y * 668265263u + (unsigned)seed * 2246822519u) & 0xFFFFFF) / 16777216.f;
}
static float smooth01(float t) { return t * t * (3 - 2 * t); }
// Ђбесшовныйї value-noise: u,v в [0,1), pu,pv Ч период по ос€м
static float vnoise(float u, float v, int pu, int pv, int seed) {
    float x = u * pu, y = v * pv; int ix = (int)floorf(x), iy = (int)floorf(y);
    float fx = smooth01(x - ix), fy = smooth01(y - iy);
    int x0 = ((ix % pu) + pu) % pu, x1 = (x0 + 1) % pu, y0 = ((iy % pv) + pv) % pv, y1 = (y0 + 1) % pv;
    float a = hrand(x0, y0, seed), b = hrand(x1, y0, seed), c = hrand(x0, y1, seed), d = hrand(x1, y1, seed);
    return a + (b - a) * fx + (c - a) * fy + (a - b - c + d) * fx * fy;
}
static float fbm(float u, float v, int per, int oct, int seed) {
    float s = 0, a = .5f, n = 0;
    for (int o = 0; o < oct; o++) { s += a * vnoise(u, v, per << o, per << o, seed + o); n += a; a *= .5f; }
    return s / n;
}
// обычный (не бесшовный) шум дл€ карты пещеры
static float vn2(float x, float y, int seed) {
    int ix = (int)floorf(x), iy = (int)floorf(y); float fx = smooth01(x - ix), fy = smooth01(y - iy);
    float a = hrand(ix, iy, seed), b = hrand(ix + 1, iy, seed), c = hrand(ix, iy + 1, seed), d = hrand(ix + 1, iy + 1, seed);
    return a + (b - a) * fx + (c - a) * fy + (a - b - c + d) * fx * fy;
}
static float fbm2(float x, float y, int seed) { return .55f * vn2(x, y, seed) + .3f * vn2(x * 2.1f, y * 2.1f, seed + 1) + .15f * vn2(x * 4.3f, y * 4.3f, seed + 2); }

static GLuint tWall, tRock, tFloor, tWood, tFlame, tGlow, tParch;   // цветовые текстуры
static GLuint nWall, nRock, nFloor, nWood, nFlat;                   // карты нормалей (RGB Ч нормаль, A Ч высота)

static GLuint makeTex(int w, int h, const vector<unsigned char>& px, int comps) {
    GLuint t; glGenTextures(1, &t); glBindTexture(GL_TEXTURE_2D, t);
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
    glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameterf(GL_TEXTURE_2D, 0x84FE, 8.f);   // анизотропна€ фильтраци€ (если есть)
    GLenum f = comps == 4 ? GL_RGBA : GL_RGB;
    glTexImage2D(GL_TEXTURE_2D, 0, f, w, h, 0, f, GL_UNSIGNED_BYTE, px.data());
    return t;
}
static void putPx(vector<unsigned char>& px, int N, int x, int y, float r, float g, float b) {
    unsigned char* p = &px[((size_t)y * N + x) * 3];
    p[0] = (unsigned char)(cl01(r) * 255); p[1] = (unsigned char)(cl01(g) * 255); p[2] = (unsigned char)(cl01(b) * 255);
}
// из цвета и карты высот делаем две текстуры: цвет и нормали (бесшовно)
static void makeMat(int N, const vector<float>& col, const vector<float>& hg, float str, GLuint& tA, GLuint& tN) {
    vector<unsigned char> a((size_t)N * N * 3), nm((size_t)N * N * 4);
    for (int y = 0; y < N; y++) for (int x = 0; x < N; x++) {
        size_t k = (size_t)y * N + x;
        putPx(a, N, x, y, col[k * 3], col[k * 3 + 1], col[k * 3 + 2]);
        float dx = (hg[(size_t)y * N + (x + 1) % N] - hg[(size_t)y * N + (x + N - 1) % N]) * str;
        float dy = (hg[(size_t)((y + 1) % N) * N + x] - hg[(size_t)((y + N - 1) % N) * N + x]) * str;
        float l = sqrtf(dx * dx + dy * dy + 1);
        unsigned char* p = &nm[k * 4];
        p[0] = (unsigned char)((-dx / l * .5f + .5f) * 255); p[1] = (unsigned char)((-dy / l * .5f + .5f) * 255);
        p[2] = (unsigned char)((1 / l * .5f + .5f) * 255); p[3] = (unsigned char)(cl01(hg[k]) * 255);
    }
    tA = makeTex(N, N, a, 3); tN = makeTex(N, N, nm, 4);
}
static void setCol(vector<float>& c, size_t k, float r, float g, float b) { c[k * 3] = r; c[k * 3 + 1] = g; c[k * 3 + 2] = b; }

//  аменна€ кладка: р€ды блоков разной ширины, сколы, швы с раствором, трещины, мох, подтЄки
static void genWallTex() {
    const int N = 512, R = 8, RH = N / R; vector<float> col((size_t)N * N * 3), hg((size_t)N * N);
    vector<float> cuts[R]; float off[R];
    for (int r = 0; r < R; r++) {
        off[r] = hrand(r, 1, 301) * N; int nb = 3 + (int)(hrand(r, 2, 301) * 2.99f);
        for (int b = 0; b < nb; b++) cuts[r].push_back(b == 0 ? 0.f : (b + (hrand(r, b + 10, 301) - .5f) * .5f) * N / nb);
        cuts[r].push_back((float)N);
    }
    for (int y = 0; y < N; y++) for (int x = 0; x < N; x++) {
        size_t k = (size_t)y * N + x; float uu = x / (float)N, vv = y / (float)N;
        float wx = x + (vnoise(uu, vv, 16, 16, 311) - .5f) * 12.f, wy = y + (vnoise(uu, vv, 16, 16, 312) - .5f) * 7.f;
        float rf = floorf(wy / RH); int r = (((int)rf % R) + R) % R; float ly = wy - rf * RH;
        float xs = fmodf(wx + off[r] + 4.f * N, (float)N);
        int b = 0; while (b + 2 < (int)cuts[r].size() && xs >= cuts[r][b + 1]) b++;
        float e = min(min(xs - cuts[r][b], cuts[r][b + 1] - xs), min(ly, RH - ly));
        int id = r * 16 + b;
        float chip = vnoise(uu, vv, 40, 40, 313), fine = vnoise(uu, vv, 128, 128, 315), bul = fbm(uu, vv, 16, 4, 314);
        float edge = smooth01(cl01((e - 2.f - chip * 5.f) / 10.f));          // 0 Ч шов, 1 Ч лицо камн€
        float h = edge * (.55f + .3f * bul + .1f * hrand(id, 4, 319)) + edge * fine * .07f;
        float cr = fabsf(vnoise(uu, vv, 12, 12, 316) * 2.f - 1.f), crk = cr < .018f ? .5f + .5f * cr / .018f : 1.f;
        h *= crk;
        float tint = .78f + .3f * hrand(id, 5, 320), warm = hrand(id, 6, 321) - .5f;
        float g = .50f * tint * (.75f + .45f * bul) * (.85f + .3f * fine);
        float damp = .78f + .22f * vnoise(uu, vv, 32, 3, 322);               // вертикальные подтЄки
        float cr_ = g * (1 + .06f * warm) * damp, cg = g * damp, cb = g * (1 - .04f * warm) * damp;
        float m = 1 - edge;                                                    // раствор в швах
        cr_ += (.10f - cr_) * m; cg += (.095f - cg) * m; cb += (.09f - cb) * m;
        float moss = cl01((fbm(uu, vv, 6, 4, 317) - .5f) * 3.f) * (.25f + .75f * m), mk = .7f + .6f * fine;
        cr_ += (.11f * mk - cr_) * moss * .7f; cg += (.20f * mk - cg) * moss * .7f; cb += (.07f * mk - cb) * moss * .7f;
        setCol(col, k, cr_ * crk, cg * crk, cb * crk); hg[k] = h;
    }
    makeMat(N, col, hg, 7.f, tWall, nWall);
}

// ѕриродна€ скала (свод и верх стен): валуны, тЄмные трещины, мох
static void genRockTex() {
    const int N = 512, C = 4; vector<float> col((size_t)N * N * 3), hg((size_t)N * N);
    for (int y = 0; y < N; y++) for (int x = 0; x < N; x++) {
        size_t k = (size_t)y * N + x; float uu = x / (float)N, vv = y / (float)N;
        float wu = uu + (vnoise(uu, vv, 6, 6, 201) - .5f) * .05f, wv = vv + (vnoise(uu, vv, 6, 6, 202) - .5f) * .05f;
        float u = wu * C, v = wv * C; int ix = (int)floorf(u), iy = (int)floorf(v);
        float f1 = 9, f2 = 9; int id = 0;
        for (int dj = -1; dj <= 1; dj++) for (int di = -1; di <= 1; di++) {
            int cx = ix + di, cy = iy + dj, wx = ((cx % C) + C) % C, wy = ((cy % C) + C) % C;
            float qx = cx + hrand(wx, wy, 101) * .7f + .15f, qy = cy + hrand(wx, wy, 102) * .7f + .15f;
            float d = sqrtf((u - qx) * (u - qx) + (v - qy) * (v - qy));
            if (d < f1) { f2 = f1; f1 = d; id = wx * 13 + wy; }
            else if (d < f2) f2 = d;
        }
        float e = f2 - f1, nz = fbm(uu, vv, 8, 4, 104), fine = vnoise(uu, vv, 128, 128, 5);
        float tint = .8f + .35f * hrand(id, 7, 5), warm = hrand(id, 3, 9) - .5f;
        float g0 = .48f * tint * (.7f + .5f * nz) * (.9f + .2f * fine);
        float cr = g0 * (1 + .06f * warm), cg = g0, cb = g0 * (1 - .06f * warm);
        float h = (.35f + .65f * smooth01(cl01(e / .35f))) * (.7f + .3f * nz) + .05f * fine;
        float cw = .035f + .02f * vnoise(uu, vv, 40, 40, 105);
        if (e < cw) { cr = .03f; cg = .03f; cb = .035f; h = 0; }
        float hair = fabsf(vnoise(uu, vv, 12, 12, 106) * 2.f - 1.f);
        if (hair < .025f) { cr *= .55f; cg *= .55f; cb *= .55f; h *= .6f; }
        float moss = cl01((fbm(uu, vv, 6, 4, 55) - .55f) * 3.5f) * (e < cw ? .4f : 1.f), k2 = .6f + .8f * fine;
        cr += (.12f * k2 - cr) * moss * .5f; cg += (.28f * k2 - cg) * moss * .5f; cb += (.10f * k2 - cb) * moss * .5f;
        setCol(col, k, cr, cg, cb); hg[k] = h;
    }
    makeMat(N, col, hg, 6.f, tRock, nRock);
}

// Ѕулыжна€ мостова€ (диаграмма ¬ороного): выпуклые камни, мокрые п€тна, мох в стыках
static void genFloorTex() {
    const int N = 512, C = 6; vector<float> col((size_t)N * N * 3), hg((size_t)N * N);
    for (int y = 0; y < N; y++) for (int x = 0; x < N; x++) {
        size_t k = (size_t)y * N + x;
        float u = (float)x / N * C, v = (float)y / N * C; int ix = (int)floorf(u), iy = (int)floorf(v);
        float f1 = 9, f2 = 9; int id = 0;
        for (int dj = -1; dj <= 1; dj++) for (int di = -1; di <= 1; di++) {
            int cx = ix + di, cy = iy + dj, wx = ((cx % C) + C) % C, wy = ((cy % C) + C) % C;
            float qx = cx + hrand(wx, wy, 1) * .8f + .1f, qy = cy + hrand(wx, wy, 2) * .8f + .1f;
            float d = sqrtf((u - qx) * (u - qx) + (v - qy) * (v - qy));
            if (d < f1) { f2 = f1; f1 = d; id = wx * 17 + wy; }
            else if (d < f2) f2 = d;
        }
        float e = f2 - f1, uu = x / (float)N, vv = y / (float)N;
        float tint = .75f + .4f * hrand(id, 5, 4), nz = .65f + .6f * fbm(uu, vv, 8, 4, 21), fine = vnoise(uu, vv, 128, 128, 23);
        float g = .40f * tint * nz * (.9f + .2f * fine);
        float r = g, gg = g, b = g * 1.03f;
        float wet = vnoise(uu, vv, 3, 3, 31);
        r *= 1 - .3f * wet; gg *= 1 - .25f * wet; b *= 1 - .18f * wet;
        float mw = .06f + .03f * vnoise(uu, vv, 40, 40, 8);
        float hs = smooth01(cl01((e - mw) / .3f));                              // скруглЄнный камень
        float h = hs * (.7f + .2f * nz + .06f * fine);
        float moss = cl01((fbm(uu, vv, 6, 3, 66) - .52f) * 3.5f) * (.2f + .8f * (1 - hs));
        r += (.12f - r) * moss * .6f; gg += (.26f - gg) * moss * .6f; b += (.09f - b) * moss * .6f;
        float sh = .55f + .45f * hs;                                           // стыки темнее
        if (e < mw) { r = .06f; gg = .058f; b = .055f; sh = 1; }
        setCol(col, k, r * sh, gg * sh, b * sh); hg[k] = h;
    }
    makeMat(N, col, hg, 5.f, tFloor, nFloor);
}

// ƒоски с прожилками
static void genWoodTex() {
    const int N = 256; vector<float> col((size_t)N * N * 3), hg((size_t)N * N);
    for (int y = 0; y < N; y++) for (int x = 0; x < N; x++) {
        size_t k = (size_t)y * N + x;
        float pw = N / 5.f; int pi = (int)(x / pw); float lx = x - pi * pw;
        float u = lx / pw, v = y / (float)N;
        float g1 = vnoise(u, v, 2, 16, pi * 7 + 1), g2 = vnoise(u, v, 4, 40, pi * 7 + 2);
        float ring = .5f + .5f * sinf(lx * .45f + 7.f * g1);
        float kk = .55f + .35f * g1 + .25f * ring * g2;
        float r = .38f * kk, g = .23f * kk, b = .12f * kk, h = .6f + .2f * ring * g2;
        if (lx < 2.f || lx > pw - 2.f) { r = .04f; g = .03f; b = .02f; h = 0; }
        setCol(col, k, r, g, b); hg[k] = h;
    }
    makeMat(N, col, hg, 3.f, tWood, nWood);
}

// ѕлам€: капл€, снизу синевата€, в середине жЄлта€, по краю оранжева€ (RGBA, аддитивное смешивание)
static GLuint genFlameTex() {
    const int W = 64, H = 128; vector<unsigned char> px(W * H * 4);
    for (int y = 0; y < H; y++) for (int x = 0; x < W; x++) {
        float tt = y / (float)(H - 1), xx = (x + .5f) / W - .5f;
        float hw = .44f * powf(1 - tt, 1.15f) + .02f; if (tt < .12f) hw *= (.65f + .35f * (tt / .12f));
        float d = fabsf(xx) / hw, I = 1 - d * d; if (I < 0) I = 0;
        I = powf(I, 1.2f) * (1 - powf(tt, 2.5f));
        float core = powf(I, 2.2f), bl = max(0.f, 1 - tt / .22f) * I;
        float r = 1.f, g = .35f + .6f * core, b = .05f + .5f * core;
        r = r * (1 - bl * .7f) + bl * .35f; g = g * (1 - bl * .5f) + bl * .5f; b = b * (1 - bl) + bl * 1.f;
        unsigned char* p = &px[((size_t)y * W + x) * 4];
        p[0] = (unsigned char)(cl01(r) * 255); p[1] = (unsigned char)(cl01(g) * 255); p[2] = (unsigned char)(cl01(b) * 255); p[3] = (unsigned char)(cl01(I) * 255);
    }
    return makeTex(W, H, px, 4);
}
static GLuint genGlowTex() {   // м€гкий круг (ореол вокруг факелов, фонарей и ключей)
    const int N = 64; vector<unsigned char> px(N * N * 4);
    for (int y = 0; y < N; y++) for (int x = 0; x < N; x++) {
        float dx = (x + .5f) / N * 2 - 1, dy = (y + .5f) / N * 2 - 1, r = sqrtf(dx * dx + dy * dy);
        float a = powf(max(0.f, 1 - r), 2.f);
        unsigned char* p = &px[((size_t)y * N + x) * 4]; p[0] = p[1] = p[2] = 255; p[3] = (unsigned char)(a * 255);
    }
    return makeTex(N, N, px, 4);
}
static GLuint genParchTex() {  // пергамент дл€ мини-карты
    const int N = 128; vector<unsigned char> px(N * N * 3);
    for (int y = 0; y < N; y++) for (int x = 0; x < N; x++) {
        float u = x / (float)N, v = y / (float)N, n = .85f + .3f * fbm(u, v, 4, 4, 41);
        float e = min(min(x, y), min(N - 1 - x, N - 1 - y)) / (float)N, edge = .55f + .45f * smooth01(cl01(e / .12f));
        putPx(px, N, x, y, .88f * n * edge, .78f * n * edge, .58f * n * edge);
    }
    return makeTex(N, N, px, 3);
}
static void genTextures() {
    genWallTex(); genRockTex(); genFloorTex(); genWoodTex(); tFlame = genFlameTex(); tGlow = genGlowTex(); tParch = genParchTex();
    nFlat = makeTex(1, 1, vector<unsigned char>{ 128, 128, 255, 160 }, 4);
}

// =====================================================================
//  „ј—“№ 1б. Ўейдер: попиксельный свет, карты нормалей, трипланарна€ кладка/скала дл€ пещеры,
//  затенение углов, рассе€ние света в тумане, ACES и гамма. »спользует стандартные glLight/glFog/glMaterial.
//  useTex: 0 Ч без текстуры, 1 Ч обычные UV (предметы), 2 Ч трипланарно по мировым координатам (пещера)
// =====================================================================
static const char* VS_SRC = R"(
#version 120
varying vec3 vP, vN, vW, vNW; varying vec2 vT; varying vec4 vC;
void main() {
    vec4 p = gl_ModelViewMatrix * gl_Vertex;
    vP = p.xyz; vN = gl_NormalMatrix * gl_Normal; vW = gl_Vertex.xyz; vNW = gl_Normal;
    vT = gl_MultiTexCoord0.xy; vC = gl_Color;
    gl_Position = gl_ProjectionMatrix * p;
}
)";
static const char* FS_SRC = R"(
#version 120
uniform sampler2D tex, nrm, tex2, nrm2;
uniform int useTex, nL;
uniform vec3 upEye;
uniform float scat[8];
uniform float triS, rockH;
varying vec3 vP, vN, vW, vNW; varying vec2 vT; varying vec4 vC;
vec3 lin(vec3 c) { return pow(max(c, vec3(0.0)), vec3(2.2)); }
vec3 aces(vec3 x) { x *= 1.5; return clamp((x * (2.51 * x + 0.03)) / (x * (2.43 * x + 0.59) + 0.14), 0.0, 1.0); }
// трипланарна€ выборка: цвет, мирова€ нормаль с рельефом, высота
void tri(sampler2D A, sampler2D M, vec3 P, vec3 Nw, vec3 w, float s, out vec3 al, out vec3 n, out float h) {
    vec2 ux = P.zy * s, uy = P.xz * s, uz = P.xy * s;
    al = texture2D(A, ux).rgb * w.x + texture2D(A, uy).rgb * w.y + texture2D(A, uz).rgb * w.z;
    vec4 nx = texture2D(M, ux), ny = texture2D(M, uy), nz = texture2D(M, uz);
    h = nx.a * w.x + ny.a * w.y + nz.a * w.z;
    vec2 tx = nx.xy * 2.0 - 1.0, ty = ny.xy * 2.0 - 1.0, tz = nz.xy * 2.0 - 1.0;
    n = normalize(Nw + vec3(0.0, tx.y, tx.x) * w.x + vec3(ty.x, 0.0, ty.y) * w.y + vec3(tz.x, tz.y, 0.0) * w.z);
}
void main() {
    vec3 N = normalize(vN), V = normalize(-vP);
    vec3 alb = vC.rgb; float h = 0.6, ao = 1.0, vao = 1.0;
    if (useTex == 1) {
        alb *= texture2D(tex, vT).rgb;
        vec4 nm = texture2D(nrm, vT); vec3 tn = nm.xyz * 2.0 - 1.0; h = nm.a;
        vec3 dp1 = dFdx(vP), dp2 = dFdy(vP); vec2 du1 = dFdx(vT), du2 = dFdy(vT);
        vec3 a = cross(dp2, N), b = cross(N, dp1);
        vec3 T = a * du1.x + b * du2.x, B = a * du1.y + b * du2.y;
        float k = inversesqrt(max(max(dot(T, T), dot(B, B)), 1e-12));
        N = normalize(T * k * tn.x + B * k * tn.y + N * tn.z);
        ao = 0.35 + 0.65 * h;
    } else if (useTex == 2) {
        vec3 Nw = normalize(vNW); vec3 wt = pow(abs(Nw), vec3(6.0)); wt /= (wt.x + wt.y + wt.z);
        vec3 a1, n1; float h1; tri(tex, nrm, vW, Nw, wt, triS, a1, n1, h1);
        if (rockH < 50.0) {   // снизу кладка, выше Ч природна€ скала; граница по высотам обеих текстур
            vec3 a2, n2; float h2; tri(tex2, nrm2, vW, Nw, wt, 0.18, a2, n2, h2);
            float r = smoothstep(0.35, 0.65, clamp((vT.x - rockH) * 0.8 + (h2 - h1) * 1.5 + 0.5, 0.0, 1.0));
            a1 = mix(a1, a2, r); n1 = normalize(mix(n1, n2, r)); h1 = mix(h1, h2, r);
        }
        alb *= a1; N = normalize(gl_NormalMatrix * n1); h = h1; ao = 0.35 + 0.65 * h; vao = vT.y;
    }
    alb = lin(alb);
    vec3 col = alb * lin(gl_LightModel.ambient.rgb) * 3.0 * (0.6 + 0.4 * dot(N, upEye)) * ao * vao;
    float dist = length(vP); vec3 rd = vP / max(dist, 1e-4);
    vec3 spc = gl_FrontMaterial.specular.rgb * (0.3 + 0.9 * h); float sh = max(gl_FrontMaterial.shininess, 1.0);
    float dao = mix(1.0, ao, 0.5) * mix(1.0, vao, 0.5);
    vec3 sc = vec3(0.0);
    for (int i = 0; i < 8; i++) {
        if (i >= nL) break;
        vec3 Lp = gl_LightSource[i].position.xyz, L = Lp - vP; float d = length(L); L /= d;
        float w = clamp(1.0 - pow(d / 14.0, 4.0), 0.0, 1.0);
        float att = w * w / (gl_LightSource[i].constantAttenuation + gl_LightSource[i].quadraticAttenuation * d * d);
        vec3 lc = gl_LightSource[i].diffuse.rgb * att;
        float nl = max(dot(N, L), 0.0);
        float sp = pow(max(dot(N, normalize(L + V)), 0.0), sh) * nl;
        col += (alb * nl * dao + spc * sp) * lc;
        float tc = dot(Lp, rd), hh = max(length(Lp - rd * tc), 0.25);   // рассе€ние в воздухе
        sc += gl_LightSource[i].diffuse.rgb * scat[i] * (atan((dist - tc) / hh) + atan(tc / hh)) / hh;
    }
    float f = exp(-pow(gl_Fog.density * dist, 2.0));
    vec3 c = mix(lin(gl_Fog.color.rgb), aces(col), f) + sc * gl_Fog.density * 0.3;
    gl_FragColor = vec4(pow(clamp(c, 0.0, 1.0), vec3(1.0 / 2.2)), vC.a);
}
)";
static GLuint prog = 0, curProg = 0; static GLint uUseTex, uUp, uScat, uNL, uTriS, uRockH;
static GLuint compileSh(GLenum type, const char* src) {
    GLuint s = glCreateShader(type); glShaderSource(s, 1, &src, NULL); glCompileShader(s);
    GLint ok; glGetShaderiv(s, GL_COMPILE_STATUS, &ok);
    if (!ok) { char log[4096]; glGetShaderInfoLog(s, 4096, NULL, log); printf("Shader error:\n%s\n", log); }
    return s;
}
static void buildShader() {
    prog = glCreateProgram();
    glAttachShader(prog, compileSh(GL_VERTEX_SHADER, VS_SRC)); glAttachShader(prog, compileSh(GL_FRAGMENT_SHADER, FS_SRC));
    glLinkProgram(prog);
    GLint ok; glGetProgramiv(prog, GL_LINK_STATUS, &ok);
    if (!ok) { char log[4096]; glGetProgramInfoLog(prog, 4096, NULL, log); printf("Link error:\n%s\n", log); }
    glUseProgram(prog);
    glUniform1i(glGetUniformLocation(prog, "tex"), 0); glUniform1i(glGetUniformLocation(prog, "nrm"), 1);
    glUniform1i(glGetUniformLocation(prog, "tex2"), 2); glUniform1i(glGetUniformLocation(prog, "nrm2"), 3);
    uUseTex = glGetUniformLocation(prog, "useTex"); uUp = glGetUniformLocation(prog, "upEye");
    uScat = glGetUniformLocation(prog, "scat"); uNL = glGetUniformLocation(prog, "nL");
    uTriS = glGetUniformLocation(prog, "triS"); uRockH = glGetUniformLocation(prog, "rockH");
    glUseProgram(0);
}
static GLuint nrmOf(GLuint t) { return t == tWall ? nWall : t == tRock ? nRock : t == tFloor ? nFloor : t == tWood ? nWood : nFlat; }
static void texOn(GLuint t) {   // включить текстуру + еЄ карту нормалей (обычные UV)
    glEnable(GL_TEXTURE_2D); glBindTexture(GL_TEXTURE_2D, t);
    glActiveTexture(GL_TEXTURE1); glBindTexture(GL_TEXTURE_2D, nrmOf(t)); glActiveTexture(GL_TEXTURE0);
    if (curProg) glUniform1i(uUseTex, 1);
}
static void texTri(GLuint a, GLuint b, float s, float rh) {   // трипланарно: a Ч основной материал, b Ч скала выше rh метров над полом
    glEnable(GL_TEXTURE_2D);
    glActiveTexture(GL_TEXTURE1); glBindTexture(GL_TEXTURE_2D, nrmOf(a));
    glActiveTexture(GL_TEXTURE2); glBindTexture(GL_TEXTURE_2D, b);
    glActiveTexture(GL_TEXTURE3); glBindTexture(GL_TEXTURE_2D, nrmOf(b));
    glActiveTexture(GL_TEXTURE0); glBindTexture(GL_TEXTURE_2D, a);
    glUniform1i(uUseTex, 2); glUniform1f(uTriS, s); glUniform1f(uRockH, rh);
}
static void texOff() { glDisable(GL_TEXTURE_2D); if (curProg) glUniform1i(uUseTex, 0); }
static void useLit(bool on) {   // включить/выключить освещение (шейдер) Ч дл€ свет€щихс€ частей
    curProg = on ? prog : 0; glUseProgram(curProg);
    if (on) glEnable(GL_LIGHTING); else glDisable(GL_LIGHTING);
}

// =====================================================================
//  „ј—“№ 2. Ўрифт 5x7 (русские заглавные, цифры, знаки) Ч рисуетс€ квадратиками
// =====================================================================
static const uint8_t RU[32][7] = {
 {0x0E,0x11,0x11,0x1F,0x11,0x11,0x11},{0x1F,0x10,0x10,0x1E,0x11,0x11,0x1E},{0x1E,0x11,0x11,0x1E,0x11,0x11,0x1E},{0x1F,0x10,0x10,0x10,0x10,0x10,0x10},
 {0x07,0x09,0x09,0x09,0x09,0x1F,0x11},{0x1F,0x10,0x10,0x1E,0x10,0x10,0x1F},{0x15,0x15,0x0E,0x04,0x0E,0x15,0x15},{0x0E,0x11,0x01,0x06,0x01,0x11,0x0E},
 {0x11,0x11,0x13,0x15,0x19,0x11,0x11},{0x0A,0x11,0x13,0x15,0x19,0x11,0x11},{0x11,0x12,0x14,0x18,0x14,0x12,0x11},{0x07,0x09,0x09,0x09,0x09,0x09,0x11},
 {0x11,0x1B,0x15,0x15,0x11,0x11,0x11},{0x11,0x11,0x11,0x1F,0x11,0x11,0x11},{0x0E,0x11,0x11,0x11,0x11,0x11,0x0E},{0x1F,0x11,0x11,0x11,0x11,0x11,0x11},
 {0x1E,0x11,0x11,0x1E,0x10,0x10,0x10},{0x0E,0x11,0x10,0x10,0x10,0x11,0x0E},{0x1F,0x04,0x04,0x04,0x04,0x04,0x04},{0x11,0x11,0x11,0x0F,0x01,0x11,0x0E},
 {0x04,0x0E,0x15,0x15,0x15,0x0E,0x04},{0x11,0x11,0x0A,0x04,0x0A,0x11,0x11},{0x12,0x12,0x12,0x12,0x12,0x1F,0x01},{0x11,0x11,0x11,0x0F,0x01,0x01,0x01},
 {0x15,0x15,0x15,0x15,0x15,0x15,0x1F},{0x15,0x15,0x15,0x15,0x15,0x1F,0x01},{0x18,0x08,0x08,0x0E,0x09,0x09,0x0E},{0x11,0x11,0x11,0x19,0x15,0x15,0x19},
 {0x10,0x10,0x10,0x1E,0x11,0x11,0x1E},{0x0E,0x11,0x01,0x07,0x01,0x11,0x0E},{0x12,0x15,0x15,0x1D,0x15,0x15,0x12},{0x0F,0x11,0x11,0x0F,0x05,0x09,0x11} };
static const uint8_t DIG[10][7] = {
 {0x0E,0x11,0x13,0x15,0x19,0x11,0x0E},{0x04,0x0C,0x04,0x04,0x04,0x04,0x0E},{0x0E,0x11,0x01,0x02,0x04,0x08,0x1F},{0x1F,0x02,0x04,0x02,0x01,0x11,0x0E},
 {0x02,0x06,0x0A,0x12,0x1F,0x02,0x02},{0x1F,0x10,0x1E,0x01,0x01,0x11,0x0E},{0x06,0x08,0x10,0x1E,0x11,0x11,0x0E},{0x1F,0x01,0x02,0x04,0x08,0x08,0x08},
 {0x0E,0x11,0x11,0x0E,0x11,0x11,0x0E},{0x0E,0x11,0x11,0x0F,0x01,0x02,0x0C} };
static const uint8_t PDOT[7] = { 0,0,0,0,0,0x0C,0x0C }, PCOM[7] = { 0,0,0,0,0x0C,0x04,0x08 }, PCOL[7] = { 0,0x0C,0x0C,0,0x0C,0x0C,0 },
PEXC[7] = { 4,4,4,4,4,0,4 }, PDSH[7] = { 0,0,0,0x1F,0,0,0 }, PQST[7] = { 0x0E,0x11,0x01,0x02,0x04,0,0x04 },
PLBR[7] = { 0x0E,0x08,0x08,0x08,0x08,0x08,0x0E }, PRBR[7] = { 0x0E,0x02,0x02,0x02,0x02,0x02,0x0E }, PBLK[7] = { 0,0,0,0,0,0,0 };
static const uint8_t* glyphOf(char16_t c) {
    if (c >= 0x430 && c <= 0x44F) c -= 0x20;
    if (c == 0x451 || c == 0x401) c = 0x415;
    if (c >= 0x410 && c <= 0x42F) return RU[c - 0x410];
    if (c >= '0' && c <= '9') return DIG[c - '0'];
    if (c >= 'a' && c <= 'z') c -= 32;
    switch (c) {
    case 'A': return RU[0]; case 'B': return RU[2]; case 'C': return RU[17]; case 'E': return RU[5]; case 'H': return RU[13];
    case 'K': return RU[10]; case 'M': return RU[12]; case 'O': return RU[14]; case 'P': return RU[16]; case 'T': return RU[18]; case 'X': return RU[21];
    case '.': return PDOT; case ',': return PCOM; case ':': return PCOL; case '!': return PEXC; case '-': return PDSH; case '?': return PQST;
    case '[': return PLBR; case ']': return PRBR;
    }
    return PBLK;
}
static float textW(const char16_t* t, float s) { int n = 0; while (t[n]) n++; return n * 6 * s; }
static void drawText(float x, float y, float s, const char16_t* t, float r, float g, float b, float a = 1.f) {
    for (int pass = 0; pass < 2; pass++) {
        float ox = pass == 0 ? s * .8f : 0, oy = ox;
        if (pass == 0) glColor4f(0, 0, 0, a * .8f); else glColor4f(r, g, b, a);
        glBegin(GL_QUADS); float cx = x;
        for (const char16_t* p = t; *p; p++) {
            const uint8_t* gp = glyphOf(*p);
            for (int row = 0; row < 7; row++) for (int col = 0; col < 5; col++) if (gp[row] & (0x10 >> col)) {
                float X = cx + ox + col * s, Y = y + oy + row * s;
                glVertex2f(X, Y); glVertex2f(X + s, Y); glVertex2f(X + s, Y + s); glVertex2f(X, Y + s);
            }
            cx += 6 * s;
        }
        glEnd();
    }
}

// =====================================================================
//  „ј—“№ 3. ƒанные уровн€, “≈ћџ и генераци€ лабиринта (залы, двери, ключи, этажи, лестницы, мосты)
// =====================================================================
struct Door { int tx, tz; bool alongX; int type; int lever; float open; bool opening; float y; };
// type: 0 дерево (железный ключ), 1 цепи (латунный ключ), 2 рычаг, 3 заколочена, 4 ложна€ (не открываетс€)
struct Item { int type; int tx, tz; float x, y, z; int dx, dz; bool taken; float pull; int door; };
// type: 0 железный ключ, 1 латунный ключ, 2 рычаг на стене
// kind: 0 Ч факел на стене, 1 Ч жаровн€ на подставке в зале
struct Torch { float wx, wz, wy, ang; int nf; float fx[2], fz[2], fy; float lx, ly, lz; float ph; float vis = 0; int kind = 0; };
// украшени€ свода: 0 цепь, 1 цепь с фонарЄм, 2 сталактит
struct Deco { int type; float x, y, z, len, rad, ph; float vis = 0; };
struct Stair { int tx, tz, dx, dz; float h0, h1; };   // лестница вверх в направлении (dx,dz): от высоты h0 к h1
// большой зал (клетки x0..x1, z0..z1 включительно). ≈сли split>=0 Ч зал двухъ€русный:
// по линии split (вдоль оси axis) обрыв, на одной стороне пол выше, наверх ведЄт лестница stairT
struct Room { int x0, z0, x1, z1, axis, split, stairT, base, lv0, lv1; };
// призрак: плывЄт по лабиринту, замечает игрока и преследует
struct Mon { float x, z, y, yaw, ph, chaseT, vis; int tgt, prev; bool sees; };

static const int DX[4] = { 1,-1,0,0 }, DZ[4] = { 0,0,1,-1 };
static int G = 0, level = 0, exitT = 0;
static vector<char> wallG, seenT, onPath, bridgeAt;
static vector<int> doorAt, stairAt, cellLv, forceTiles, roomId;
static vector<Door> doors; static vector<Item> items; static vector<Torch> torches; static vector<Deco> decos; static vector<Stair> stairs;
static vector<Room> rooms; static vector<V3> cols; static vector<Mon> mons;
static float portX = 0, portZ = 0, portY = 0, portAng = 0;
static bool isWall(int i, int j) { return i < 0 || j < 0 || i >= G || j >= G || wallG[j * G + i]; }

// ---- “≈ћџ ”–ќ¬Ќ≈… (мен€ютс€ по кругу) ----
//  wall/flr Ч оттенок камн€ и пола; fog Ч туман; amb Ч фоновый (холодный) свет;
//  torch Ч свет факелов (линейна€ €ркость); pl Ч слабый свет Ђу игрокаї; lamp Ч свет фонарей; dens Ч плотность тумана
struct Theme { const char16_t* name; float wall[3], flr[3], fog[3], amb[3], torch[3], pl[3], lamp[3]; float dens; };
static const Theme THEMES[4] = {
    { u"—»Ќяя ѕ≈ў≈–ј",     {.74f,.82f,1.f},   {.76f,.84f,1.f},   {.012f,.035f,.090f}, {.12f,.24f,.50f}, {2.6f,1.25f,.42f}, {.04f,.07f,.15f}, {.45f,.75f,1.f},  .026f },
    { u"«≈Ћ®Ќџ≈  ј“ј ќћЅџ",{.74f,.95f,.74f},  {.76f,.95f,.74f},  {.010f,.030f,.012f}, {.09f,.24f,.11f}, {2.4f,1.3f,.48f},  {.04f,.08f,.05f}, {.60f,1.f,.50f},  .029f },
    { u"“®ѕЋџ≈ –”»Ќџ",     {1.f,.90f,.78f},   {1.f,.90f,.78f},   {.020f,.012f,.008f}, {.22f,.15f,.10f}, {2.4f,1.15f,.40f}, {.09f,.06f,.04f}, {1.f,.75f,.40f},  .029f },
    { u" –ј—Ќџ… — Ћ≈ѕ",    {1.f,.66f,.60f},   {.95f,.70f,.64f},  {.060f,.008f,.008f}, {.28f,.07f,.05f}, {2.6f,.95f,.30f},  {.10f,.03f,.02f}, {1.f,.35f,.20f},  .032f }
};
static const Theme* TH = &THEMES[0];
static void applyTheme() {
    TH = &THEMES[level % 4];
    GLfloat fc[4] = { TH->fog[0], TH->fog[1], TH->fog[2], 1 }, am[4] = { TH->amb[0], TH->amb[1], TH->amb[2], 1 };
    glFogfv(GL_FOG_COLOR, fc); glFogf(GL_FOG_DENSITY, TH->dens);
    glLightModelfv(GL_LIGHT_MODEL_AMBIENT, am);
}
static bool isCell(int t) { return t % G % 2 == 1 && t / G % 2 == 1; }
static bool isPassage(int t) { return (t % G + t / G) % 2 == 1; }
// обход в ширину по открытым клеткам; doorsBlock Ч закрытые двери непроходимы; blk Ч ещЄ одна запрещЄнна€ клетка
static void bfsFrom(int src, vector<int>& d, vector<int>* par, bool doorsBlock, int blk = -1) {
    d.assign(G * G, -1); if (par) par->assign(G * G, -1);
    vector<int> q; q.push_back(src); d[src] = 0;
    for (size_t i = 0; i < q.size(); i++) {
        int t = q[i], x = t % G, z = t / G;
        for (int k = 0; k < 4; k++) {
            int X = x + DX[k], Z = z + DZ[k]; if (X < 0 || Z < 0 || X >= G || Z >= G) continue;
            int u = Z * G + X; if (d[u] >= 0 || wallG[u] || u == blk) continue;
            if (doorsBlock && doorAt[u] >= 0 && doors[doorAt[u]].open < .7f) continue;
            d[u] = d[t] + 1; if (par) (*par)[u] = t; q.push_back(u);
        }
    }
}

static void genLevel() {
    int n = min(7 + level * 2, 15); G = 2 * n + 1;
    wallG.assign(G * G, 1); seenT.assign(G * G, 0); onPath.assign(G * G, 0); doorAt.assign(G * G, -1);
    stairAt.assign(G * G, -1); bridgeAt.assign(G * G, 0); cellLv.assign(G * G, 0); roomId.assign(G * G, -1);
    doors.clear(); items.clear(); torches.clear(); stairs.clear(); forceTiles.clear(); rooms.clear();
    // 0) большие залы: пр€моугольники из нескольких клеток, между залами минимум одна клетка
    int maxR = 2 + n / 4, rmax = min(3, n / 3);
    for (int a = 0; a < 80 && (int)rooms.size() < maxR; a++) {
        int w = 2 + rndi(rmax), h = 2 + rndi(rmax), cx = rndi(n - w + 1), cz = rndi(n - h + 1);
        Room R; R.x0 = 2 * cx + 1; R.z0 = 2 * cz + 1; R.x1 = 2 * (cx + w - 1) + 1; R.z1 = 2 * (cz + h - 1) + 1;
        bool ok = true;
        for (auto& Q : rooms) if (R.x0 <= Q.x1 + 2 && Q.x0 <= R.x1 + 2 && R.z0 <= Q.z1 + 2 && Q.z0 <= R.z1 + 2) ok = false;
        if (!ok) continue;
        R.axis = 0; R.split = -1; R.stairT = -1; R.base = -1; R.lv0 = R.lv1 = 0;
        if ((w >= 3 || h >= 3) && rndf() < .8f) {            // двухъ€русный зал
            R.axis = (w > h) ? 0 : (h > w ? 1 : rndi(2));
            int span = R.axis == 0 ? w : h, other = R.axis == 0 ? h : w, s = 1 + rndi(span - 1), k = rndi(other);
            if (R.axis == 0) { R.split = R.x0 + 2 * s - 1; R.stairT = (R.z0 + 2 * k) * G + R.split; }
            else { R.split = R.z0 + 2 * s - 1; R.stairT = R.split * G + R.x0 + 2 * k; }
        }
        for (int z = R.z0; z <= R.z1; z++) for (int x = R.x0; x <= R.x1; x++) { roomId[z * G + x] = (int)rooms.size(); wallG[z * G + x] = 0; }
        rooms.push_back(R);
    }
    // 1) лабиринт-дерево; каждый зал Ч один узел дерева (в зал ведЄт ровно один Ђвходнойї проход)
    vector<char> vis(G * G, 0); vector<int> st;
    auto enter = [&](int t) {
        int r = roomId[t];
        if (r < 0) { vis[t] = 1; st.push_back(t); return; }
        vector<int> cs; const Room& R = rooms[r];
        for (int z = R.z0; z <= R.z1; z += 2) for (int x = R.x0; x <= R.x1; x += 2) { vis[z * G + x] = 1; cs.push_back(z * G + x); }
        shuffle(cs.begin(), cs.end(), rng); for (int c : cs) st.push_back(c);
        };
    wallG[G + 1] = 0; enter(G + 1);
    while (!st.empty()) {
        int t = st.back(), x = t % G, y = t / G, c[4], nc = 0;
        for (int k = 0; k < 4; k++) { int X = x + DX[k] * 2, Y = y + DZ[k] * 2; if (X > 0 && Y > 0 && X < G - 1 && Y < G - 1 && !vis[Y * G + X]) c[nc++] = k; }
        if (!nc) { st.pop_back(); continue; }
        int k = c[rndi(nc)], u = (y + DZ[k] * 2) * G + x + DX[k] * 2;
        wallG[(y + DZ[k]) * G + x + DX[k]] = 0; wallG[u] = 0; enter(u);
    }
    // 2) рассто€ни€ от старта, выход Ч сама€ дальн€€ клетка вне залов
    vector<int> dist, par; bfsFrom(G + 1, dist, &par, false);
    int ex = -1; for (int t = 0; t < G * G; t++) if (isCell(t) && roomId[t] < 0 && dist[t] > 0 && (ex < 0 || dist[t] > dist[ex])) ex = t;
    if (ex < 0) ex = G + 1;
    exitT = ex;
    vector<int> path; for (int t = ex; t != -1; t = par[t]) path.push_back(t);
    reverse(path.begin(), path.end());
    for (int t : path) onPath[t] = 1;
    // 3) двери на главном пути Ч только в коридорах между узлами (их нельз€ обойти); последн€€ охран€ет выход
    vector<int> dcand; for (int i = 1; i + 1 < (int)path.size(); i++) if (roomId[path[i]] < 0 && isPassage(path[i])) dcand.push_back(i);
    int m = min(min(1 + level, 3), (int)dcand.size()); vector<int> dp;
    for (int j = 0; j < m; j++) { int idx = dcand[(int)((dcand.size() - 1) * (j + 1) / (float)m)]; if (!dp.empty() && idx <= dp.back()) continue; dp.push_back(idx); }
    m = (int)dp.size();
    int typeList[3] = { 0, 2, 1 }; if (m == 2) typeList[1] = 1;
    for (int j = 0; j < m; j++) {
        int t = path[dp[j]]; Door d; d.tx = t % G; d.tz = t / G; d.alongX = (d.tx % 2 == 0);
        d.type = (m == 1) ? 0 : (j == m - 1 ? 1 : typeList[j]); d.lever = -1; d.open = 0; d.opening = false; d.y = 0;
        doorAt[t] = (int)doors.size(); doors.push_back(d);
    }
    // 4) ключи/рычаги: всегда в зоне, достижимой ƒќ своей двери
    for (int j = 0; j < m; j++) {
        vector<int> d2; bfsFrom(G + 1, d2, nullptr, false, path[dp[j]]);
        vector<int> cand, cand2;
        for (int t = 0; t < G * G; t++) {
            if (!isCell(t) || d2[t] < 0 || t == G + 1 || t == ex) continue;
            int open = 0; for (int k = 0; k < 4; k++) if (!wallG[(t / G + DZ[k]) * G + t % G + DX[k]]) open++;
            bool taken = false; for (auto& it : items) if (it.tz * G + it.tx == t) taken = true;
            if (taken) continue;
            cand2.push_back(t); if (open == 1 && !onPath[t]) cand.push_back(t);
        }
        vector<int>& use = cand.empty() ? cand2 : cand;
        int t = use[rndi((int)use.size())];
        Item it; it.type = doors[j].type == 0 ? 0 : (doors[j].type == 1 ? 1 : 2); it.tx = t % G; it.tz = t / G;
        it.x = (it.tx + .5f) * TILE; it.z = (it.tz + .5f) * TILE; it.y = 0; it.dx = it.dz = 0; it.taken = false; it.pull = 0; it.door = j;
        if (it.type == 2) doors[j].lever = (int)items.size();
        items.push_back(it);
    }
    // 5) ложные и заколоченные двери Ч только в ветках, где нет ничего нужного
    vector<char> sub(G * G, 0);
    for (auto& it : items) for (int t = it.tz * G + it.tx; t != -1; t = par[t]) sub[t] = 1;
    vector<int> cd;
    for (int t = 0; t < G * G; t++) if (!wallG[t] && isPassage(t) && roomId[t] < 0 && !onPath[t] && !sub[t] && dist[t] >= 5 && doorAt[t] < 0) cd.push_back(t);
    shuffle(cd.begin(), cd.end(), rng);
    int nd = min((int)cd.size(), 2 + min(level, 2));
    for (int i = 0; i < nd; i++) {
        int t = cd[i]; Door d; d.tx = t % G; d.tz = t / G; d.alongX = (d.tx % 2 == 0); d.type = (i % 2 == 0) ? 3 : 4; d.lever = -1; d.open = 0; d.opening = false; d.y = 0;
        doorAt[t] = (int)doors.size(); doors.push_back(d);
    }
    // 5б) этажи: идЄм от старта в пор€дке обхода. ¬ коридорах Ч лестницы (+-1 этаж) и мосты над провалом,
    //     в двухъ€русных залах Ч обрыв по линии split и лестница наверх
    {
        vector<int> order; for (int t = 0; t < G * G; t++) if (dist[t] >= 0) order.push_back(t);
        sort(order.begin(), order.end(), [&](int a, int b) { return dist[a] < dist[b]; });
        vector<int> nextLv(G * G, -1);
        auto pickDelta = [&](int lq) { return lq == 0 ? 1 : (lq == 3 ? -1 : (rndf() < .5f ? 1 : -1)); };
        for (int u : order) {
            int p = par[u], r = roomId[u];
            int inherit = p < 0 ? 0 : (nextLv[u] >= 0 ? nextLv[u] : cellLv[p]);
            if (r >= 0) {
                Room& R = rooms[r];
                auto side = [&](int t) { int c = R.axis == 0 ? t % G : t / G; return R.split < 0 ? 0 : (c < R.split ? 0 : (c > R.split ? 1 : 2)); };
                if (R.base < 0) {                       // перва€ клетка зала Ч вход: от неЄ уровень зала
                    R.base = inherit; R.lv0 = R.lv1 = R.base;
                    if (R.split >= 0) {
                        int far = R.base + pickDelta(R.base);
                        if (side(u) == 0) R.lv1 = far; else R.lv0 = far;
                        Stair S; S.tx = R.stairT % G; S.tz = R.stairT / G;
                        int dir = R.lv1 > R.lv0 ? 1 : -1;
                        S.dx = R.axis == 0 ? dir : 0; S.dz = R.axis == 1 ? dir : 0;
                        S.h0 = min(R.lv0, R.lv1) * LV; S.h1 = max(R.lv0, R.lv1) * LV;
                        stairAt[R.stairT] = (int)stairs.size(); stairs.push_back(S);
                    }
                }
                int sd = side(u);
                cellLv[u] = sd == 0 ? R.lv0 : (sd == 1 ? R.lv1 : (u == R.stairT ? min(R.lv0, R.lv1) : max(R.lv0, R.lv1)));
                continue;
            }
            cellLv[u] = inherit;
            if (p < 0 || !isPassage(u) || doorAt[u] >= 0) continue;
            int c = u + (u - p);                         // клетка за проходом
            if (c < 0 || c >= G * G) continue;
            if (rndf() < .32f) {
                int lq = cellLv[p], lr = lq + pickDelta(lq); nextLv[c] = lr;
                Stair S; S.tx = u % G; S.tz = u / G;
                int ddx = (c % G - p % G) / 2, ddz = (c / G - p / G) / 2;
                if (lr > lq) { S.dx = ddx; S.dz = ddz; S.h0 = lq * LV; S.h1 = lr * LV; }
                else { S.dx = -ddx; S.dz = -ddz; S.h0 = lr * LV; S.h1 = lq * LV; }
                stairAt[u] = (int)stairs.size(); stairs.push_back(S);
            }
            else if (rndf() < .14f) bridgeAt[u] = 1;
        }
    }
    // 6) клетки, где об€зательно должен быть факел: старт, выход, по обе стороны от каждой двери
    forceTiles.push_back(G + 1); forceTiles.push_back(ex);
    for (int j = 0; j < m; j++) { forceTiles.push_back(path[dp[j] - 1]); forceTiles.push_back(path[dp[j] + 1]); }
}

// =====================================================================
//  „ј—“№ 4. ѕещера: карта высот пола/свода и сетка (стены Ч крутые скалистые склоны)
// =====================================================================
static int NXf = 0;
static vector<float> Fv, Cv, Tv, Bv, aoF, aoC;   // пол, свод, Ђплотность породыї, номинальный уровень пола, затенение углов пола/свода
struct Mesh { vector<float> P, N, T; };
static Mesh mFl, mWl;                  // пол и Ђскалаї (склоны + свод)
static vector<int> flS, flC, wlS, wlC;

static int cellIdx(float x, float z) {
    int i = (int)floorf(x / TILE), j = (int)floorf(z / TILE);
    i = max(0, min(G - 1, i)); j = max(0, min(G - 1, j)); return j * G + i;
}
static void buildField() {
    const int NX = G * RES + 1, NT = G * RES; NXf = NX; const size_t NN = (size_t)NX * NX;
    Fv.assign(NN, 0.f); Cv.assign(NN, 0.f); Tv.assign(NN, 1.f);
    // 1) маска породы по Ђтексел€мї; координаты слегка искажены, чтобы границы не были пр€мыми
    vector<float> tex((size_t)NT * NT, 1.f), rtex((size_t)NT * NT, 0.f);
    for (int b = 0; b < NT; b++) for (int a = 0; a < NT; a++) {
        float x = (a + .5f) * HS, z = (b + .5f) * HS;
        float xw = x + (vn2(x * .35f + 11, z * .35f, 21) - .5f) * .8f, zw = z + (vn2(x * .35f, z * .35f + 17, 22) - .5f) * .8f;
        int ci = (int)floorf(xw / TILE), cj = (int)floorf(zw / TILE); float v = 1.f;
        if (ci >= 0 && cj >= 0 && ci < G && cj < G) {
            int t = cj * G + ci;
            if (!wallG[t]) {
                v = 0.f; float pw = 99.f;
                if (doorAt[t] >= 0) pw = .9f; else if (stairAt[t] >= 0 && roomId[t] < 0) pw = 1.2f;   // у дверей и лестниц проход уже
                if (pw < 90.f) { float lx = xw - (ci + .5f) * TILE, lz = zw - (cj + .5f) * TILE, lat = (ci % 2 == 0) ? lz : lx; if (fabsf(lat) > pw) v = 1.f; }
            }
            if (roomId[t] >= 0) rtex[(size_t)b * NT + a] = 1.f;
        }
        tex[(size_t)b * NT + a] = v;
    }
    auto toGrid = [&](const vector<float>& src, float border, vector<float>& dst) {
        dst.assign(NN, 0.f);
        for (int j = 0; j < NX; j++) for (int i = 0; i < NX; i++) {
            float s = 0;
            for (int dj = -1; dj <= 0; dj++) for (int di = -1; di <= 0; di++) { int a = i + di, b = j + dj; s += (a < 0 || b < 0 || a >= NT || b >= NT) ? border : src[(size_t)b * NT + a]; }
            dst[(size_t)j * NX + i] = s * .25f;
        }
        };
    auto blur = [&](vector<float>& src, vector<float>& dst, float border) {
        for (int j = 0; j < NX; j++) for (int i = 0; i < NX; i++) {
            float s = 0;
            for (int dj = -1; dj <= 1; dj++) for (int di = -1; di <= 1; di++) { int a = i + di, b = j + dj; s += (a < 0 || b < 0 || a >= NX || b >= NX) ? border : src[(size_t)b * NX + a]; }
            dst[(size_t)j * NX + i] = s / 9.f;
        }
        };
    vector<float> M, RM, tmp(NN);
    toGrid(tex, 1.f, M); toGrid(rtex, 0.f, RM);
    for (int p = 0; p < 3; p++) { blur(M, tmp, 1.f); M.swap(tmp); }
    for (int p = 0; p < 4; p++) { blur(RM, tmp, 0.f); RM.swap(tmp); }
    // 2) Ђплотностьї с шумом у границ
    for (int j = 0; j < NX; j++) for (int i = 0; i < NX; i++) {
        float x = i * HS, z = j * HS, m = M[(size_t)j * NX + i], bw = 1.f - fabsf(2.f * m - 1.f);
        float w = m + bw * (fbm2(x * .2f, z * .2f, 31) - .5f) * 1.0f;
        Tv[(size_t)j * NX + i] = smooth01(cl01((w - .35f) / .40f));
    }
    // 3) номинальна€ высота этажа: известна в открытых клетках, в камне Ч заполн€ем от соседей
    auto nominal = [&](int t, float x, float z) -> float {
        int si = stairAt[t];
        if (si >= 0) {
            const Stair& S = stairs[si]; float cx = (t % G + .5f) * TILE, cz = (t / G + .5f) * TILE;
            float s = cl01(((x - cx) * S.dx + (z - cz) * S.dz) / TILE + .5f); return S.h0 + (S.h1 - S.h0) * s;
        }
        return cellLv[t] * LV;
        };
    vector<float> B(NN, 0.f); vector<char> kn(NN, 0);
    for (int j = 0; j < NX; j++) for (int i = 0; i < NX; i++) {
        float x = i * HS, z = j * HS, sum = 0; int cnt = 0;
        for (int dj = -1; dj <= 0; dj++) for (int di = -1; di <= 0; di++) {
            int a = i + di, b = j + dj; if (a < 0 || b < 0 || a >= NT || b >= NT) continue;
            int t = (b / RES) * G + (a / RES); if (wallG[t]) continue; sum += nominal(t, x, z); cnt++;
        }
        if (cnt) { B[(size_t)j * NX + i] = sum / cnt; kn[(size_t)j * NX + i] = 1; }
    }
    {
        vector<int> q; vector<char> unk(NN, 0);
        for (size_t k = 0; k < NN; k++) { if (kn[k]) q.push_back((int)k); else unk[k] = 1; }
        for (size_t h = 0; h < q.size(); h++) {
            int k = q[h], i = k % NX, j = k / NX;
            for (int d = 0; d < 4; d++) {
                int a = i + DX[d], b = j + DZ[d]; if (a < 0 || b < 0 || a >= NX || b >= NX) continue;
                size_t u = (size_t)b * NX + a; if (kn[u]) continue; kn[u] = 1; B[u] = B[k]; q.push_back((int)u);
            }
        }
        for (int pass = 0; pass < 6; pass++) {
            vector<float> B2 = B;
            for (int j = 0; j < NX; j++) for (int i = 0; i < NX; i++) {
                size_t k = (size_t)j * NX + i; if (!unk[k]) continue;
                float s = 0; int c = 0;
                for (int d = 0; d < 4; d++) { int a = i + DX[d], b = j + DZ[d]; if (a < 0 || b < 0 || a >= NX || b >= NX) continue; s += B[(size_t)b * NX + a]; c++; }
                if (c) B2[k] = s / c;
            }
            B.swap(B2);
        }
    }
    // 4) пол и свод (в залах свод выше на 3.5 м Ч видно далеко и высоко)
    for (int j = 0; j < NX; j++) for (int i = 0; i < NX; i++) {
        size_t k = (size_t)j * NX + i; float x = i * HS, z = j * HS, t = Tv[k];
        float Hc = 3.5f + 2.4f * vn2(x * .11f + 3, z * .11f, 41) + .9f * vn2(x * .35f, z * .35f + 5, 43) + 3.5f * RM[k];
        static const float pts[4][2] = { {-.02f,0},{.02f,0},{0,-.02f},{0,.02f} };
        for (auto& q : pts) { int c = cellIdx(x + q[0], z + q[1]); if (doorAt[c] >= 0) Hc = min(Hc, 2.8f); }   // низкий свод у двери
        int cc = cellIdx(x, z);
        float pit = 0.f;
        if (bridgeAt[cc]) {                                              // над мостом свод выше, под ним Ч провал
            Hc += 1.5f; int ci = cc % G, cj = cc / G;
            float s = (ci % 2 == 0) ? (x - ci * TILE) / TILE : (z - cj * TILE) / TILE; s = cl01(s);
            pit = 4.f * smooth01(cl01(s / .18f)) * smooth01(cl01((1.f - s) / .18f));
        }
        float Ch = Hc * (1.f - .25f * t), s = cl01(t / .72f), ss = s * s * (3.f - 2.f * s);
        Cv[k] = B[k] + Ch; Fv[k] = B[k] + Ch * ss - pit * (1.f - ss);
    }
    Bv = B;
    // 5) затенение углов: точка пола ниже соседей (у подножи€ стен) Ч темнее; свод выше соседей (ниши) Ч темнее
    aoF.assign(NN, 1.f); aoC.assign(NN, 1.f);
    const int RA = 3;
    for (int j = 0; j < NX; j++) for (int i = 0; i < NX; i++) {
        float sf = 0, sc = 0; int c = 0;
        for (int dj = -RA; dj <= RA; dj++) for (int di = -RA; di <= RA; di++) {
            int a = max(0, min(NX - 1, i + di)), b = max(0, min(NX - 1, j + dj));
            sf += Fv[(size_t)b * NX + a]; sc += Cv[(size_t)b * NX + a]; c++;
        }
        size_t k = (size_t)j * NX + i;
        aoF[k] = max(.25f, min(1.f, 1.f - .3f * (sf / c - Fv[k])));
        aoC[k] = max(.3f, min(1.f, 1.f - .3f * (Cv[k] - sc / c)));
    }
}

static V3 normF(int i, int j) {
    int i0 = max(0, i - 1), i1 = min(NXf - 1, i + 1), j0 = max(0, j - 1), j1 = min(NXf - 1, j + 1);
    float dx = (Fv[(size_t)j * NXf + i1] - Fv[(size_t)j * NXf + i0]) / ((i1 - i0) * HS), dz = (Fv[(size_t)j1 * NXf + i] - Fv[(size_t)j0 * NXf + i]) / ((j1 - j0) * HS);
    float l = sqrtf(dx * dx + 1 + dz * dz); return { -dx / l, 1 / l, -dz / l };
}
static V3 normC(int i, int j) {
    int i0 = max(0, i - 1), i1 = min(NXf - 1, i + 1), j0 = max(0, j - 1), j1 = min(NXf - 1, j + 1);
    float dx = (Cv[(size_t)j * NXf + i1] - Cv[(size_t)j * NXf + i0]) / ((i1 - i0) * HS), dz = (Cv[(size_t)j1 * NXf + i] - Cv[(size_t)j0 * NXf + i]) / ((j1 - j0) * HS);
    float l = sqrtf(dx * dx + 1 + dz * dz); return { dx / l, -1 / l, dz / l };
}
// в texcoord кладЄм: x Ч высота над уровнем пола (дл€ перехода кладка?скала), y Ч затенение угла
static void emitQuad(Mesh& m, const V3* p, const V3* n, const float* ha, const float* ao) {
    for (int c = 0; c < 4; c++) {
        m.P.push_back(p[c].x); m.P.push_back(p[c].y); m.P.push_back(p[c].z);
        m.N.push_back(n[c].x); m.N.push_back(n[c].y); m.N.push_back(n[c].z);
        m.T.push_back(ha[c]); m.T.push_back(ao[c]);
    }
}
static void buildMesh() {
    mFl.P.clear(); mFl.N.clear(); mFl.T.clear(); mWl.P.clear(); mWl.N.clear(); mWl.T.clear();
    flS.assign(G * G, 0); flC.assign(G * G, 0); wlS.assign(G * G, 0); wlC.assign(G * G, 0);
    const int NX = NXf;
    for (int tz = 0; tz < G; tz++) for (int tx = 0; tx < G; tx++) {
        int t = tz * G + tx; flS[t] = (int)mFl.P.size() / 3; wlS[t] = (int)mWl.P.size() / 3;
        for (int b = 0; b < RES; b++) for (int a = 0; a < RES; a++) {
            int i = tx * RES + a, j = tz * RES + b;
            const int ii[4] = { i,i + 1,i + 1,i }, jj[4] = { j,j,j + 1,j + 1 };
            float mc = 0; for (int c = 0; c < 4; c++) mc = max(mc, Cv[(size_t)jj[c] * NX + ii[c]] - Fv[(size_t)jj[c] * NX + ii[c]]);
            if (mc < .08f) continue;       // внутри сплошной породы ничего не рисуем
            V3 pf[4], nf[4], pc[4], nc[4], an = { 0,0,0 }; float hf[4], hc[4], af[4], ac[4];
            for (int c = 0; c < 4; c++) {
                size_t k = (size_t)jj[c] * NX + ii[c]; float x = ii[c] * HS, z = jj[c] * HS;
                pf[c] = { x, Fv[k], z }; nf[c] = normF(ii[c], jj[c]); pc[c] = { x, Cv[k], z }; nc[c] = normC(ii[c], jj[c]);
                hf[c] = Fv[k] - Bv[k]; hc[c] = Cv[k] - Bv[k]; af[c] = aoF[k]; ac[c] = aoC[k];
                an = an + nf[c];
            }
            float al = sqrtf(an.x * an.x + an.y * an.y + an.z * an.z); an = an * (1.f / al);
            emitQuad(an.y >= .72f ? mFl : mWl, pf, nf, hf, af);
            emitQuad(mWl, pc, nc, hc, ac);
        }
        flC[t] = (int)mFl.P.size() / 3 - flS[t]; wlC[t] = (int)mWl.P.size() / 3 - wlS[t];
    }
}

// ---- высоты: пол, свод, лестницы, мосты ----
static float bil(const vector<float>& A, float x, float z) {
    float fx = max(0.f, min((float)(NXf - 1) - .001f, x / HS)), fz = max(0.f, min((float)(NXf - 1) - .001f, z / HS));
    int i = (int)fx, j = (int)fz; float tx = fx - i, tz = fz - j;
    float a = A[(size_t)j * NXf + i], b = A[(size_t)j * NXf + i + 1], c = A[(size_t)(j + 1) * NXf + i], d = A[(size_t)(j + 1) * NXf + i + 1];
    return a + (b - a) * tx + (c - a) * tz + (a - b - c + d) * tx * tz;
}
static float floorAt(float x, float z) { return bil(Fv, x, z); }
static float ceilAt(float x, float z) { return bil(Cv, x, z); }
static const float NOGROUND = -1e9f;
static float stairTop(float x, float z) {
    int i = (int)floorf(x / TILE), j = (int)floorf(z / TILE); if (i < 0 || j < 0 || i >= G || j >= G) return NOGROUND;
    int sid = stairAt[j * G + i]; if (sid < 0) return NOGROUND;
    const Stair& S = stairs[sid]; float cx = (i + .5f) * TILE, cz = (j + .5f) * TILE;
    float a = (x - cx) * S.dx + (z - cz) * S.dz, lat = S.dx != 0 ? (z - cz) : (x - cx);
    if (fabsf(lat) > 1.6f) return NOGROUND;
    int k = (int)floorf(cl01(a / TILE + .5f) * STEPS); if (k >= STEPS) k = STEPS - 1;
    return S.h0 + (k + 1) * (S.h1 - S.h0) / STEPS;
}
static float bridgeTop(float x, float z) {
    int i = (int)floorf(x / TILE), j = (int)floorf(z / TILE); if (i < 0 || j < 0 || i >= G || j >= G) return NOGROUND;
    int t = j * G + i; if (!bridgeAt[t]) return NOGROUND;
    float lat = (i % 2 == 0) ? (z - (j + .5f) * TILE) : (x - (i + .5f) * TILE);
    return fabsf(lat) < .9f ? cellLv[t] * LV : NOGROUND;
}
static float groundAt(float x, float z) { return max(floorAt(x, z), max(stairTop(x, z), bridgeTop(x, z))); }

// найти точку стены: идЄм из центра клетки в сторону k, пока пол не подниметс€ на target (м)
static bool findWall(float cx, float cz, int k, float target, float& wx, float& wz) {
    float base = floorAt(cx, cz);
    for (float d = .3f; d < TILE * 1.3f; d += .04f) {
        float x = cx + DX[k] * d, z = cz + DZ[k] * d;
        if (floorAt(x, z) - base >= target) { wx = x; wz = z; return true; }
    }
    return false;
}

// =====================================================================
//  „ј—“№ 5. ‘акелы, жаровни, колонны, предметы, украшени€ свода
// =====================================================================
static bool tryTorch(int tx, int tz, int k, bool force) {
    if (!isWall(tx + DX[k], tz + DZ[k])) return false;
    float cx = (tx + .5f) * TILE, cz = (tz + .5f) * TILE, wx, wz;
    if (!findWall(cx, cz, k, 1.3f, wx, wz)) return false;
    float wy = floorAt(cx, cz);
    if (!force) for (auto& t : torches) if (hypotf(t.wx - wx, t.wz - wz) < 3.6f && fabsf(t.wy - wy) < 2.f) return false;
    Torch t; t.wx = wx; t.wz = wz; t.wy = wy; t.ang = atan2f((float)-DX[k], (float)-DZ[k]); t.nf = rndf() < .5f ? 2 : 1; t.ph = rndf() * 20;
    float c = cosf(t.ang), s = sinf(t.ang);
    for (int i = 0; i < t.nf; i++) {
        float lx = t.nf == 1 ? 0.f : (i ? .2f : -.2f), lz = .41f;
        t.fx[i] = wx + c * lx + s * lz; t.fz[i] = wz - s * lx + c * lz;
    }
    t.fy = wy + 1.72f; t.lx = wx - DX[k] * .55f; t.ly = wy + 1.85f; t.lz = wz - DZ[k] * .55f;
    torches.push_back(t); return true;
}
static void placeTorches() {
    torches.clear();
    for (int t = 0; t < G * G; t++) {
        if (wallG[t] || doorAt[t] >= 0 || stairAt[t] >= 0 || bridgeAt[t]) continue; int tx = t % G, tz = t / G;
        for (int k = 0; k < 4; k++) if (rndf() < .16f) tryTorch(tx, tz, k, false);
    }
    for (int tile : forceTiles) { int k0 = rndi(4); for (int q = 0; q < 4; q++) if (tryTorch(tile % G, tile / G, (k0 + q) % 4, true)) break; }
}
// в залах: каменные колонны на Ђузлахї сетки и жаровни на подставках
static void placeRoomProps() {
    cols.clear();
    for (auto& R : rooms) {
        for (int z = R.z0 + 1; z < R.z1; z += 2) for (int x = R.x0 + 1; x < R.x1; x += 2) {
            if (R.split >= 0 && (R.axis == 0 ? x : z) == R.split) continue;      // не на краю обрыва
            if (rndf() < .7f) { float cx = (x + .5f) * TILE, cz = (z + .5f) * TILE; cols.push_back({ cx, floorAt(cx, cz), cz }); }
        }
        vector<int> cand;
        for (int z = R.z0; z <= R.z1; z += 2) for (int x = R.x0; x <= R.x1; x += 2) {
            int t = z * G + x; bool busy = (t == G + 1 || t == exitT);
            for (auto& it : items) if (it.tz * G + it.tx == t) busy = true;
            if (!busy) cand.push_back(t);
        }
        shuffle(cand.begin(), cand.end(), rng);
        for (size_t i = 0; i < cand.size(); i++) {
            if (i > 0 && rndf() > .3f) continue;
            int t = cand[i]; Torch T; T.kind = 1; T.nf = 1; T.ang = 0; T.ph = rndf() * 20;
            T.wx = (t % G + .5f + (rndf() - .5f) * .3f) * TILE; T.wz = (t / G + .5f + (rndf() - .5f) * .3f) * TILE; T.wy = floorAt(T.wx, T.wz);
            T.fx[0] = T.wx; T.fz[0] = T.wz; T.fy = T.wy + 1.22f; T.lx = T.wx; T.ly = T.wy + 1.7f; T.lz = T.wz;
            torches.push_back(T);
        }
    }
}
static void finalizeItems() {
    for (auto& D : doors) D.y = floorAt((D.tx + .5f) * TILE, (D.tz + .5f) * TILE);
    for (auto& it : items) {
        float cx = (it.tx + .5f) * TILE, cz = (it.tz + .5f) * TILE; it.y = floorAt(cx, cz);
        if (it.type == 2) {
            int k0 = rndi(4); bool ok = false; int kw = 0;
            for (int q = 0; q < 4 && !ok; q++) {
                int k = (k0 + q) % 4; if (!isWall(it.tx + DX[k], it.tz + DZ[k])) continue; kw = k;
                float wx, wz; if (findWall(cx, cz, k, 1.2f, wx, wz)) { it.x = wx; it.z = wz; it.dx = DX[k]; it.dz = DZ[k]; ok = true; }
            }
            if (!ok) { it.dx = DX[kw]; it.dz = DZ[kw]; it.x = cx + it.dx * 1.2f; it.z = cz + it.dz * 1.2f; }
        }
    }
}
static void genDecor() {
    decos.clear();
    for (int tz = 0; tz < G; tz++) for (int tx = 0; tx < G; tx++) {
        int t = tz * G + tx; if (wallG[t] || doorAt[t] >= 0 || stairAt[t] >= 0) continue;
        float x0 = tx * TILE, z0 = tz * TILE;
        int ns = 2 + rndi(4);                                    // сталактиты
        for (int i = 0; i < ns; i++) {
            float x = x0 + (.1f + .8f * rndf()) * TILE, z = z0 + (.1f + .8f * rndf()) * TILE;
            if (bil(Tv, x, z) > .35f) continue;
            float y = ceilAt(x, z), f = floorAt(x, z), len = min(.4f + 1.5f * rndf(), y - f - 2.3f);
            if (len < .3f) continue;
            decos.push_back({ 2, x, y + .03f, z, len, .07f + .14f * rndf(), rndf() * 6.f });
        }
        int nc = rndf() < .25f ? 1 + rndi(2) : 0;                // цепи (часть Ч толстые)
        for (int i = 0; i < nc; i++) {
            float x = x0 + (.15f + .7f * rndf()) * TILE, z = z0 + (.15f + .7f * rndf()) * TILE;
            if (bil(Tv, x, z) > .3f) continue;
            float y = ceilAt(x, z), f = floorAt(x, z), len = min(.8f + 1.8f * rndf(), y - f - .5f);
            if (len > .4f) decos.push_back({ 0, x, y + .03f, z, len, rndf() < .6f ? 1.8f : 0.f, rndf() * 6.f });
        }
        float lp = roomId[t] >= 0 ? .3f : .10f;
        if (tx % 2 == 1 && tz % 2 == 1 && !bridgeAt[t] && rndf() < lp) {   // фонарь на цепи в центре клетки
            float x = x0 + TILE * .5f, z = z0 + TILE * .5f; float y = ceilAt(x, z), f = floorAt(x, z), len = y - f - 2.4f - .36f;
            if (bil(Tv, x, z) < .2f && len > .3f) decos.push_back({ 1, x, y + .03f, z, len, 0.f, rndf() * 6.f });
        }
    }
    {   // у старта всегда есть фонарь
        float x = 1.5f * TILE, z = 1.5f * TILE, y = ceilAt(x, z), f = floorAt(x, z), len = max(.3f, y - f - 2.4f - .36f);
        bool has = false; for (auto& d : decos) if (d.type == 1 && hypotf(d.x - x, d.z - z) < .5f) has = true;
        if (!has) decos.push_back({ 1, x, y + .03f, z, len, 0.f, 0.f });
    }
    {   // свет€ща€с€ голуба€ арка-выход на торцевой стене тупика
        int ex = exitT % G, ez = exitT / G; bool done = false; float cx = (ex + .5f) * TILE, cz = (ez + .5f) * TILE;
        for (int pass = 0; pass < 2 && !done; pass++) for (int k = 0; k < 4 && !done; k++) {
            if (!isWall(ex + DX[k], ez + DZ[k])) continue;
            if (pass == 0 && isWall(ex - DX[k], ez - DZ[k])) continue;
            float wx, wz; if (!findWall(cx, cz, k, .35f, wx, wz)) continue;
            bool torchHere = false; for (auto& T : torches) if (hypotf(T.wx - wx, T.wz - wz) < 1.6f) torchHere = true;
            if (pass == 0 && torchHere) continue;
            portX = wx; portZ = wz; portY = floorAt(cx, cz); portAng = atan2f((float)-DX[k], (float)-DZ[k]); done = true;
        }
        if (!done) { portX = cx; portZ = cz; portY = floorAt(cx, cz); portAng = 0; }
    }
}

// =====================================================================
//  „ј—“№ 6. —толкновени€ и видимость
// =====================================================================
static bool solidAt(float x, float z) {
    int i = (int)floorf(x / TILE), j = (int)floorf(z / TILE);
    if (i < 0 || j < 0 || i >= G || j >= G) return true;
    int t = j * G + i, d = doorAt[t]; float cx = (i + .5f) * TILE, cz = (j + .5f) * TILE;
    if (d >= 0) {
        const Door& D = doors[d];
        float a = D.alongX ? x - cx : z - cz, b = D.alongX ? z - cz : x - cx;
        if (fabsf(a) < .16f && (D.open < .7f || fabsf(b) > .62f)) return true;
        if (fabsf(a) < .26f && fabsf(b) > .62f) return true;
    }
    if (bridgeAt[t]) { float lat = (i % 2 == 0) ? (z - cz) : (x - cx); if (fabsf(lat) > .78f) return true; }   // парапет моста
    for (auto& it : items) if (it.type < 2 && hypotf(x - it.x, z - it.z) < .32f) return true;                 // постамент
    for (auto& c : cols) if (fabsf(x - c.x) < .5f && fabsf(z - c.z) < .5f) return true;                       // колонна
    for (auto& T : torches) if (T.kind == 1 && hypotf(x - T.wx, z - T.wz) < .32f) return true;                // жаровн€
    return false;
}
static bool canStand(float x, float z, float ox, float oz, float curG) {
    static const float o[5][2] = { {0,0},{.22f,0},{-.22f,0},{0,.22f},{0,-.22f} };
    for (auto& q : o) {
        float X = x + q[0], Z = z + q[1];
        if (solidAt(X, Z)) return false;
        float g = groundAt(X, Z);
        if (ceilAt(X, Z) - g < 1.85f) return false;     // нет места дл€ головы
        if (g - curG > .26f) return false;               // слишком высокий уступ (спрыгнуть вниз можно)
    }
    float f0 = floorAt(ox, oz), f1 = floorAt(x, z), d = hypotf(x - ox, z - oz);
    if (d > 1e-5f && groundAt(x, z) <= f1 + .02f && (f1 - f0) / d > 1.15f) return false;   // слишком крутой склон
    return true;
}
static bool blocksLight(float x, float z) {
    int i = (int)floorf(x / TILE), j = (int)floorf(z / TILE); if (isWall(i, j)) return true;
    int d = doorAt[j * G + i]; return d >= 0 && doors[d].open < .7f;
}
static bool hasLOS(float x0, float z0, float x1, float z1) {
    float dx = x1 - x0, dz = z1 - z0, L = sqrtf(dx * dx + dz * dz); int n = (int)(L / .4f) + 1;
    for (int i = 1; i < n; i++) if (blocksLight(x0 + dx * i / n, z0 + dz * i / n)) return false;
    return true;
}

// =====================================================================
//  „ј—“№ 7. ѕримитивы (куб, цилиндр, сфера, тор) и модели: факел, жаровн€, ключ, рычаг, цепь, фонарь, арки, призрак
// =====================================================================
static void drawBox(float sx, float sy, float sz) {
    float x = sx / 2, y = sy / 2, z = sz / 2, a = sx * .5f, b = sy * .5f, c = sz * .5f;
    glBegin(GL_QUADS);
    glNormal3f(0, 0, 1);  glTexCoord2f(0, 0); glVertex3f(-x, -y, z); glTexCoord2f(a, 0); glVertex3f(x, -y, z); glTexCoord2f(a, b); glVertex3f(x, y, z); glTexCoord2f(0, b); glVertex3f(-x, y, z);
    glNormal3f(0, 0, -1); glTexCoord2f(0, 0); glVertex3f(x, -y, -z); glTexCoord2f(a, 0); glVertex3f(-x, -y, -z); glTexCoord2f(a, b); glVertex3f(-x, y, -z); glTexCoord2f(0, b); glVertex3f(x, y, -z);
    glNormal3f(1, 0, 0);  glTexCoord2f(0, 0); glVertex3f(x, -y, z); glTexCoord2f(c, 0); glVertex3f(x, -y, -z); glTexCoord2f(c, b); glVertex3f(x, y, -z); glTexCoord2f(0, b); glVertex3f(x, y, z);
    glNormal3f(-1, 0, 0); glTexCoord2f(0, 0); glVertex3f(-x, -y, -z); glTexCoord2f(c, 0); glVertex3f(-x, -y, z); glTexCoord2f(c, b); glVertex3f(-x, y, z); glTexCoord2f(0, b); glVertex3f(-x, y, -z);
    glNormal3f(0, 1, 0);  glTexCoord2f(0, 0); glVertex3f(-x, y, z); glTexCoord2f(a, 0); glVertex3f(x, y, z); glTexCoord2f(a, c); glVertex3f(x, y, -z); glTexCoord2f(0, c); glVertex3f(-x, y, -z);
    glNormal3f(0, -1, 0); glTexCoord2f(0, 0); glVertex3f(-x, -y, -z); glTexCoord2f(a, 0); glVertex3f(x, -y, -z); glTexCoord2f(a, c); glVertex3f(x, -y, z); glTexCoord2f(0, c); glVertex3f(-x, -y, z);
    glEnd();
}
static void drawCyl(float r, float len, int seg) {   // ось вдоль Y
    float h = len / 2;
    glBegin(GL_QUAD_STRIP);
    for (int i = 0; i <= seg; i++) { float a = 2 * PI * i / seg, c = cosf(a), s = sinf(a); glNormal3f(c, 0, s); glTexCoord2f(i / (float)seg, 0); glVertex3f(r * c, -h, r * s); glTexCoord2f(i / (float)seg, 1); glVertex3f(r * c, h, r * s); }
    glEnd();
    for (int cap = 0; cap < 2; cap++) {
        float yy = cap ? h : -h; glBegin(GL_TRIANGLE_FAN); glNormal3f(0, cap ? 1.f : -1.f, 0); glVertex3f(0, yy, 0);
        for (int i = 0; i <= seg; i++) { float a = 2 * PI * i / seg * (cap ? 1 : -1); glVertex3f(r * cosf(a), yy, r * sinf(a)); }
        glEnd();
    }
}
static void drawSphere(float r, int seg) {
    for (int i = 0; i < seg; i++) {
        float a0 = PI * i / seg - PI / 2, a1 = PI * (i + 1) / seg - PI / 2;
        glBegin(GL_QUAD_STRIP);
        for (int j = 0; j <= seg * 2; j++) {
            float b = 2 * PI * j / (seg * 2), cb = cosf(b), sb = sinf(b);
            glNormal3f(cosf(a0) * cb, sinf(a0), cosf(a0) * sb); glVertex3f(r * cosf(a0) * cb, r * sinf(a0), r * cosf(a0) * sb);
            glNormal3f(cosf(a1) * cb, sinf(a1), cosf(a1) * sb); glVertex3f(r * cosf(a1) * cb, r * sinf(a1), r * cosf(a1) * sb);
        }
        glEnd();
    }
}
static void drawTorus(float R, float r, int seg, int sides) {   // кольцо в плоскости XY, ось Z
    for (int i = 0; i < seg; i++) {
        float t0 = 2 * PI * i / seg, t1 = 2 * PI * (i + 1) / seg;
        glBegin(GL_QUAD_STRIP);
        for (int j = 0; j <= sides; j++) {
            float p = 2 * PI * j / sides, cp = cosf(p), sp = sinf(p);
            for (int e = 0; e < 2; e++) {
                float t = e ? t1 : t0, ct = cosf(t), st = sinf(t);
                glNormal3f(cp * ct, cp * st, sp); glVertex3f((R + r * cp) * ct, (R + r * cp) * st, r * sp);
            }
        }
        glEnd();
    }
}
static void setSpec(float s, float sh) { GLfloat m[4] = { s, s * .92f, s * .8f, 1 }; glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, m); glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, sh); }

static GLuint lTorch1, lTorch2, lKeyIron, lKeyBrass, lLink, lLink2, lCone;
static void torchModel(int nf) {   // локально: +Z Ч от стены в комнату
    texOff(); setSpec(.6f, 40); glColor3f(.17f, .16f, .16f);
    glPushMatrix(); glTranslatef(0, 1.3f, .015f); drawBox(.16f, .26f, .03f); glPopMatrix();
    glPushMatrix(); glTranslatef(0, 1.22f, .15f); drawBox(.045f, .045f, .3f); glPopMatrix();
    if (nf == 2) { glPushMatrix(); glTranslatef(0, 1.25f, .28f); drawBox(.5f, .035f, .035f); glPopMatrix(); }
    for (int i = 0; i < nf; i++) {
        float sx = nf == 1 ? 0.f : (i ? 1.f : -1.f);
        glPushMatrix(); glTranslatef(sx * .1f, 1.3f, .28f); glRotatef(18, 1, 0, 0); glRotatef(-sx * 14, 0, 0, 1);
        glColor3f(.30f, .20f, .10f); drawCyl(.028f, .75f, 8);
        glColor3f(.10f, .06f, .035f); glPushMatrix(); glTranslatef(0, .4f, 0); drawCyl(.052f, .17f, 8); glPopMatrix();
        glColor3f(.17f, .16f, .16f); glPushMatrix(); glTranslatef(0, .05f, 0); glRotatef(90, 1, 0, 0); drawTorus(.05f, .01f, 10, 5); glPopMatrix();
        glPopMatrix();
    }
}
static void keyModel(float r, float g, float b) {
    texOff(); setSpec(.9f, 70); glColor3f(r, g, b);
    glPushMatrix(); glTranslatef(-.13f, 0, 0); drawTorus(.06f, .018f, 14, 6); glPopMatrix();
    glPushMatrix(); glRotatef(90, 0, 0, 1); drawCyl(.014f, .3f, 8); glPopMatrix();
    glPushMatrix(); glTranslatef(.12f, -.045f, 0); drawBox(.03f, .09f, .02f); glPopMatrix();
    glPushMatrix(); glTranslatef(.07f, -.035f, 0); drawBox(.03f, .07f, .02f); glPopMatrix();
}
static void buildLists() {
    lTorch1 = glGenLists(7);
    glNewList(lTorch1, GL_COMPILE); torchModel(1); glEndList();
    lTorch2 = lTorch1 + 1; glNewList(lTorch2, GL_COMPILE); torchModel(2); glEndList();
    lKeyIron = lTorch1 + 2; glNewList(lKeyIron, GL_COMPILE); keyModel(.58f, .62f, .68f); glEndList();
    lKeyBrass = lTorch1 + 3; glNewList(lKeyBrass, GL_COMPILE); keyModel(.9f, .68f, .2f); glEndList();
    lLink = lTorch1 + 4; glNewList(lLink, GL_COMPILE); drawTorus(.045f, .012f, 10, 6); glEndList();
    lLink2 = lTorch1 + 5; glNewList(lLink2, GL_COMPILE); drawTorus(.07f, .02f, 10, 6); glEndList();   // звено потолочной цепи
    lCone = lTorch1 + 6; glNewList(lCone, GL_COMPILE);   // единичный конус: основание y=0 (радиус 1), остриЄ внизу y=-1
    {
        const int S = 12;
        glBegin(GL_TRIANGLES);
        for (int i = 0; i < S; i++) {
            float a0 = 2 * PI * i / S, a1 = 2 * PI * (i + 1) / S, am = (a0 + a1) / 2;
            glNormal3f(cosf(a0), -1, sinf(a0)); glVertex3f(cosf(a0), 0, sinf(a0));
            glNormal3f(cosf(a1), -1, sinf(a1)); glVertex3f(cosf(a1), 0, sinf(a1));
            glNormal3f(cosf(am), -1, sinf(am)); glVertex3f(0, -1, 0);
        }
        glEnd();
    }
    glEndList();
}
static void drawPedestal() {   // постамент из камн€ (текстура кладки)
    texOn(tWall); glColor3f(.85f, .85f, .85f); setSpec(.15f, 15);
    glPushMatrix(); glTranslatef(0, .06f, 0); drawBox(.54f, .12f, .54f); glPopMatrix();
    glPushMatrix(); glTranslatef(0, .5f, 0); drawBox(.36f, .76f, .36f); glPopMatrix();
    glPushMatrix(); glTranslatef(0, .93f, 0); drawBox(.52f, .1f, .52f); glPopMatrix();
}
static void drawBrazier() {   // железна€ жаровн€ на подставке с угл€ми
    texOff(); setSpec(.5f, 30); glColor3f(.12f, .11f, .10f);
    glPushMatrix(); glTranslatef(0, .04f, 0); drawCyl(.28f, .08f, 12); glPopMatrix();
    glPushMatrix(); glTranslatef(0, .6f, 0); drawCyl(.045f, 1.1f, 8); glPopMatrix();
    glPushMatrix(); glTranslatef(0, 1.22f, 0); glScalef(.34f, .24f, .34f); glCallList(lCone); glPopMatrix();
    glPushMatrix(); glTranslatef(0, 1.21f, 0); glRotatef(90, 1, 0, 0); drawTorus(.33f, .025f, 16, 6); glPopMatrix();
    useLit(false); glColor3f(1.f, .45f, .12f);
    glPushMatrix(); glTranslatef(0, 1.2f, 0); glScalef(1, .3f, 1); drawSphere(.27f, 8); glPopMatrix();
    useLit(true);
}
static void drawLever(float pull) {   // локально: +Z Ч от стены в комнату
    texOff(); setSpec(.6f, 40); glColor3f(.18f, .17f, .17f);
    glPushMatrix(); glTranslatef(0, 1.2f, .03f); drawBox(.26f, .36f, .06f); glPopMatrix();
    glColor3f(.08f, .08f, .08f); glPushMatrix(); glTranslatef(0, 1.2f, .065f); drawBox(.04f, .26f, .01f); glPopMatrix();
    glPushMatrix(); glTranslatef(0, 1.2f, .09f); glRotatef(40 - 80 * pull, 1, 0, 0);
    glColor3f(.5f, .5f, .54f); glPushMatrix(); glTranslatef(0, .16f, 0); drawCyl(.014f, .32f, 8); glPopMatrix();
    glColor3f(.6f, .12f, .08f); glPushMatrix(); glTranslatef(0, .33f, 0); drawSphere(.04f, 8); glPopMatrix();
    glPopMatrix();
}
static void drawGhost(float t, float ph) {   // призрак в рваном балахоне, +Z Ч вперЄд
    texOff(); setSpec(.05f, 6); glColor3f(.05f, .05f, .06f);
    glPushMatrix(); glScalef(.5f, -1.75f, .5f); glCallList(lCone); glPopMatrix();                                   // балахон
    glPushMatrix(); glRotatef(15.f + 10.f * sinf(t * 3 + ph), 0, 1, 0); glTranslatef(0, -.05f, 0); glScalef(.58f, -1.15f, .58f); glCallList(lCone); glPopMatrix();
    glColor3f(.07f, .07f, .08f);
    glPushMatrix(); glTranslatef(0, 1.62f, 0); glScalef(1, 1.15f, 1); drawSphere(.21f, 8); glPopMatrix();           // капюшон
    for (int s = -1; s <= 1; s += 2) {                                                                                // т€нущиес€ руки
        glPushMatrix(); glTranslatef(s * .22f, 1.28f, .15f); glRotatef(-65.f + 6.f * sinf(t * 4 + ph + s), 1, 0, 0); glTranslatef(0, .28f, 0); drawCyl(.045f, .56f, 6); glPopMatrix();
    }
    useLit(false);
    glColor3f(0, 0, 0); glPushMatrix(); glTranslatef(0, 1.6f, .14f); drawBox(.2f, .2f, .06f); glPopMatrix();          // тьма под капюшоном
    glColor3f(1.f, .35f, .1f);
    for (int s = -1; s <= 1; s += 2) { glPushMatrix(); glTranslatef(s * .055f, 1.63f, .18f); drawSphere(.024f, 6); glPopMatrix(); }
    useLit(true);
}

// ---- потолочна€ цепь (вниз от точки подвеса) и вис€чий фонарь ----
static void drawChain(float len, float th = 1.f) {   // th Ч толщина (1 Ч тонка€, ~1.8 Ч толста€)
    texOff(); setSpec(.5f, 30); glColor3f(.40f, .30f, .22f);
    float pitch = .13f * th; int n = max(1, (int)(len / pitch));
    for (int L = 0; L < n; L++) {
        glPushMatrix(); glTranslatef(0, -.1f * th - L * pitch, 0);
        if (L & 1) glRotatef(90, 0, 1, 0);
        glScalef(th, 1.5f * th, th); glCallList(lLink2); glPopMatrix();
    }
}
static void drawLantern() {   // локально: верх фонар€ в (0,0,0), корпус уходит вниз
    texOff(); setSpec(.6f, 40); glColor3f(.12f, .11f, .11f);
    glPushMatrix(); glTranslatef(0, -.02f, 0); drawBox(.24f, .04f, .24f); glPopMatrix();
    glPushMatrix(); glTranslatef(0, -.345f, 0); drawBox(.2f, .03f, .2f); glPopMatrix();
    for (int i = 0; i < 4; i++) {
        glPushMatrix(); glTranslatef((i & 1) ? .085f : -.085f, -.18f, (i & 2) ? .085f : -.085f); drawBox(.025f, .3f, .025f); glPopMatrix();
    }
    glPushMatrix(); glTranslatef(0, .045f, 0); drawTorus(.04f, .01f, 10, 5); glPopMatrix();
    useLit(false);
    glColor3f(TH->lamp[0], TH->lamp[1], TH->lamp[2]);
    glPushMatrix(); glTranslatef(0, -.18f, 0); drawBox(.15f, .28f, .15f); glPopMatrix();
    useLit(true);
}

// ---- арка-выход: каменна€ рама и свет€щеес€ голубое Ђокної (локально +Z Ч от стены в пещеру) ----
static void archFill(float hw, float hs, float z, const float* ci, const float* co) {
    glBegin(GL_TRIANGLE_FAN);
    glColor3f(ci[0], ci[1], ci[2]); glVertex3f(0, hs * .7f, z);
    glColor3f(co[0], co[1], co[2]);
    glVertex3f(-hw, 0, z); glVertex3f(hw, 0, z); glVertex3f(hw, hs, z);
    for (int i = 1; i < 12; i++) { float a = PI * i / 12; glVertex3f(hw * cosf(a), hs + hw * sinf(a), z); }
    glVertex3f(-hw, hs, z); glVertex3f(-hw, 0, z);
    glEnd();
}
static void archFrame(float hw, float hs, float th) {
    texOn(tWall); glColor3f(TH->wall[0] * .9f, TH->wall[1] * .9f, TH->wall[2] * .9f); setSpec(.08f, 10);
    for (int sx = -1; sx <= 1; sx += 2) { glPushMatrix(); glTranslatef(sx * (hw + th * .5f), hs * .5f, -.02f); drawBox(th, hs, .26f); glPopMatrix(); }
    const int S = 12; float R = hw + th * .5f;
    for (int i = 0; i < S; i++) {
        float am = PI * (i + .5f) / S;
        glPushMatrix(); glTranslatef(R * cosf(am), hs + R * sinf(am), -.02f); glRotatef(am * 180 / PI + 90, 0, 0, 1);
        drawBox(PI * R / S * 1.12f, th, .26f); glPopMatrix();
    }
    texOff();
}
static void drawPortal(float t) {
    texOn(tWall); glColor3f(TH->wall[0] * .8f, TH->wall[1] * .8f, TH->wall[2] * .8f); setSpec(.05f, 8);
    glPushMatrix(); glTranslatef(0, 1.6f, -.54f); drawBox(3.0f, 3.6f, 1.2f); glPopMatrix();   // каменный массив за аркой
    archFrame(.6f, 1.2f, .26f);
    texOff(); useLit(false);
    float p = .9f + .1f * sinf(t * 2.3f);
    const float a[3] = { .75f * p,.95f * p,1.f * p }, b[3] = { .15f,.45f,.95f * p };
    archFill(.6f, 1.2f, .075f, a, b);
    useLit(true);
}

// ---- двери (динамические, поворачиваютс€ на петле) с каменной рамой ----
static void drawDoor(const Door& D) {
    static const float tint[5][3] = { {1,.95f,.85f},{.8f,.78f,.75f},{.55f,.57f,.6f},{.72f,.62f,.5f},{1,.85f,.7f} };
    float cx = (D.tx + .5f) * TILE, cz = (D.tz + .5f) * TILE;
    glPushMatrix(); glTranslatef(cx, D.y, cz); if (!D.alongX) glRotatef(90, 0, 1, 0);
    {   // каменна€ рама: две колонны и перемычка (перекрывают неровные склоны вокруг проЄма)
        float Cd = max(2.4f, ceilAt(cx, cz) - D.y);
        texOn(tWall); glColor3f(TH->wall[0] * .85f, TH->wall[1] * .85f, TH->wall[2] * .85f); setSpec(.08f, 10);
        for (int sg = -1; sg <= 1; sg += 2) { glPushMatrix(); glTranslatef(0, Cd * .5f, sg * 1.1f); drawBox(.5f, Cd + .4f, .9f); glPopMatrix(); }
        float h = Cd + .2f - 2.2f;
        if (h > .05f) { glPushMatrix(); glTranslatef(0, 2.2f + h * .5f, 0); drawBox(.5f, h, 2.0f); glPopMatrix(); }
    }
    glTranslatef(0, 0, -.65f); glRotatef(-D.open * 105.f, 0, 1, 0);   // петл€ у кра€ проЄма
    texOn(tWood); glColor3f(tint[D.type][0], tint[D.type][1], tint[D.type][2]); setSpec(.12f, 12);
    const int NC = 26, NR = 3; const float top = 2.2f;
    for (int i = 0; i < NC; i++) {
        float s0 = 1.3f * i / NC, s1 = 1.3f * (i + 1) / NC;
        for (int sg = -1; sg <= 1; sg += 2) {
            glNormal3f((float)sg, 0, 0); glBegin(GL_QUADS);
            for (int r = 0; r < NR; r++) {
                float y0 = top * r / NR, y1 = top * (r + 1) / NR;
                glTexCoord2f(s0 / 1.3f, y0 / 2.1f); glVertex3f(sg * .05f, y0, s0); glTexCoord2f(s1 / 1.3f, y0 / 2.1f); glVertex3f(sg * .05f, y0, s1);
                glTexCoord2f(s1 / 1.3f, y1 / 2.1f); glVertex3f(sg * .05f, y1, s1); glTexCoord2f(s0 / 1.3f, y1 / 2.1f); glVertex3f(sg * .05f, y1, s0);
            }
            glEnd();
        }
        glNormal3f(0, 1, 0); glBegin(GL_QUADS); glVertex3f(-.05f, top, s0); glVertex3f(.05f, top, s0); glVertex3f(.05f, top, s1); glVertex3f(-.05f, top, s1); glEnd();
    }
    texOff();
    for (int sg = -1; sg <= 1; sg += 2) {
        float f = (float)sg;
        if (D.type == 2) {   // стальные пластины и заклЄпки
            setSpec(.7f, 40); glColor3f(.2f, .21f, .23f);
            for (int p = 0; p < 2; p++) {
                float y = p ? 1.55f : .55f; glPushMatrix(); glTranslatef(f * .058f, y, .65f); drawBox(.03f, .85f, 1.2f); glPopMatrix();
                glColor3f(.35f, .36f, .38f); for (int q = 0; q < 4; q++) { glPushMatrix(); glTranslatef(f * .078f, y + (q & 1 ? .36f : -.36f), .65f + (q & 2 ? .5f : -.5f)); drawSphere(.025f, 5); glPopMatrix(); } glColor3f(.2f, .21f, .23f);
            }
        }
        else {             // железные полосы
            setSpec(.6f, 40); if (D.type == 4) glColor3f(.8f, .6f, .2f); else glColor3f(.14f, .14f, .15f);
            for (int p = 0; p < 3; p++) { glPushMatrix(); glTranslatef(f * .058f, .45f + p * .7f, .65f); drawBox(.02f, .09f, 1.25f); glPopMatrix(); }
        }
        if (D.type == 0 || D.type == 1 || D.type == 4) {   // кольцо-ручка и замочна€ скважина
            glColor3f(.12f, .12f, .12f); glPushMatrix(); glTranslatef(f * .075f, 1.0f, 1.08f); glRotatef(90, 0, 1, 0); drawTorus(.07f, .012f, 12, 6); glPopMatrix();
            glColor3f(D.type == 4 ? .9f : .35f, D.type == 4 ? .7f : .3f, D.type == 4 ? .25f : .25f);
            glPushMatrix(); glTranslatef(f * .065f, 1.18f, 1.0f); drawBox(.015f, .17f, .1f); glPopMatrix();
            glColor3f(0, 0, 0); glPushMatrix(); glTranslatef(f * .075f, 1.18f, 1.0f); drawBox(.012f, .06f, .02f); glPopMatrix();
        }
        if (D.type == 1) {   // две цепи крест-накрест и замок
            setSpec(.7f, 50); glColor3f(.3f, .3f, .32f);
            for (int ch = 0; ch < 2; ch++) {
                float s0 = ch ? 1.25f : .05f, s1 = ch ? .05f : 1.25f, y0 = .25f, y1 = 1.95f, alpha = atan2f(y1 - y0, -(s1 - s0));
                for (int L = 0; L <= 9; L++) {
                    float u = L / 9.f;
                    glPushMatrix(); glTranslatef(f * .078f, y0 + (y1 - y0) * u, s0 + (s1 - s0) * u);
                    glRotatef(90, 0, 1, 0); glRotatef(alpha * 180 / PI, 0, 0, 1); if (L & 1) glRotatef(90, 1, 0, 0); glScalef(1.5f, 1, 1); glCallList(lLink); glPopMatrix();
                }
            }
            glColor3f(.8f, .6f, .2f); glPushMatrix(); glTranslatef(f * .09f, 1.05f, .65f); drawBox(.05f, .14f, .1f); glPopMatrix();
        }
        if (D.type == 3) {   // доски крест-накрест
            setSpec(.1f, 8); glColor3f(.26f, .18f, .1f);
            for (int p = 0; p < 3; p++) { glPushMatrix(); glTranslatef(f * .08f, .5f + p * .55f, .65f); glRotatef((p & 1 ? 24 : -24) * f, 1, 0, 0); drawBox(.045f, .17f, 1.55f); glPopMatrix(); }
        }
    }
    glPopMatrix();
}

// =====================================================================
//  „ј—“№ 8. —осто€ние игрока и освещение
// =====================================================================
static float px, pz, pg = 0, camYaw, camPitch, bobPh = 0, eyeY = 1.6f, stamina = 100;
static bool haveKey[2] = { false, false }, fullMap = false, mouseLocked = false, wantUse = false, wantRestart = false;
static bool dead = false; static float deadT = 0;
static const char16_t* msg = u""; static float msgT = 0;
static void say(const char16_t* s) { msg = s; msgT = 4.f; }
static float gTime = 0;

// »сточники света: ближайшие факелы, жаровни, фонари и глаза призраков (до 7) + слабый свет у игрока.
// vis Ч плавна€ видимость источника (дл€ объЄмного ореола в тумане, чтобы он не просвечивал сквозь скалу)
struct LCand { float x, y, z, r, g, b, ca, qa, score; float* vis; };
static void setupLights(float t) {
    static float lastT = -1.f; float dt = lastT < 0 ? 0.f : min(.1f, t - lastT); lastT = t;
    float fade = min(1.f, dt * 5.f);
    vector<LCand> c;
    auto add = [&](float x, float y, float z, float r, float g, float b, float ca, float qa, float bonus, float* vis) {
        float dy = y - eyeY, d = sqrtf((x - px) * (x - px) + (z - pz) * (z - pz) + dy * dy); if (d > 19) return;
        bool los = hasLOS(px, pz, x, z); *vis += ((los ? 1.f : 0.f) - *vis) * fade;
        c.push_back({ x, y, z, r, g, b, ca, qa, d + bonus + (los ? 0.f : 8.f), vis });
        };
    for (auto& T : torches) {
        float fl = 1 + .14f * sinf(t * 11 + T.ph) + .1f * sinf(t * 23 + T.ph * 2.3f) + .06f * sinf(t * 47 + T.ph * 3.1f);
        float k = (T.kind == 1 ? 1.6f : (T.nf == 2 ? 1.35f : 1.f)) * fl;
        add(T.lx, T.ly, T.lz, TH->torch[0] * k, TH->torch[1] * k, TH->torch[2] * k, .6f, .35f, 0.f, &T.vis);
    }
    for (auto& D : decos) if (D.type == 1) {
        float k = 1.3f * (.92f + .08f * sinf(t * 5 + D.ph));
        add(D.x, D.y - D.len - .18f, D.z, TH->lamp[0] * k, TH->lamp[1] * k, TH->lamp[2] * k, .8f, .6f, 1.f, &D.vis);
    }
    for (auto& m : mons) add(m.x, m.y + 1.6f, m.z, .7f, .12f, .05f, 1.f, 1.5f, -2.f, &m.vis);
    sort(c.begin(), c.end(), [](const LCand& a, const LCand& b) { return a.score < b.score; });
    int n = min((int)c.size(), 7); float sc[8] = { 0 };
    GLfloat zero[4] = { 0,0,0,1 };
    for (int i = 0; i <= n; i++) {
        GLenum L = GL_LIGHT0 + i;
        bool pl = (i == n);   // последний Ч свет у игрока
        GLfloat pos[4] = { pl ? px : c[i].x, pl ? eyeY : c[i].y, pl ? pz : c[i].z, 1 };
        GLfloat dif[4] = { pl ? TH->pl[0] : c[i].r, pl ? TH->pl[1] : c[i].g, pl ? TH->pl[2] : c[i].b, 1 };
        glLightfv(L, GL_POSITION, pos); glLightfv(L, GL_DIFFUSE, dif); glLightfv(L, GL_SPECULAR, dif); glLightfv(L, GL_AMBIENT, zero);
        glLightf(L, GL_CONSTANT_ATTENUATION, pl ? 1.f : c[i].ca); glLightf(L, GL_LINEAR_ATTENUATION, 0.f); glLightf(L, GL_QUADRATIC_ATTENUATION, pl ? .25f : c[i].qa);
        sc[i] = pl ? 0.f : *c[i].vis;
    }
    useLit(true);
    glUniform1i(uNL, n + 1); glUniform1fv(uScat, 8, sc); glUniform3f(uUp, 0, cosf(camPitch), -sinf(camPitch));
}
static void setupLighting() {
    glEnable(GL_NORMALIZE); glEnable(GL_COLOR_MATERIAL); glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
    glFogi(GL_FOG_MODE, GL_EXP2);
    applyTheme();
}

// =====================================================================
//  „ј—“№ 9. ќтрисовка сцены
// =====================================================================
static void setPerspective(float fovY, float aspect, float zn, float zf) {
    float fH = tanf(fovY / 360.0f * PI) * zn, fW = fH * aspect; glFrustum(-fW, fW, -fH, fH, zn, zf);
}
static void drawStatic() {
    int ti = (int)(px / TILE), tj = (int)(pz / TILE); const int R = 12;
    glEnableClientState(GL_VERTEX_ARRAY); glEnableClientState(GL_NORMAL_ARRAY); glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    for (int pass = 0; pass < 2; pass++) {
        Mesh& M = pass ? mWl : mFl; vector<int>& S = pass ? wlS : flS; vector<int>& Cn = pass ? wlC : flC;
        if (M.P.empty()) continue;
        glVertexPointer(3, GL_FLOAT, 0, M.P.data()); glNormalPointer(GL_FLOAT, 0, M.N.data()); glTexCoordPointer(2, GL_FLOAT, 0, M.T.data());
        const float* c = pass ? TH->wall : TH->flr; glColor3f(c[0], c[1], c[2]);
        if (pass) { texTri(tWall, tRock, .25f, 2.8f); setSpec(.12f, 20.f); }     // кладка до ~2.8 м, выше Ч скала
        else { texTri(tFloor, tFloor, .5f, 99.f); setSpec(.45f, 45.f); }         // мокрый блест€щий пол
        for (int j = max(0, tj - R); j <= min(G - 1, tj + R); j++) for (int i = max(0, ti - R); i <= min(G - 1, ti + R); i++) {
            int t = j * G + i; if (Cn[t] > 0) glDrawArrays(GL_QUADS, S[t], Cn[t]);
        }
    }
    glDisableClientState(GL_VERTEX_ARRAY); glDisableClientState(GL_NORMAL_ARRAY); glDisableClientState(GL_TEXTURE_COORD_ARRAY);
}
static void billboard(V3 c, float w, float h, V3 r, V3 u) {
    V3 a = r * (w / 2), b = u * (h / 2);
    glBegin(GL_QUADS);
    glTexCoord2f(0, 0); glVertex3f(c.x - a.x - b.x, c.y - a.y - b.y, c.z - a.z - b.z);
    glTexCoord2f(1, 0); glVertex3f(c.x + a.x - b.x, c.y + a.y - b.y, c.z + a.z - b.z);
    glTexCoord2f(1, 1); glVertex3f(c.x + a.x + b.x, c.y + a.y + b.y, c.z + a.z + b.z);
    glTexCoord2f(0, 1); glVertex3f(c.x - a.x + b.x, c.y - a.y + b.y, c.z - a.z + b.z);
    glEnd();
}
static float ghostBob(const Mon& m, float t) { return .08f * sinf(t * 2.f + m.ph); }
static void drawProps(float t) {
    const float VD = 34;   // дальность прорисовки предметов
    for (auto& T : torches) {
        if (hypotf(T.wx - px, T.wz - pz) > VD) continue;
        glPushMatrix(); glTranslatef(T.wx, T.wy, T.wz);
        if (T.kind == 1) { drawBrazier(); glPopMatrix(); continue; }
        glRotatef(T.ang * 180 / PI, 0, 1, 0);
        texOn(tWall); glColor3f(TH->wall[0] * .7f, TH->wall[1] * .7f, TH->wall[2] * .7f); setSpec(.05f, 8);
        glPushMatrix(); glTranslatef(0, 1.3f, -.25f); drawBox(.5f, .8f, .5f); glPopMatrix();      // каменна€ площадка под кронштейн
        texOff(); glCallList(T.nf == 2 ? lTorch2 : lTorch1); glPopMatrix();
    }
    // колонны в залах: база, ствол до свода, капитель
    for (auto& c : cols) {
        if (hypotf(c.x - px, c.z - pz) > VD) continue;
        float H = ceilAt(c.x, c.z) - c.y + .4f, cap = min(H - .5f, 3.4f);
        glPushMatrix(); glTranslatef(c.x, c.y, c.z);
        texOn(tWall); glColor3f(TH->wall[0] * .92f, TH->wall[1] * .92f, TH->wall[2] * .92f); setSpec(.12f, 18);
        glPushMatrix(); glTranslatef(0, .25f, 0); drawBox(1.15f, .5f, 1.15f); glPopMatrix();
        glPushMatrix(); glTranslatef(0, H * .5f, 0); drawBox(.82f, H, .82f); glPopMatrix();
        glPushMatrix(); glTranslatef(0, cap, 0); drawBox(1.05f, .3f, 1.05f); glPopMatrix();
        glPopMatrix();
    }
    // украшени€ свода: сталактиты, цепи, фонари
    for (auto& d : decos) {
        if (hypotf(d.x - px, d.z - pz) > VD) continue;
        glPushMatrix(); glTranslatef(d.x, d.y, d.z);
        if (d.type == 2) {
            texOff(); setSpec(.3f, 25); glColor3f(TH->wall[0] * .5f, TH->wall[1] * .5f, TH->wall[2] * .55f);
            glScalef(d.rad, d.len, d.rad); glCallList(lCone);
        }
        else if (d.type == 0) {
            glRotatef(2.5f * sinf(t * .7f + d.ph), 1, 0, 0); glRotatef(2.5f * sinf(t * .5f + d.ph * 1.7f), 0, 0, 1);
            drawChain(d.len, d.rad > 0 ? d.rad : 1.f);
        }
        else { drawChain(d.len, 1.f); glTranslatef(0, -d.len, 0); drawLantern(); }
        glPopMatrix();
    }
    // лестницы (сплошной каменный массив до самого низа)
    for (auto& S : stairs) {
        float cx = (S.tx + .5f) * TILE, cz = (S.tz + .5f) * TILE; if (hypotf(cx - px, cz - pz) > VD) continue;
        glPushMatrix(); glTranslatef(cx, 0, cz); glRotatef(atan2f((float)-S.dz, (float)S.dx) * 180 / PI, 0, 1, 0);
        texOn(tWall); glColor3f(TH->wall[0] * .9f, TH->wall[1] * .9f, TH->wall[2] * .9f); setSpec(.15f, 20);
        float rise = (S.h1 - S.h0) / STEPS;
        for (int k = 0; k < STEPS; k++) {
            float top = S.h0 + (k + 1) * rise, bot = S.h0 - .45f;
            glPushMatrix(); glTranslatef(((k + .5f) / STEPS - .5f) * TILE, (top + bot) * .5f, 0); drawBox(TILE / STEPS + .03f, top - bot, 3.2f); glPopMatrix();
        }
        glPopMatrix();
    }
    // мосты через провалы
    for (int tt = 0; tt < G * G; tt++) if (bridgeAt[tt]) {
        int ci = tt % G, cj = tt / G; float cx = (ci + .5f) * TILE, cz = (cj + .5f) * TILE; if (hypotf(cx - px, cz - pz) > VD) continue;
        glPushMatrix(); glTranslatef(cx, cellLv[tt] * LV, cz); if (ci % 2 != 0) glRotatef(90, 0, 1, 0);
        texOn(tWall); glColor3f(TH->wall[0] * .95f, TH->wall[1] * .95f, TH->wall[2] * .95f); setSpec(.12f, 16);
        glPushMatrix(); glTranslatef(0, -.2f, 0); drawBox(TILE + .1f, .4f, 1.8f); glPopMatrix();                        // настил
        for (int sg = -1; sg <= 1; sg += 2) { glPushMatrix(); glTranslatef(0, .25f, sg * .82f); drawBox(TILE + .1f, .5f, .22f); glPopMatrix(); }   // парапеты
        glPushMatrix(); glTranslatef(0, -2.4f, 0); drawBox(.7f, 4.0f, 1.2f); glPopMatrix();                              // опора
        glPopMatrix();
    }
    for (auto& it : items) {
        if (hypotf(it.x - px, it.z - pz) > VD) continue;
        if (it.type < 2) {
            glPushMatrix(); glTranslatef(it.x, it.y, it.z); drawPedestal(); glPopMatrix();
            if (!it.taken) { texOff(); glPushMatrix(); glTranslatef(it.x, it.y + 1.2f + .05f * sinf(t * 2), it.z); glRotatef(t * 60, 0, 1, 0); glCallList(it.type == 0 ? lKeyIron : lKeyBrass); glPopMatrix(); }
        }
        else {
            glPushMatrix(); glTranslatef(it.x, it.y, it.z); glRotatef(atan2f((float)-it.dx, (float)-it.dz) * 180 / PI, 0, 1, 0);
            texOn(tWall); glColor3f(TH->wall[0] * .7f, TH->wall[1] * .7f, TH->wall[2] * .7f); setSpec(.05f, 8);
            glPushMatrix(); glTranslatef(0, 1.2f, -.25f); drawBox(.5f, .7f, .5f); glPopMatrix();
            drawLever(it.pull); glPopMatrix();
        }
    }
    for (auto& D : doors) if (hypotf((D.tx + .5f) * TILE - px, (D.tz + .5f) * TILE - pz) < VD) drawDoor(D);
    if (hypotf(portX - px, portZ - pz) < 40) { glPushMatrix(); glTranslatef(portX, portY, portZ); glRotatef(portAng * 180 / PI, 0, 1, 0); drawPortal(t); glPopMatrix(); }
    for (auto& m : mons) {
        if (hypotf(m.x - px, m.z - pz) > VD) continue;
        glPushMatrix(); glTranslatef(m.x, m.y + ghostBob(m, t), m.z); glRotatef(m.yaw * 180 / PI, 0, 1, 0); drawGhost(t, m.ph); glPopMatrix();
    }
}
static void drawGlows(float t) {   // плам€, ореолы, пыль Ч аддитивное смешивание
    V3 r = { cosf(camYaw), 0, sinf(camYaw) }, u = { -sinf(camYaw) * sinf(camPitch), cosf(camPitch), cosf(camYaw) * sinf(camPitch) };
    glDisable(GL_LIGHTING); glDisable(GL_FOG); glEnable(GL_TEXTURE_2D); glEnable(GL_BLEND); glBlendFunc(GL_SRC_ALPHA, GL_ONE); glDepthMask(GL_FALSE);
    glBindTexture(GL_TEXTURE_2D, tGlow);
    const float GD = 38;
    for (auto& T : torches) {
        if (hypotf(T.wx - px, T.wz - pz) > GD) continue;
        float fl = 1 + .12f * sinf(t * 11 + T.ph) + .08f * sinf(t * 23 + T.ph * 2.3f), fs = T.kind == 1 ? 1.5f : 1.f;
        for (int i = 0; i < T.nf; i++) {
            glColor4f(cl01(TH->torch[0] * .5f), cl01(TH->torch[1] * .5f), cl01(TH->torch[2] * .5f), .38f * fl);
            billboard({ T.fx[i], T.fy + .22f * fs, T.fz[i] }, 1.5f * fl * fs, 1.5f * fl * fs, r, u);
        }
    }
    for (auto& d : decos) if (d.type == 1 && hypotf(d.x - px, d.z - pz) < GD) {   // ореол подвесных фонарей
        float p = .85f + .1f * sinf(t * 5 + d.ph);
        glColor4f(TH->lamp[0], TH->lamp[1], TH->lamp[2], .55f * p);
        billboard({ d.x, d.y - d.len - .18f, d.z }, 1.8f, 1.8f, r, u);
    }
    for (auto& it : items) if (it.type < 2 && !it.taken) {
        float p = .6f + .25f * sinf(t * 3);
        if (it.type == 0) glColor4f(.6f, .75f, 1, p * .5f); else glColor4f(1, .75f, .25f, p * .55f);
        billboard({ it.x, it.y + 1.2f + .05f * sinf(t * 2), it.z }, .9f, .9f, r, u);
    }
    for (auto& m : mons) {   // свет€щиес€ глаза призрака
        float p = .7f + .3f * sinf(t * 6 + m.ph);
        glColor4f(1.f, .3f, .08f, .6f * p);
        billboard({ m.x + sinf(m.yaw) * .2f, m.y + ghostBob(m, t) + 1.63f, m.z + cosf(m.yaw) * .2f }, .55f, .35f, r, u);
    }
    {   // си€ние голубой арки-выхода
        float p = .75f + .25f * sinf(t * 2.3f), sx = sinf(portAng), sz = cosf(portAng);
        glColor4f(.35f, .75f, 1.f, .5f * p); billboard({ portX + sx * .4f, portY + 1.1f, portZ + sz * .4f }, 4.2f, 4.2f, r, u);
        glColor4f(.7f, .95f, 1.f, .45f * p); billboard({ portX + sx * .15f, portY + 1.1f, portZ + sz * .15f }, 1.8f, 2.2f, r, u);
    }
    glBindTexture(GL_TEXTURE_2D, tFlame); V3 up = { 0,1,0 };
    for (auto& T : torches) {
        if (hypotf(T.wx - px, T.wz - pz) > GD) continue;
        float fs = T.kind == 1 ? 1.7f : 1.f;
        for (int i = 0; i < T.nf; i++) for (int l = 0; l < 3; l++) {
            float ph = T.ph + i * 1.3f + l * 1.7f;
            float hh = fs * .46f * (1 + .16f * sinf(t * 14 + ph) + .10f * sinf(t * 31 + ph * 2)) * (l == 0 ? 1.f : (l == 1 ? .75f : .5f));
            float ww = fs * .26f * (1 + .1f * sinf(t * 19 + ph * 1.3f)), sway = .03f * fs * sinf(t * 7 + ph);
            glColor4f(1, 1, 1, l == 0 ? .95f : .7f);
            billboard({ T.fx[i] + r.x * sway, T.fy + hh / 2, T.fz[i] + r.z * sway }, ww, hh, r, up);
        }
    }
    // пылинки в воздухе
    glDisable(GL_TEXTURE_2D); glPointSize(2.f); glColor4f(1, .8f, .55f, .35f); glBegin(GL_POINTS);
    for (int i = 0; i < 160; i++) {
        float bx = hrand(i, 1, 77) * 14 - 7, by = hrand(i, 2, 77) * 3.2f, bz = hrand(i, 3, 77) * 14 - 7;
        glVertex3f(px + bx + sinf(t * .3f + i) * .3f, pg + by + sinf(t * .4f + i * 2.f) * .15f, pz + bz + cosf(t * .27f + i) * .3f);
    }
    glEnd();
    glDepthMask(GL_TRUE); glDisable(GL_BLEND); glEnable(GL_FOG); glEnable(GL_LIGHTING);
}
static void render3D(int w, int h, float t) {
    glViewport(0, 0, w, h); glClearColor(TH->fog[0], TH->fog[1], TH->fog[2], 1); glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glMatrixMode(GL_PROJECTION); glLoadIdentity(); setPerspective(68.f, (float)w / h, .08f, 120.f);
    glMatrixMode(GL_MODELVIEW); glLoadIdentity();
    glRotatef(-camPitch * 180 / PI, 1, 0, 0); glRotatef(camYaw * 180 / PI, 0, 1, 0); glTranslatef(-px, -eyeY, -pz);
    glEnable(GL_DEPTH_TEST); glEnable(GL_LIGHTING); glEnable(GL_FOG); glEnable(GL_TEXTURE_2D); glDisable(GL_BLEND);
    setupLights(t);          // включает шейдер
    drawStatic();
    drawProps(t);
    useLit(false);           // дальше Ч свет€щиес€ спрайты без освещени€
    drawGlows(t);
}

// =====================================================================
//  „ј—“№ 10. »нтерфейс: мини-карта, выносливость, инвентарь, подсказки, экран поражени€
// =====================================================================
static void begin2D(int w, int h) {
    glMatrixMode(GL_PROJECTION); glPushMatrix(); glLoadIdentity(); glOrtho(0, w, h, 0, -1, 1);
    glMatrixMode(GL_MODELVIEW); glPushMatrix(); glLoadIdentity();
    glDisable(GL_LIGHTING); glDisable(GL_FOG); glDisable(GL_DEPTH_TEST); glDisable(GL_TEXTURE_2D);
    glEnable(GL_BLEND); glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
}
static void end2D() { glMatrixMode(GL_PROJECTION); glPopMatrix(); glMatrixMode(GL_MODELVIEW); glPopMatrix(); glDisable(GL_BLEND); }
static void rect2(float x, float y, float w, float h, float r, float g, float b, float a = 1) { glColor4f(r, g, b, a); glBegin(GL_QUADS); glVertex2f(x, y); glVertex2f(x + w, y); glVertex2f(x + w, y + h); glVertex2f(x, y + h); glEnd(); }
static void drawMinimap(int w, int h) {
    float S = min(w, h) * .30f, X = w - S - 16, Y = 16, pad = S * .07f, cs = (S - 2 * pad) / G, ox = X + pad, oy = Y + pad;
    glEnable(GL_TEXTURE_2D); glBindTexture(GL_TEXTURE_2D, tParch); glColor4f(1, 1, 1, .95f);
    glBegin(GL_QUADS); glTexCoord2f(0, 0); glVertex2f(X, Y); glTexCoord2f(1, 0); glVertex2f(X + S, Y); glTexCoord2f(1, 1); glVertex2f(X + S, Y + S); glTexCoord2f(0, 1); glVertex2f(X, Y + S); glEnd();
    glDisable(GL_TEXTURE_2D);
    for (int j = 0; j < G; j++) for (int i = 0; i < G; i++) {
        if (!(fullMap || seenT[j * G + i])) continue;
        if (wallG[j * G + i]) rect2(ox + i * cs, oy + j * cs, cs + .6f, cs + .6f, .25f, .22f, .19f, .85f);
        else {
            int t = j * G + i; float lv = cellLv[t] / 3.f;     // чем выше этаж, тем светлее клетка
            rect2(ox + i * cs, oy + j * cs, cs + .6f, cs + .6f, .55f + .3f * lv, .45f + .3f * lv, .28f + .3f * lv, .6f);
            if (stairAt[t] >= 0) rect2(ox + i * cs + cs * .2f, oy + j * cs + cs * .2f, cs * .6f, cs * .6f, .95f, .9f, .75f, .9f);
            if (bridgeAt[t]) rect2(ox + i * cs + cs * .2f, oy + j * cs + cs * .2f, cs * .6f, cs * .6f, .2f, .3f, .6f, .9f);
        }
    }
    static const float dc[5][3] = { {.55f,.3f,.1f},{.3f,.5f,.85f},{.65f,.3f,.75f},{.4f,.4f,.4f},{.8f,.55f,.15f} };
    for (auto& D : doors) { if (!(fullMap || seenT[D.tz * G + D.tx]) || D.open > .5f) continue; rect2(ox + D.tx * cs, oy + D.tz * cs, cs + .6f, cs + .6f, dc[D.type][0], dc[D.type][1], dc[D.type][2], 1); }
    for (auto& it : items) {
        if (it.taken || !(fullMap || seenT[it.tz * G + it.tx])) continue;
        float cx = ox + (it.tx + .5f) * cs, cy = oy + (it.tz + .5f) * cs, s = max(2.f, cs * .35f);
        if (it.type == 0) rect2(cx - s, cy - s, 2 * s, 2 * s, .6f, .75f, 1); else if (it.type == 1) rect2(cx - s, cy - s, 2 * s, 2 * s, 1, .8f, .25f); else rect2(cx - s, cy - s, 2 * s, 2 * s, .75f, .3f, .85f);
    }
    for (auto& m : mons) if (m.sees) { float s = max(2.5f, cs * .45f); rect2(ox + m.x / TILE * cs - s, oy + m.z / TILE * cs - s, 2 * s, 2 * s, .85f, .05f, .05f); }
    if (fullMap || seenT[exitT]) rect2(ox + (exitT % G + .15f) * cs, oy + (exitT / G + .15f) * cs, cs * .7f, cs * .7f, .3f, .95f, .5f);
    float cx = ox + px / TILE * cs, cy = oy + pz / TILE * cs, r = max(4.f, cs * .8f), fx = sinf(camYaw), fz = -cosf(camYaw), rx = cosf(camYaw), rz = sinf(camYaw);
    glColor4f(.9f, .15f, .1f, 1); glBegin(GL_TRIANGLES);
    glVertex2f(cx + fx * r, cy + fz * r); glVertex2f(cx - fx * r * .7f + rx * r * .7f, cy - fz * r * .7f + rz * r * .7f); glVertex2f(cx - fx * r * .7f - rx * r * .7f, cy - fz * r * .7f - rz * r * .7f); glEnd();
    glColor4f(.2f, .12f, .05f, 1); glBegin(GL_LINE_LOOP); glVertex2f(X, Y); glVertex2f(X + S, Y); glVertex2f(X + S, Y + S); glVertex2f(X, Y + S); glEnd();
}
static void appendNum(char16_t* b, int& k, int n) { char16_t tmp[12]; int c = 0; do { tmp[c++] = (char16_t)(u'0' + n % 10); n /= 10; } while (n); while (c) b[k++] = tmp[--c]; }
static void vignette(int w, int h, float r, float g, float b, float a) {
    float e = min(w, h) * .28f;
    glBegin(GL_QUADS);
    glColor4f(r, g, b, a); glVertex2f(0, 0); glColor4f(r, g, b, 0); glVertex2f(e, 0); glVertex2f(e, (float)h); glColor4f(r, g, b, a); glVertex2f(0, (float)h);
    glColor4f(r, g, b, 0); glVertex2f(w - e, 0); glColor4f(r, g, b, a); glVertex2f((float)w, 0); glVertex2f((float)w, (float)h); glColor4f(r, g, b, 0); glVertex2f(w - e, (float)h);
    glColor4f(r, g, b, a); glVertex2f(0, 0); glVertex2f((float)w, 0); glColor4f(r, g, b, 0); glVertex2f((float)w, e); glVertex2f(0, e);
    glColor4f(r, g, b, 0); glVertex2f(0, h - e); glVertex2f((float)w, h - e); glColor4f(r, g, b, a); glVertex2f((float)w, (float)h); glVertex2f(0, (float)h);
    glEnd();
}

// ---- что игрок сейчас Ђвидитї дл€ взаимодействи€ ----
struct Target { int kind = -1, idx = -1; float dist = 99; };
static Target findTarget() {
    Target best; float cp = cosf(camPitch);
    V3 f = { sinf(camYaw) * cp, sinf(camPitch), -cosf(camYaw) * cp }, e = { px, eyeY, pz };
    auto test = [&](V3 c, int kind, int idx) {
        V3 v = c - e; float d = sqrtf(v.x * v.x + v.y * v.y + v.z * v.z); if (d > 2.6f || d < .01f) return;
        if ((v.x * f.x + v.y * f.y + v.z * f.z) / d < .86f || d >= best.dist) return;
        best.kind = kind; best.idx = idx; best.dist = d;
        };
    for (size_t i = 0; i < items.size(); i++) { const Item& it = items[i]; if (it.taken) continue; test({ it.x, it.y + 1.2f, it.z }, 0, (int)i); }
    for (size_t i = 0; i < doors.size(); i++) { const Door& D = doors[i]; if (D.open > 0 || D.opening) continue; test({ (D.tx + .5f) * TILE, D.y + 1.1f, (D.tz + .5f) * TILE }, 1, (int)i); }
    return best;
}
static void interact(const Target& T) {
    if (T.kind == 0) {
        Item& it = items[T.idx]; it.taken = true;
        if (it.type == 0) { haveKey[0] = true; say(u"¬ы вз€ли железный ключ"); }
        else if (it.type == 1) { haveKey[1] = true; say(u"¬ы вз€ли латунный ключ"); }
        else { doors[it.door].opening = true; say(u"–ычаг щЄлкнул. √де-то открылась т€жЄла€ дверь..."); }
    }
    else if (T.kind == 1) {
        Door& D = doors[T.idx];
        switch (D.type) {
        case 0: if (haveKey[0]) { haveKey[0] = false; D.opening = true; say(u"«амок щЄлкнул. ƒверь открываетс€"); }
              else say(u"«аперто. Ќужен железный ключ"); break;
        case 1: if (haveKey[1]) { haveKey[1] = false; D.opening = true; say(u"÷епи упали. ƒверь открываетс€"); }
              else say(u"ƒверь в цеп€х. Ќужен особый латунный ключ"); break;
        case 2: say(u"ƒверь не поддаЄтс€. ћеханизм где-то на стене"); break;
        case 3: say(u"ƒверь заколочена досками. ≈Є не открыть"); break;
        default: say(u"«амок не поддаЄтс€. Ёта дверь не откроетс€"); break;
        }
    }
}
static void drawHUD(int w, int h) {
    begin2D(w, h);
    vignette(w, h, 0, 0, 0, .75f);
    float fear = 0;   // красна€ рамка, когда призрак гонитс€ р€дом
    for (auto& m : mons) if (m.chaseT > 0) fear = max(fear, cl01(1.f - hypotf(m.x - px, m.z - pz) / 14.f));
    if (fear > 0 && !dead) vignette(w, h, .6f, 0, 0, fear * (.45f + .15f * sinf(gTime * 9.f)));
    if (dead) {
        rect2(0, 0, (float)w, (float)h, .22f, 0, 0, min(.7f, deadT * .5f));
        const char16_t* a = u"¬џ ѕќ√»ЅЋ»", * b = u"ѕ–»«–ј  Ќј—“»√ ¬ј— ¬ “≈ћЌќ“≈", * c = u"Ќј∆ћ»“≈ ѕ–ќЅ≈Ћ, „“ќЅџ Ќј„ј“№ «јЌќ¬ќ";
        float al = min(1.f, deadT);
        drawText((w - textW(a, 7)) / 2, h * .36f, 7, a, .95f, .15f, .1f, al);
        drawText((w - textW(b, 3)) / 2, h * .52f, 3, b, .9f, .8f, .7f, al);
        if (deadT > 1.2f) drawText((w - textW(c, 2)) / 2, h * .62f, 2, c, 1.f, .95f, .8f, .6f + .4f * sinf(gTime * 4.f));
        end2D(); return;
    }
    drawMinimap(w, h);
    drawText(18, 18, 2, u"¬џЌќ—Ћ»¬ќ—“№", .85f, .8f, .7f);
    rect2(18, 40, 184, 14, .05f, .05f, .05f, .8f); rect2(20, 42, 180 * stamina / 100.f, 10, .75f, .25f, .15f);
    drawText(18, 66, 2, u"»Ќ¬≈Ќ“ј–№:", .85f, .8f, .7f);
    float yy = 86; if (haveKey[0]) { drawText(18, yy, 2, u"∆≈Ћ≈«Ќџ…  Ћё„", .65f, .78f, 1.f); yy += 20; }
    if (haveKey[1]) { drawText(18, yy, 2, u"Ћј“”ЌЌџ…  Ћё„", 1.f, .78f, .3f); yy += 20; }
    if (!haveKey[0] && !haveKey[1]) drawText(18, yy, 2, u"ѕ”—“ќ", .5f, .5f, .5f);
    char16_t b[48]; int k = 0; for (const char16_t* p = u"”–ќ¬≈Ќ№ "; *p; ) b[k++] = *p++; appendNum(b, k, level + 1); b[k] = 0;
    drawText((w - textW(b, 3)) / 2, 14, 3, b, .9f, .8f, .55f);
    const char16_t* goal = u"Ќј…ƒ»“≈  Ћё„» » ¬џ’ќƒ. »«Ѕ≈√ј…“≈ ѕ–»«–ј ќ¬";
    drawText((w - textW(goal, 2)) / 2, 44, 2, goal, .75f, .7f, .6f, .8f);
    drawText((w - textW(TH->name, 2)) / 2, 64, 2, TH->name, .2f + .8f * TH->lamp[0], .2f + .8f * TH->lamp[1], .2f + .8f * TH->lamp[2], .9f);
    rect2(w / 2.f - 1, h / 2.f - 8, 2, 5, 1, 1, 1, .6f); rect2(w / 2.f - 1, h / 2.f + 3, 2, 5, 1, 1, 1, .6f);
    rect2(w / 2.f - 8, h / 2.f - 1, 5, 2, 1, 1, 1, .6f); rect2(w / 2.f + 3, h / 2.f - 1, 5, 2, 1, 1, 1, .6f);
    Target T = findTarget();
    if (T.kind == 0) { const Item& it = items[T.idx]; const char16_t* s = it.type == 2 ? u"[E] ѕќ“яЌ”“№ –џ„ј√" : u"[E] ¬«я“№  Ћё„"; drawText((w - textW(s, 3)) / 2, h * .62f, 3, s, 1.f, .95f, .7f); }
    else if (T.kind == 1) { const char16_t* s = doors[T.idx].type >= 3 ? u"[E] ќ—ћќ“–≈“№ ƒ¬≈–№" : u"[E] ќ“ –џ“№ ƒ¬≈–№"; drawText((w - textW(s, 3)) / 2, h * .62f, 3, s, 1.f, .95f, .7f); }
    if (msgT > 0) drawText((w - textW(msg, 3)) / 2, h * .72f, 3, msg, 1, 1, 1, min(1.f, msgT));
    end2D();
}

// =====================================================================
//  „ј—“№ 11. »грова€ логика: новый уровень, призраки, движение по высоте, двери, выход, поражение
// =====================================================================
static void spawnMonsters() {
    mons.clear(); vector<int> d; bfsFrom(G + 1, d, nullptr, false);
    int maxd = 0; for (int t = 0; t < G * G; t++) if (isCell(t) && d[t] > maxd) maxd = d[t];
    vector<int> cand; for (int t = 0; t < G * G; t++) if (isCell(t) && t != exitT && d[t] >= max(12, maxd / 2)) cand.push_back(t);
    shuffle(cand.begin(), cand.end(), rng);
    int nm = min((int)cand.size(), min(3, 1 + level / 2));
    for (int i = 0; i < nm; i++) {
        int t = cand[i]; Mon m; m.x = (t % G + .5f) * TILE; m.z = (t / G + .5f) * TILE; m.y = groundAt(m.x, m.z) + .1f;
        m.yaw = 0; m.ph = rndf() * 6.f; m.chaseT = 0; m.vis = 0; m.tgt = m.prev = t; m.sees = false;
        mons.push_back(m);
    }
}
static bool walkable(int t) { return !wallG[t] && !(doorAt[t] >= 0 && doors[doorAt[t]].open < .7f); }
static void updateMonsters(float dt) {
    vector<int> pd; bfsFrom(cellIdx(px, pz), pd, nullptr, true);   // карта рассто€ний до игрока (закрытые двери призрак не проходит)
    for (auto& m : mons) {
        int mt = cellIdx(m.x, m.z);
        float d = hypotf(px - m.x, pz - m.z);
        m.sees = d < 20.f && hasLOS(m.x, m.z, px, pz);
        if (m.sees) { if (m.chaseT <= 0) say(u"ѕризрак заметил вас! Ѕегите!"); m.chaseT = 5.f; }
        else m.chaseT -= dt;
        bool chase = m.chaseT > 0 && pd[mt] >= 0;
        float tx = (m.tgt % G + .5f) * TILE, tz = (m.tgt / G + .5f) * TILE;
        if (hypotf(tx - m.x, tz - m.z) < .12f) {          // дошЄл до центра клетки Ч выбираем следующую
            int best = -1, nb[4], nn = 0;
            for (int k = 0; k < 4; k++) {
                int X = mt % G + DX[k], Z = mt / G + DZ[k]; if (X < 0 || Z < 0 || X >= G || Z >= G) continue;
                int u = Z * G + X; if (!walkable(u)) continue; nb[nn++] = u;
                if (chase && pd[u] >= 0 && pd[u] < pd[mt] && (best < 0 || pd[u] < pd[best])) best = u;
            }
            if (!chase) {                                  // бродит, стара€сь не поворачивать назад
                int c[4], nc = 0; for (int i = 0; i < nn; i++) if (nb[i] != m.prev) c[nc++] = nb[i];
                best = nc ? c[rndi(nc)] : (nn ? nb[0] : mt);
            }
            if (best < 0) best = mt;
            m.prev = mt; m.tgt = best; tx = (best % G + .5f) * TILE; tz = (best / G + .5f) * TILE;
        }
        if (chase && m.sees && d < TILE) { tx = px; tz = pz; m.tgt = mt; }   // рывок к игроку; потом вернЄтс€ к центру своей клетки
        float vx = tx - m.x, vz = tz - m.z, L = hypotf(vx, vz), sp = (chase ? 2.45f : 1.2f) * dt;
        if (L > 1e-4f) { float s = min(sp, L) / L; m.x += vx * s; m.z += vz * s; m.yaw = atan2f(vx, vz); }
        m.y += (groundAt(m.x, m.z) + .1f - m.y) * min(1.f, dt * 4.f);
        if (d < .8f && fabsf(m.y - pg) < 1.6f) { dead = true; deadT = 0; say(u""); return; }
    }
}
static void newLevel() {
    applyTheme();
    genLevel(); buildField(); buildMesh(); finalizeItems(); placeTorches(); placeRoomProps(); genDecor(); spawnMonsters();
    px = 1.5f * TILE; pz = 1.5f * TILE; camPitch = 0; haveKey[0] = haveKey[1] = false; dead = false; deadT = 0; stamina = 100;
    pg = groundAt(px, pz); eyeY = pg + 1.6f;
    for (int k = 0; k < 4; k++) if (!isWall(1 + DX[k], 1 + DZ[k])) { camYaw = atan2f((float)DX[k], (float)-DZ[k]); break; }
    say(u"Ќайдите ключ, откройте дверь, найдите выход");
}
static void update(GLFWwindow* win, float dt) {
    if (msgT > 0) msgT -= dt;
    if (dead) {                                            // поражение: камера оседает, ждЄм пробел
        deadT += dt; eyeY += (pg + .35f - eyeY) * min(1.f, dt * 3.f); camPitch += (.35f - camPitch) * min(1.f, dt * 2.f);
        if (wantRestart && deadT > .8f) newLevel();
        wantRestart = false; wantUse = false; return;
    }
    wantRestart = false;
    auto K = [&](int k) { return glfwGetKey(win, k) == GLFW_PRESS; };
    camYaw += ((K(GLFW_KEY_RIGHT) ? 1 : 0) - (K(GLFW_KEY_LEFT) ? 1 : 0)) * dt * 1.9f;
    float f = (float)((K(GLFW_KEY_W) || K(GLFW_KEY_UP) ? 1 : 0) - (K(GLFW_KEY_S) || K(GLFW_KEY_DOWN) ? 1 : 0)), s = (float)((K(GLFW_KEY_D) ? 1 : 0) - (K(GLFW_KEY_A) ? 1 : 0));
    float mx = sinf(camYaw) * f + cosf(camYaw) * s, mz = -cosf(camYaw) * f + sinf(camYaw) * s, len = sqrtf(mx * mx + mz * mz);
    bool moving = len > .01f, sprint = K(GLFW_KEY_LEFT_SHIFT) && moving && stamina > 5;
    if (sprint) stamina = max(0.f, stamina - 25 * dt); else stamina = min(100.f, stamina + 15 * dt);
    if (moving) {
        float sp = (sprint ? 4.4f : 2.6f) * dt, vx = mx / len * sp, vz = mz / len * sp;
        if (canStand(px + vx, pz, px, pz, pg)) px += vx;
        if (canStand(px, pz + vz, px, pz, pg)) pz += vz;
        bobPh += dt * (sprint ? 11.f : 7.5f);
    }
    pg = groundAt(px, pz);
    float target = pg + 1.6f + (moving ? .035f * sinf(bobPh) : 0.f);
    eyeY += (target - eyeY) * min(1.f, dt * 12.f);        // камера плавно поднимаетс€ по лестнице / опускаетс€ при прыжке с уступа
    for (auto& D : doors) if (D.opening && D.open < 1) D.open = min(1.f, D.open + dt / 1.3f);
    for (auto& it : items) if (it.type == 2 && it.taken && it.pull < 1) it.pull = min(1.f, it.pull + dt / .6f);
    int ti = (int)floorf(px / TILE), tj = (int)floorf(pz / TILE);
    for (int j = tj - 2; j <= tj + 2; j++) for (int i = ti - 2; i <= ti + 2; i++) if (i >= 0 && j >= 0 && i < G && j < G && hypotf((float)(i - ti), (float)(j - tj)) < 2.4f) seenT[j * G + i] = 1;
    if (ti >= 0 && tj >= 0 && ti < G && tj < G && roomId[tj * G + ti] >= 0) {   // в зале видно его целиком
        const Room& R = rooms[roomId[tj * G + ti]];
        for (int j = R.z0 - 1; j <= R.z1 + 1; j++) for (int i = R.x0 - 1; i <= R.x1 + 1; i++) if (i >= 0 && j >= 0 && i < G && j < G) seenT[j * G + i] = 1;
    }
    if (wantUse) { wantUse = false; Target T = findTarget(); if (T.kind >= 0) interact(T); }
    updateMonsters(dt);
    if (!dead && ti >= 0 && tj >= 0 && ti < G && tj < G && tj * G + ti == exitT) { level++; newLevel(); }
}

// =====================================================================
//  „ј—“№ 12. ќкно, обратные вызовы, главный цикл
// =====================================================================
static double lmx = 0, lmy = 0;
static void cursorCb(GLFWwindow*, double x, double y) {
    if (mouseLocked && !dead) { camYaw += (float)(x - lmx) * .0025f; camPitch = max(-1.2f, min(1.2f, camPitch - (float)(y - lmy) * .0025f)); }
    lmx = x; lmy = y;
}
static void mouseCb(GLFWwindow* w, int b, int a, int) { if (b == GLFW_MOUSE_BUTTON_LEFT && a == GLFW_PRESS) { glfwSetInputMode(w, GLFW_CURSOR, GLFW_CURSOR_DISABLED); mouseLocked = true; } }
static void keyCb(GLFWwindow* w, int key, int, int action, int) {
    if (action != GLFW_PRESS) return;
    if (key == GLFW_KEY_E) wantUse = true;
    if (key == GLFW_KEY_SPACE) wantRestart = true;
    if (key == GLFW_KEY_M) fullMap = !fullMap;
    if (key == GLFW_KEY_ESCAPE) { if (mouseLocked) { glfwSetInputMode(w, GLFW_CURSOR, GLFW_CURSOR_NORMAL); mouseLocked = false; } else glfwSetWindowShouldClose(w, true); }
}

int main() {
    glfwInit();
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 2); glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 1); glfwWindowHint(GLFW_SAMPLES, 4);
    GLFWwindow* window = glfwCreateWindow(1280, 720, "Dungeon", NULL, NULL);
    if (!window) { printf("Failed to create GLFW window\n"); glfwTerminate(); return -1; }
    glfwMakeContextCurrent(window); glfwSwapInterval(1);
    if (!gladLoadGLLoader((GLADloadproc)glfwGetProcAddress)) { printf("Failed to initialize GLAD\n"); return -1; }
    glfwSetCursorPosCallback(window, cursorCb); glfwSetMouseButtonCallback(window, mouseCb); glfwSetKeyCallback(window, keyCb);
    glEnable(GL_MULTISAMPLE);
    printf("√енераци€ текстур...\n");
    genTextures(); buildLists(); buildShader(); setupLighting();
    printf("√енераци€ пещеры...\n");
    newLevel();
    double last = glfwGetTime();
    while (!glfwWindowShouldClose(window)) {
        double now = glfwGetTime(); float dt = min((float)(now - last), .05f); last = now; gTime = (float)now;
        glfwPollEvents(); update(window, dt);
        int w, h; glfwGetFramebufferSize(window, &w, &h); if (!w || !h) continue;
        render3D(w, h, gTime);
        drawHUD(w, h);
        glfwSwapBuffers(window);
    }
    glfwTerminate();
    return 0;
}
